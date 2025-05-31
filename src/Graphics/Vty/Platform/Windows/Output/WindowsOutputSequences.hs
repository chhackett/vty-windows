{-# LANGUAGE OverloadedStrings #-}

-- | Provides functions that return escape sequences for various
--   modes and styles.
--   See: https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
module Graphics.Vty.Platform.Windows.Output.WindowsOutputSequences
  ( clearScreen,
    setStyleSeq,
    setColorSeq,
    clearToEndOfLine,
    clearToEndOfDisplay,
    showCursorSeq,
    hideCursorSeq,
    moveCursorHome,
    moveCursorPos,
    enterAlternateScreenBuffer,
    exitAlternateScreenBuffer,
    setDefaultAttr,
    setDefaultForegroundColor,
    setDefaultBackgroundColor,
    startHyperlink,
    endHyperlink,
    enableBracketedPastes,
    disableBracketedPastes,
    requestFocusEvents,
    disableFocusEvents,
    requestMouseEvents,
    disableMouseEvents,
    utf8inlineHack,
    RequiredStyleChanges (..),
    StyleChange (..),
    IntensityChange (..),
    ColorSide (..),
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU
import Data.Word (Word8)
import qualified Graphics.Vty.Attributes as VA

-- | Capture all the style changes required to apply
--   the requested Attr to the terminal. Note, Windows
--   terminals do not support standout mode, so that
--   style change is just ignored. Also, the Bold and
--   Dim styles are mutually exclusive, so they are
--   combined in the 'intensity' style change.
data RequiredStyleChanges = RequiredStyleChanges
  { italicChange :: StyleChange,
    strikethroughChange :: StyleChange,
    underlineChange :: StyleChange,
    reverseVideoChange :: StyleChange,
    blinkChange :: StyleChange,
    intensityChange :: IntensityChange
  }

data StyleChange = NoChange | Set | Unset
  deriving (Eq, Show)

data IntensityChange = NoIntensityChange | SetBold | SetDim | SetNormal
  deriving (Eq, Show)

instance Semigroup StyleChange where
  m0 <> NoChange = m0
  _ <> m1 = m1

instance Semigroup IntensityChange where
  r0 <> NoIntensityChange = r0
  _ <> r1 = r1

instance Semigroup RequiredStyleChanges where
  r0 <> r1 =
    RequiredStyleChanges
      (italicChange r0 <> italicChange r1)
      (strikethroughChange r0 <> strikethroughChange r1)
      (underlineChange r0 <> underlineChange r1)
      (reverseVideoChange r0 <> reverseVideoChange r1)
      (blinkChange r0 <> blinkChange r1)
      (intensityChange r0 <> intensityChange r1)

instance Monoid RequiredStyleChanges where
  mempty = RequiredStyleChanges NoChange NoChange NoChange NoChange NoChange NoIntensityChange
  mappend = (<>)

-- setting a color can either be for foreground or background
data ColorSide = Foreground | Background
  deriving (Eq)

-- | Given a set of style changes, generate the bytestring corresponding to the escape sequence needed
--   to set those styles
setStyleSeq :: RequiredStyleChanges -> BS.ByteString
setStyleSeq requiredStyleChanges =
     italicSeq
  <> strikethroughSeq
  <> underlineSeq
  <> reverseVideoSeq
  <> blinkSeq
  <> intensitySeq
  where
    italicSeq        = mkSeq italicChange        (setItalicStyle,        exitItalicStyle)
    strikethroughSeq = mkSeq strikethroughChange (setStrikethroughStyle, exitStrikethroughStyle)
    underlineSeq     = mkSeq underlineChange     (setUnderlineStyle,     exitUnderlineStyle)
    reverseVideoSeq  = mkSeq reverseVideoChange  (setReverseVideoMode,   exitReverseVideoMode)
    blinkSeq         = mkSeq blinkChange         (setBlinkMode,          exitBlinkMode)
    intensitySeq     =
      case intensityChange requiredStyleChanges of
        NoIntensityChange -> mempty
        SetNormal         -> resetIntensity
        SetBold           -> setBoldStyle
        SetDim            -> setDimStyle

    mkSeq :: (RequiredStyleChanges -> StyleChange) -> (BS.ByteString, BS.ByteString) -> BS.ByteString
    mkSeq maybeChange (setSeq, exitSeq) =
      case maybeChange requiredStyleChanges of
        NoChange -> mempty
        Set -> setSeq
        Unset -> exitSeq

-- | Given a color to set, the color mode, and whether the change is to the foreground or background,
--   generate the bytestring corresponding to the escape sequence needed to set that color
setColorSeq :: ColorSide -> VA.Color -> VA.ColorMode -> BS.ByteString
setColorSeq side (VA.ISOColor v) _ = setIsoColorSeq side $ toEnum (fromEnum v)
setColorSeq side (VA.Color240 v) colorMode =
  case colorMode of
    VA.NoColor        -> mempty
    VA.ColorMode8     -> color240ToIso8Color
    VA.ColorMode16    -> color240ToIso8Color
    VA.ColorMode240 _ -> color240Seq side v
    VA.FullColor      -> color240Seq side v
  where
    color240ToIso8Color :: BS.ByteString
    color240ToIso8Color
      | v < 8 = setIsoColorSeq side $ toEnum (fromEnum v)
      | v < 16 = setIsoColorSeq side $ toEnum (fromEnum $ v - 8)
      | v < 216 =
          let v' = v - 16
              r = v' `div` 36
              g = (v' - 36 * r) `div` 6
              b = v' - 36 * r - 6 * g in
          rgbColorToIso8Color side r g b
      | otherwise = undefined

setColorSeq side (VA.RGBColor r g b) colorMode =
  case colorMode of
    VA.NoColor        -> mempty
    VA.ColorMode8     -> rgbColorToIso8Color side r g b
    VA.ColorMode16    -> rgbColorToIso8Color side r g b
    VA.ColorMode240 _ -> color240Seq side $ VA.rgbColorToColor240 r g b
    VA.FullColor      -> rgbColorSeq
  where
    -- RGB sequence has the format: "\ESC[{side};2;{r};{g};{b}m"
    rgbColorSeq :: BS.ByteString
    rgbColorSeq =
        csi
      <> sideCode side
      <> ";2;"
      <> BS.singleton r
      <> ";"
      <> BS.singleton g
      <> ";"
      <> BS.singleton b
      <> "m"

color240Seq :: ColorSide -> Word8 -> BS.ByteString
color240Seq side v =
     csi
  <> sideCode side
  <> ";5;"
  <> BS.singleton v
  <> "m"

sideCode :: ColorSide -> BS.ByteString
sideCode side = case side of
  Foreground -> "38"
  Background -> "48"

data IsoColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Show, Enum)

-- | If the ColorMode is set to ISOColor
rgbColorToIso8Color :: ColorSide -> Word8 -> Word8 -> Word8 -> BS.ByteString
rgbColorToIso8Color side r g b =
  setIsoColorSeq side $ rgbColorToIso8Color' (isByteLow r) (isByteLow g) (isByteLow b)
  where
    isByteLow :: Word8 -> Bool
    isByteLow byte = byte < 128

    rgbColorToIso8Color' :: Bool -> Bool -> Bool -> IsoColor
    rgbColorToIso8Color' False False False = Black
    rgbColorToIso8Color' True False False  = Red
    rgbColorToIso8Color' False True False  = Green
    rgbColorToIso8Color' True True False   = Yellow
    rgbColorToIso8Color' False False True  = Blue
    rgbColorToIso8Color' True False True   = Magenta
    rgbColorToIso8Color' False True True   = Cyan
    rgbColorToIso8Color' True True True    = White

setIsoColorSeq :: ColorSide -> IsoColor -> BS.ByteString
setIsoColorSeq side isoColor =
  let offset = if side == Foreground then 30 else 40
  in csi <> BSU.fromString (show (offset + fromEnum isoColor)) <> "m"

-- A bunch of escape sequences for setting styles and attributes including colors and embedded URLs

esc :: BS.ByteString
esc = "\ESC"

csi :: BS.ByteString
csi = esc <> "["

osc :: BS.ByteString
osc = esc <> "]"

clearScreen, clearToEndOfLine, clearToEndOfDisplay :: BS.ByteString
clearScreen = moveCursorHome <> clearToEndOfDisplay
clearToEndOfLine    = csi <> "K"
clearToEndOfDisplay = csi <> "J"

showCursorSeq, hideCursorSeq, moveCursorHome :: BS.ByteString
showCursorSeq  = csi <> "?25h"
hideCursorSeq  = csi <> "?25l"
moveCursorHome = csi <> "H" -- moves cursor to 'home' position (1,1)

moveCursorPos :: Int -> Int -> BS.ByteString
moveCursorPos x y = csi <> BSU.fromString (show $ y + 1) <> ";" <> BSU.fromString (show $ x + 1) <> "H"

enterAlternateScreenBuffer, exitAlternateScreenBuffer :: BS.ByteString
enterAlternateScreenBuffer = csi <> "?1049h"
exitAlternateScreenBuffer  = csi <> "?1049l"

-- | This sequence resets all styling and color attributes to the default
setDefaultAttr :: BS.ByteString
setDefaultAttr = csi <> "0m"

setBoldStyle, setDimStyle, resetIntensity :: BS.ByteString
setBoldStyle   = csi <> "1m"
setDimStyle    = csi <> "2m"
resetIntensity = csi <> "22m" -- Resets the bold and dim styles to 'normal' intensity

setItalicStyle, exitItalicStyle :: BS.ByteString
setItalicStyle  = csi <> "3m"
exitItalicStyle = csi <> "23m"

setUnderlineStyle, exitUnderlineStyle :: BS.ByteString
setUnderlineStyle  = csi <> "4m"
exitUnderlineStyle = csi <> "24m"

setBlinkMode, exitBlinkMode :: BS.ByteString
setBlinkMode = csi <> "5m"
exitBlinkMode  = csi <> "25m"

setReverseVideoMode, exitReverseVideoMode :: BS.ByteString
setReverseVideoMode  = csi <> "7m"
exitReverseVideoMode = csi <> "27m"

setStrikethroughStyle, exitStrikethroughStyle :: BS.ByteString
setStrikethroughStyle  = csi <> "9m"
exitStrikethroughStyle = csi <> "29m"

setDefaultForegroundColor, setDefaultBackgroundColor :: BS.ByteString
setDefaultForegroundColor = csi <> "39m"
setDefaultBackgroundColor = csi <> "49m"

startHyperlink :: BS.ByteString -> BS.ByteString
startHyperlink url = osc <> "8;;" <> url <> "\BEL"

endHyperlink :: BS.ByteString
endHyperlink = osc <> "8;;\BEL"

-- | These sequences set xterm-based terminals to send focus event
-- sequences.
requestFocusEvents :: BS.ByteString
requestFocusEvents = BS8.pack "\ESC[?1004h"

-- | These sequences disable focus events.
disableFocusEvents :: BS.ByteString
disableFocusEvents = BS8.pack "\ESC[?1004l"

-- | These sequences set xterm-based terminals to send mouse event
-- sequences.
requestMouseEvents :: BS.ByteString
requestMouseEvents = BS8.pack "\ESC[?1000h\ESC[?1002h\ESC[?1006h"

-- | These sequences disable mouse events.
disableMouseEvents :: BS.ByteString
disableMouseEvents = BS8.pack "\ESC[?1000l\ESC[?1002l\ESC[?1006l"

-- | Enable bracketed paste mode:
-- http://cirw.in/blog/bracketed-paste
enableBracketedPastes :: BS.ByteString
enableBracketedPastes = BS8.pack "\ESC[?2004h"

-- | Disable bracketed paste mode:
disableBracketedPastes :: BS.ByteString
disableBracketedPastes = BS8.pack "\ESC[?2004l"

-- | These sequences set xterm based terminals to UTF-8 output.
--
-- There is no known terminfo capability equivalent to this.
utf8inlineHack :: BS.ByteString
utf8inlineHack = BS8.pack "\ESC[K"