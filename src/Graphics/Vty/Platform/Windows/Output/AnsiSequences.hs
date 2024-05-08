{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

-- | Provides functions that return escape sequences for various 
--   modes and styles.
--   See: https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
module Graphics.Vty.Platform.Windows.Output.AnsiSequences
    ( clearScreen
    , setStyleSeq
    , setColorSeq
    , clearToEndOfLine
    , clearToEndOfDisplay
    , showCursorSeq
    , hideCursorSeq
    , moveCursorHome
    , moveCursorPos
    , enterAlternateScreenBuffer
    , exitAlternateScreenBuffer
    , setDefaultAttr
    , setDefaultForegroundColor
    , setDefaultBackgroundColor
    , startHyperlink
    , endHyperlink
    , RequiredStyleChanges(..)
    , MaybeSetExit(..)
    , MaybeSetExitIntensity(..)
    , ColorSide(..))
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Word (Word8)
import qualified Graphics.Vty.Attributes as VA

-- | Capture all the style changes required to apply 
--   the requested Attr to the terminal. Note, Windows
--   does not support standout mode, so that style
--   is just ignored. Also, only either Bold or Dim styles
--   can be applied at the same time, so they are combined
--   in the 'intensity' style.
data RequiredStyleChanges = RequiredStyleChanges
    { italicChange :: MaybeSetExit
    , strikethroughChange :: MaybeSetExit
    , underlineChange :: MaybeSetExit
    , reverseVideoChange :: MaybeSetExit
    , blinkChange :: MaybeSetExit
    , intensityChange :: MaybeSetExitIntensity
    }

data MaybeSetExit = NoChange | Set | Exit
data MaybeSetExitIntensity = KeepCurrentIntensity | SetBold | SetDim | ExitIntensity

instance Semigroup MaybeSetExit where
    m0 <> NoChange = m0
    _  <> m1       = m1

instance Semigroup MaybeSetExitIntensity where
    r0 <> KeepCurrentIntensity = r0
    _  <> r1                   = r1

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
    mempty = RequiredStyleChanges NoChange NoChange NoChange NoChange NoChange KeepCurrentIntensity
    mappend = (<>)

-- setting a color can either be for foreground or background
data ColorSide = Foreground | Background
    deriving (Eq)

-- A bunch of escape sequences for setting styles and attributes including colors

esc :: BS.ByteString
esc = "\ESC"

csi :: BS.ByteString
csi = esc <> "["

osc :: BS.ByteString
osc = esc <> "]"

clearScreen, clearToEndOfLine, clearToEndOfDisplay :: BS.ByteString
clearScreen = moveCursorHome <> clearToEndOfDisplay
clearToEndOfLine = csi <> "K"
clearToEndOfDisplay = csi <> "J"

showCursorSeq, hideCursorSeq, moveCursorHome :: BS.ByteString
showCursorSeq = csi <> "?25h"
hideCursorSeq = csi <> "?25l"
moveCursorHome = csi <> "H"    -- moves cursor to 'home' position (1,1)

moveCursorPos :: Int -> Int -> BS.ByteString
moveCursorPos x y = csi <> BSU.fromString  (show y) <> ";" <> BSU.fromString (show x) <> "H"

enterAlternateScreenBuffer, exitAlternateScreenBuffer :: BS.ByteString
enterAlternateScreenBuffer = csi <> "?1049h"
exitAlternateScreenBuffer = csi <> "?1049l"

setDefaultAttr :: BS.ByteString
setDefaultAttr = csi <> "0m" 

setBoldStyle, setDimStyle, resetIntensity :: BS.ByteString
setBoldStyle = csi <> "1m"
setDimStyle = csi <> "2m"

resetIntensity = csi <> "22m"    -- Resets both bold AND dim styles

setItalicStyle, exitItalicStyle :: BS.ByteString
setItalicStyle = csi <> "3m"
exitItalicStyle = csi <> "23m"

setUnderlineStyle, exitUnderlineStyle :: BS.ByteString
setUnderlineStyle = csi <> "4m"
exitUnderlineStyle = csi <> "24m"

setReverseVideoMode, exitReverseVideoMode :: BS.ByteString
setReverseVideoMode = csi <> "7m"
exitReverseVideoMode = csi <> "27m"

setStrikethroughStyle, exitStrikethroughStyle :: BS.ByteString
setStrikethroughStyle = csi <> "9m"
exitStrikethroughStyle = csi <> "29m"

enterBlinkMode, exitBlinkMode :: BS.ByteString
enterBlinkMode = csi <> "?12h"
exitBlinkMode = csi <> "?12l"

setDefaultForegroundColor, setDefaultBackgroundColor :: BS.ByteString
setDefaultForegroundColor = csi <> "39m"
setDefaultBackgroundColor = csi <> "49m"

startHyperlink :: BS.ByteString -> BS.ByteString
startHyperlink url = osc <> "8;;" <> url <> "\BEL"

endHyperlink :: BS.ByteString
endHyperlink = osc <> "8;;\BEL"

setStyleSeq :: RequiredStyleChanges -> BS.ByteString
setStyleSeq requiredStyleChanges =
    let italicSeq        = mkSeq italicChange (setItalicStyle, exitItalicStyle)
        strikethroughSeq = mkSeq strikethroughChange (setStrikethroughStyle, exitStrikethroughStyle)
        underlineSeq     = mkSeq underlineChange (setUnderlineStyle, exitUnderlineStyle)
        reverseVideoSeq  = mkSeq reverseVideoChange (setReverseVideoMode, exitReverseVideoMode)
        blinkSeq         = mkSeq blinkChange (enterBlinkMode, exitBlinkMode)
        intensitySeq =
            case intensityChange requiredStyleChanges of
                KeepCurrentIntensity -> mempty
                ExitIntensity        -> resetIntensity
                SetBold              -> setBoldStyle 
                SetDim               -> setDimStyle
    in italicSeq <> strikethroughSeq <> underlineSeq <> reverseVideoSeq
                 <> blinkSeq <> intensitySeq
    where
        mkSeq :: (RequiredStyleChanges -> MaybeSetExit) -> (BS.ByteString, BS.ByteString)
                    -> BS.ByteString
        mkSeq maybeChange (setSeq, exitSeq) =
            case maybeChange requiredStyleChanges of
                NoChange -> mempty
                Set      -> setSeq
                Exit     -> exitSeq

setColorSeq :: ColorSide -> VA.Color -> VA.ColorMode -> BS.ByteString
setColorSeq side (VA.ISOColor v) _ = setIsoColorSeq side v
setColorSeq side (VA.Color240 v) colorMode = 
    case colorMode of
        VA.NoColor        -> mempty
        VA.ColorMode8     -> undefined
        VA.ColorMode16    -> undefined
        VA.ColorMode240 _ -> color240Seq side v
        VA.FullColor      -> color240Seq side v
setColorSeq side (VA.RGBColor r g b) colorMode =
    case colorMode of
        VA.NoColor        -> mempty
        VA.ColorMode8     -> undefined
        VA.ColorMode16    -> rgbColorToIso16Color r g b
        VA.ColorMode240 _ -> color240Seq side $ VA.rgbColorToColor240 r g b
        VA.FullColor      -> rgbColorSeq side r g b

color240Seq :: ColorSide -> Word8 -> BS.ByteString
color240Seq side v =
  csi <> sideCode side <> ";5;"
      <> BS.singleton v <> "m"

-- RGB sequence has the format: "\ESC[{side};2;{r};{g};{b}m"
rgbColorSeq :: ColorSide -> Word8 -> Word8 -> Word8 -> BS.ByteString
rgbColorSeq side r g b =
  csi <> sideCode side <> ";2;"
      <> BS.singleton r <> ";"
      <> BS.singleton g <> ";"
      <> BS.singleton b <> "m"

sideCode :: ColorSide -> BS.ByteString
sideCode side = case side of
    Foreground -> "38"
    Background -> "48"

-- | If the ColorMode is set to ISOColor
rgbColorToIso16Color :: Integral i => i -> i -> i -> BS.ByteString
rgbColorToIso16Color r g b =
    undefined

setIsoColorSeq :: ColorSide -> Word8 -> BS.ByteString
setIsoColorSeq side v
    | v >= 0 && v < 8 =
        let offset = if side == Foreground then 30 else 40 in
        csi <> BSU.fromString (show (offset + fromEnum v)) <> "m"
    | otherwise = error $ "Invalid ISOColor supplied [" ++ show v ++ "]. Should be in the range 0 to 7"
