{-# LANGUAGE OverloadedStrings #-}

-- | Provides functions for configuring the terminal for VT processing, and to
-- change the window size
module Graphics.Vty.Platform.Windows.Output.TerminfoBased
  ( reserveTerminal,
  )
where

import Blaze.ByteString.Builder (Write, writeByteString)
import Control.Monad (when)
import Data.Char (isPrint, showLitChar)
import Data.Foldable (foldl')
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BS8
import Graphics.Vty.Attributes
  ( Attr,
    ColorMode,
    FixedAttr (..),
    Style,
    blink,
    bold,
    dim,
    hasStyle,
    italic,
    reverseVideo,
    strikethrough,
    underline,
  )
import Graphics.Vty.DisplayAttributes
  ( DisplayAttrDiff (..),
    DisplayColorDiff (..),
    StyleStateChange (..),
    URLDiff (..),
  )
import Graphics.Vty.Image (DisplayRegion)
import Graphics.Vty.Output (DisplayContext (..), Mode (..), Output (..), initialAssumedState)
import Graphics.Vty.Platform.Windows.Output.WindowsOutputSequences
import Graphics.Vty.Platform.Windows.Output.WindowSize
    ( setWindowSize, getWindowSize )
import Graphics.Vty.Platform.Windows.WindowsInterfaces (configureOutput, writeBuf)
import System.IO (Handle)

-- | Constructs an output driver
reserveTerminal :: String -> Handle -> ColorMode -> IO Output
reserveTerminal termName outHandle colorMode = do
  restoreMode <- configureOutput outHandle

  mouseModeStatus <- newIORef False
  focusModeStatus <- newIORef False
  pasteModeStatus <- newIORef False
  hyperlinkModeStatus <- newIORef False
  newAssumedStateRef <- newIORef initialAssumedState

  let setModeStatus m newStatus = do
        curStatus <- getMmodeStatus m
        when (newStatus /= curStatus) $
          case m of
            Focus -> do
              writeBuf outHandle $ if newStatus then requestFocusEvents else disableFocusEvents
              writeIORef focusModeStatus newStatus
            Mouse -> do
              writeBuf outHandle $ if newStatus then requestMouseEvents else disableMouseEvents
              writeIORef mouseModeStatus newStatus
            BracketedPaste -> do
              writeBuf outHandle $ if newStatus then enableBracketedPastes else disableBracketedPastes
              writeIORef pasteModeStatus newStatus
            Hyperlink -> do
              writeIORef hyperlinkModeStatus newStatus
              writeIORef newAssumedStateRef initialAssumedState

      getMmodeStatus Mouse = readIORef mouseModeStatus
      getMmodeStatus Focus = readIORef focusModeStatus
      getMmodeStatus BracketedPaste = readIORef pasteModeStatus
      getMmodeStatus Hyperlink = readIORef hyperlinkModeStatus

  return
    Output
      { terminalID = termName,
        releaseTerminal = do
          setModeStatus BracketedPaste False
          setModeStatus Mouse False
          setModeStatus Focus False
          writeBuf outHandle setDefaultAttr
          writeBuf outHandle showCursorSeq
          restoreMode,
        supportsBell = pure False,
        supportsItalics = pure True,
        supportsStrikethrough = pure True,
        ringTerminalBell = pure (),
        reserveDisplay = do
          writeBuf outHandle enterAlternateScreenBuffer
          writeBuf outHandle clearScreen,
        releaseDisplay = do
          writeBuf outHandle exitAlternateScreenBuffer
          writeBuf outHandle showCursorSeq,
        setDisplayBounds = \(w, h) -> setWindowSize outHandle (w, h),
        displayBounds = do
          rawSize <- getWindowSize outHandle
          case rawSize of
            (w, h)
              | w < 0 || h < 0 -> fail $ "getwinsize returned < 0 : " ++ show rawSize
              | otherwise -> return (w, h),
        outputByteBuffer = writeBuf outHandle,
        supportsCursorVisibility = True,
        supportsMode = const True,
        setMode = setModeStatus,
        getModeStatus = getMmodeStatus,
        assumedStateRef = newAssumedStateRef,
        outputColorMode = colorMode,
        mkDisplayContext = terminfoDisplayContext,
        -- This function emits an Xterm-compatible escape sequence that we
        -- anticipate will work for essentially all modern terminal emulators.
        -- Ideally we'd use a terminal capability for this, but there does not
        -- seem to exist a termcap for setting window titles. If you find that
        -- this function does not work for a given terminal emulator, please
        -- report the issue.

        -- For details, see:

        -- https://tldp.org/HOWTO/Xterm-Title-3.html
        setOutputWindowTitle = \title -> do
          let sanitize :: String -> String
              sanitize = concatMap sanitizeChar
              sanitizeChar c
                | not (isPrint c) = showLitChar c ""
                | otherwise = [c]
          writeBuf outHandle $ BS8.pack $ "\ESC]2;" <> sanitize title <> "\007"
      }

terminfoDisplayContext :: Monad m => Output -> DisplayRegion -> m DisplayContext
terminfoDisplayContext tActual region =
    return DisplayContext
      { contextDevice = tActual,
        contextRegion = region,
        writeMoveCursor = \x y -> writeByteString $ moveCursorPos x y,
        writeShowCursor = writeByteString showCursorSeq,
        writeHideCursor = writeByteString hideCursorSeq,
        writeSetAttr = terminfoWriteSetAttr (outputColorMode tActual),
        writeDefaultAttr = \urlsEnabled -> writeByteString setDefaultAttr <> writeUrlChange urlsEnabled EndLink,
        writeRowEnd = writeByteString clearToEndOfLine,
        inlineHack = outputByteBuffer tActual utf8inlineHack
      }

-- | Represents the requested change for all attr types that would be reset by sending
-- the 'reset to default' escape sequence: <CSI>0m
data WindowsAttrChanges = WindowsAttrChanges
  { requiredStyleChanges :: RequiredStyleChanges
  , fgColorDiff :: DisplayColorDiff
  , bgColorDiff :: DisplayColorDiff
  }

-- | Generate the necessary escape sequences to change terminal attributes. There are two ways to do this:
-- 1: Send the 'reset all attributes to their default value' sequence (<CSI>0m), then send sequences
--    to set all non-default attributes.
-- 2: Send sequences for all changing attributes.
-- The goal is to send the shortest set of sequences that will result in the specified set of attribute changes.
terminfoWriteSetAttr :: ColorMode -> Bool -> FixedAttr -> Attr -> DisplayAttrDiff -> Write
terminfoWriteSetAttr colorMode urlsEnabled prevAttr attr diffs =
  let attrChanges = calcWindowsAttrChanges diffs in
  (if numNonDefaultAttrsReset attrChanges < numNonDefaultAttrsNoChange attrChanges
   then writeOnlyNonDefaults colorMode attrChanges prevAttr
   else writeAllChanges colorMode prevAttr attr diffs)
  <> writeUrlChange urlsEnabled (urlDiff diffs)

calcWindowsAttrChanges :: DisplayAttrDiff -> WindowsAttrChanges
calcWindowsAttrChanges diffs = 
  WindowsAttrChanges
    (RequiredStyleChanges
      calcItalicChange
      calcStrikeThroughChange
      calcUnderlineChange
      calcReverseVideoChange
      calcBlinkChange
      calcIntensityChange)
    (foreColorDiff diffs)
    (backColorDiff diffs)
  where
    calcItalicChange = calcGenericStyleChange RemoveItalic ApplyItalic
    calcStrikeThroughChange = calcGenericStyleChange RemoveStrikethrough ApplyStrikethrough
    calcUnderlineChange = calcGenericStyleChange RemoveUnderline ApplyUnderline
    calcReverseVideoChange = calcGenericStyleChange RemoveReverseVideo ApplyReverseVideo
    calcBlinkChange = calcGenericStyleChange RemoveBlink ApplyBlink

    calcGenericStyleChange :: StyleStateChange -> StyleStateChange -> StyleChange
    calcGenericStyleChange unsetChange setChange
      | elem unsetChange $ styleDiffs diffs = Unset
      | elem setChange $ styleDiffs diffs = Set
      | otherwise = NoChange

    calcIntensityChange :: IntensityChange
    calcIntensityChange
      | RemoveDim `elem` styleDiffs diffs || RemoveBold `elem` styleDiffs diffs = SetNormal
      | ApplyDim `elem` styleDiffs diffs = SetDim
      | ApplyBold `elem` styleDiffs diffs = SetBold
      | otherwise = NoIntensityChange

-- | Computes the number of attributes that are requested to be reset to default values
numNonDefaultAttrsReset :: WindowsAttrChanges -> Int
numNonDefaultAttrsReset attrChanges = numNonDefaultAttrsChangeCount attrChanges (Unset, SetNormal, ColorToDefault)

-- | Computes the number of attributes that are not being changed in this update
numNonDefaultAttrsNoChange :: WindowsAttrChanges -> Int
numNonDefaultAttrsNoChange attrChanges = numNonDefaultAttrsChangeCount attrChanges (NoChange, NoIntensityChange, NoColorChange)

-- | Computes the number of attribute changes of the specified types
numNonDefaultAttrsChangeCount :: WindowsAttrChanges -> (StyleChange, IntensityChange, DisplayColorDiff) -> Int
numNonDefaultAttrsChangeCount attrChanges (styleChange, intensityChangeType, colorChange) = 
  let allChanges = map styleChangeCheck styleChangeGetters ++  [intensityChangeCheck, foregroundColorChangeCheck, backgroundColorChangeCheck]
  in length $ filter id allChanges
  where
    styleChangeGetters = [italicChange, strikethroughChange, underlineChange, reverseVideoChange, blinkChange]

    styleChangeCheck :: (RequiredStyleChanges -> StyleChange) -> Bool
    styleChangeCheck getter = getter (requiredStyleChanges attrChanges) == styleChange

    intensityChangeCheck = intensityChange (requiredStyleChanges attrChanges) == intensityChangeType

    foregroundColorChangeCheck = fgColorDiff attrChanges == colorChange
    backgroundColorChangeCheck = bgColorDiff attrChanges == colorChange

-- | This function generates the Write value corresponding to the escape sequence for all changing attributes
writeAllChanges :: ColorMode -> FixedAttr -> Attr -> DisplayAttrDiff -> Write
writeAllChanges colorMode prevAttr _ diffs =
       writeStyleChanges (styleDiffs diffs)
    <> writeColorDiff (foreColorDiff diffs) Foreground
    <> writeColorDiff (backColorDiff diffs) Background
  where
    writeStyleChanges :: [StyleStateChange] -> Write
    writeStyleChanges styleStateChanges =
      let requiredChanges = foldl' applyStyleChange mempty styleStateChanges
      in writeByteString $ setStyleSeq requiredChanges

    applyStyleChange :: RequiredStyleChanges -> StyleStateChange -> RequiredStyleChanges
    applyStyleChange requiredChanges styleStateChange =
      case styleStateChange of
        ApplyStandout       -> mempty -- windows doesn't support standout mode
        RemoveStandout      -> mempty
        ApplyItalic         -> requiredChanges { italicChange        = calcStyleChange italic Set }
        RemoveItalic        -> requiredChanges { italicChange        = calcStyleChange italic Unset }
        ApplyStrikethrough  -> requiredChanges { strikethroughChange = calcStyleChange strikethrough Set }
        RemoveStrikethrough -> requiredChanges { strikethroughChange = calcStyleChange strikethrough Unset }
        ApplyUnderline      -> requiredChanges { underlineChange     = calcStyleChange underline Set }
        RemoveUnderline     -> requiredChanges { underlineChange     = calcStyleChange underline Unset }
        ApplyReverseVideo   -> requiredChanges { reverseVideoChange  = calcStyleChange reverseVideo Set }
        RemoveReverseVideo  -> requiredChanges { reverseVideoChange  = calcStyleChange reverseVideo Unset }
        ApplyBlink          -> requiredChanges { blinkChange         = calcStyleChange blink Set   }
        RemoveBlink         -> requiredChanges { blinkChange         = calcStyleChange blink Unset }
        ApplyDim            -> requiredChanges { intensityChange     = calcIntensityChange SetDim    }
        RemoveDim           -> requiredChanges { intensityChange     = calcIntensityChange SetNormal }
        ApplyBold           -> requiredChanges { intensityChange     = calcIntensityChange SetBold   }
        RemoveBold          -> requiredChanges { intensityChange     = calcIntensityChange SetNormal }

    calcStyleChange :: Style -> StyleChange -> StyleChange
    calcStyleChange style change =
      case change of
        NoChange -> NoChange
        Set      -> if hasStyle style (fixedStyle prevAttr) then NoChange else Set
        Unset    -> if hasStyle style (fixedStyle prevAttr) then Unset else NoChange

    -- | Given whether the two settings for the styles 'dim' and 'bold', compute what change
    --   is required to achieve the requested new intensity setting: dim, normal, or bold.
    calcIntensityChange :: IntensityChange -> IntensityChange
    calcIntensityChange requestedIntensityChange =
      let isDimSet = hasStyle dim (fixedStyle prevAttr)
          isBoldSet = hasStyle bold (fixedStyle prevAttr)
          shouldIntensityChange =
            case (isDimSet, isBoldSet) of
              (False, False) -> requestedIntensityChange /= SetNormal
              (False, True)  -> requestedIntensityChange /= SetBold
              (True, False)  -> requestedIntensityChange /= SetDim
              _              -> error "Should never have both bold and dim applied at the same time" in
      if shouldIntensityChange then requestedIntensityChange else NoIntensityChange

    -- | Given a requested change to the current foreground or background color, return the required
    --   sequence to select the new color mode.
    writeColorDiff :: DisplayColorDiff -> ColorSide -> Write
    writeColorDiff NoColorChange _ = mempty
    writeColorDiff ColorToDefault side =
      case side of
        Foreground -> writeByteString setDefaultForegroundColor
        Background -> writeByteString setDefaultBackgroundColor
    writeColorDiff (SetColor c) side = writeByteString $ setColorSeq side c colorMode


-- | This function generates the Write value corresponding to the escape sequence for the 'reset' command,
-- followed by the escape sequences for all non-default attributes.
writeOnlyNonDefaults :: ColorMode ->  WindowsAttrChanges -> FixedAttr -> Write
writeOnlyNonDefaults colorMode attrChanges prevAttr = writeByteString setDefaultAttr <> writeNonDefaultAttrs
  where
    -- Write an attribute if the new value is non-default. This applies to attributes that haven't changed, but
    -- are currently non-default as well as attributes that were default and are now non-default.
    writeNonDefaultAttrs :: Write
    writeNonDefaultAttrs =
      writeNonDefaultStyles
      <> writeNonDefaultColor (fgColorDiff attrChanges) Foreground
      <> writeNonDefaultColor (bgColorDiff attrChanges) Background

    writeNonDefaultStyles :: Write
    writeNonDefaultStyles = writeByteString $ setStyleSeq calcRequiredStyleChanges

    calcRequiredStyleChanges :: RequiredStyleChanges
    calcRequiredStyleChanges =
      let maybeSet needsSet = if needsSet then Set else NoChange in
      RequiredStyleChanges
        (maybeSet needsItalicSet)
        (maybeSet needsStrikeThroughSet)
        (maybeSet needsUnderlineSet)
        (maybeSet needsReverseVideoSet)
        (maybeSet needsBlinkSet)
        calcIntensityChange

    needsItalicSet        = needsStyleSet italicChange italic
    needsStrikeThroughSet = needsStyleSet strikethroughChange strikethrough
    needsUnderlineSet     = needsStyleSet underlineChange underline
    needsReverseVideoSet  = needsStyleSet reverseVideoChange reverseVideo
    needsBlinkSet         = needsStyleSet blinkChange blink

    needsStyleSet :: (RequiredStyleChanges -> StyleChange) -> Style -> Bool
    needsStyleSet getter theStyle =
      getter (requiredStyleChanges attrChanges) == Set ||
      getter (requiredStyleChanges attrChanges) == NoChange && hasStyle (fixedStyle prevAttr) theStyle

    calcIntensityChange :: IntensityChange
    calcIntensityChange =
      let wasDimSet = hasStyle dim (fixedStyle prevAttr)
          wasBoldSet = hasStyle bold (fixedStyle prevAttr) in
      case intensityChange $ requiredStyleChanges attrChanges of
        SetBold -> SetBold
        SetDim  -> SetDim
        NoIntensityChange -> if wasDimSet then SetDim else if wasBoldSet then SetBold else NoIntensityChange
        SetNormal -> NoIntensityChange

    -- | Given a requested change to the current foreground or background color, return the required
    --   sequence to select the new color mode.
    writeNonDefaultColor :: DisplayColorDiff -> ColorSide -> Write
    writeNonDefaultColor ColorToDefault _ = mempty
    writeNonDefaultColor NoColorChange side = 
      let prevColor = (if side == Foreground then fixedForeColor else fixedBackColor) prevAttr in
      case prevColor of
        Nothing -> mempty
        Just c -> writeByteString $ setColorSeq side c colorMode
    writeNonDefaultColor (SetColor c) side = writeByteString $ setColorSeq side c colorMode

-- | Write the escape sequences that are used to include embedded hyperlinks.
writeUrlChange :: Bool -> URLDiff -> Write
writeUrlChange True (LinkTo url) = writeByteString $ startHyperlink url
writeUrlChange True EndLink      = writeByteString endHyperlink
writeUrlChange _ _               = mempty
