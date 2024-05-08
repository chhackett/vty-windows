{-# LANGUAGE OverloadedStrings #-}

-- | Provides functions for configuring the terminal for VT processing, and to
-- change the window size
module Graphics.Vty.Platform.Windows.Output.TerminfoBased
  ( reserveTerminal,
  )
where

import Blaze.ByteString.Builder (Write, writeByteString)
import Control.Monad (void, when)
import Data.Foldable (foldl')
import Data.IORef (newIORef, readIORef, writeIORef)
import Graphics.Vty.Attributes
  ( Attr,
    ColorMode,
    FixedAttr (fixedStyle),
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
import Graphics.Vty.Platform.Windows.Output.AnsiSequences
  ( ColorSide (..),
    MaybeSetExit (..),
    MaybeSetExitIntensity (..),
    RequiredStyleChanges (..),
    clearScreen,
    clearToEndOfLine,
    endHyperlink,
    enterAlternateScreenBuffer,
    exitAlternateScreenBuffer,
    hideCursorSeq,
    moveCursorPos,
    setColorSeq,
    setDefaultAttr,
    setDefaultBackgroundColor,
    setDefaultForegroundColor,
    setStyleSeq,
    showCursorSeq,
    startHyperlink,
  )
import Graphics.Vty.Platform.Windows.Output.WindowSizer
  ( WindowSizer (..),
  )
import Graphics.Vty.Platform.Windows.WindowsInterfaces (configureOutput, writeBuf)
import System.IO (Handle)

-- | Constructs an output driver
reserveTerminal :: String -> Handle -> ColorMode -> IO Output
reserveTerminal termName outHandle colorMode = do
  restoreMode <- configureOutput outHandle

  hyperlinkModeStatus <- newIORef False
  newAssumedStateRef <- newIORef initialAssumedState

  let terminfoSetMode m newStatus = do
        curStatus <- terminfoModeStatus m
        when (newStatus /= curStatus) $
          case m of
            Hyperlink -> do
              writeIORef hyperlinkModeStatus newStatus
              writeIORef newAssumedStateRef initialAssumedState
            _ -> return ()
      terminfoModeStatus m =
        case m of
          Hyperlink -> readIORef hyperlinkModeStatus
          _ -> return False
      terminfoModeSupported Hyperlink = True
      terminfoModeSupported _ = False
      sendSeqToOutputBuffer = writeBuf

  return
    Output
      { terminalID = termName,
        releaseTerminal = do
          void $ sendSeqToOutputBuffer outHandle setDefaultAttr
          void $ sendSeqToOutputBuffer outHandle showCursorSeq
          restoreMode,
        supportsBell = pure False,
        supportsItalics = pure True,
        supportsStrikethrough = pure True,
        ringTerminalBell = pure (),
        reserveDisplay = do
          void $ sendSeqToOutputBuffer outHandle enterAlternateScreenBuffer
          void $ sendSeqToOutputBuffer outHandle clearScreen,
        releaseDisplay = do
          void $ sendSeqToOutputBuffer outHandle exitAlternateScreenBuffer
          void $ sendSeqToOutputBuffer outHandle showCursorSeq,
        setDisplayBounds = \(w, h) -> setWindowSize outHandle (w, h),
        displayBounds = do
          rawSize <- getWindowSize outHandle
          case rawSize of
            (w, h)
              | w < 0 || h < 0 -> fail $ "getwinsize returned < 0 : " ++ show rawSize
              | otherwise -> return (w, h),
        outputByteBuffer = void . writeBuf outHandle,
        supportsCursorVisibility = True,
        supportsMode = terminfoModeSupported,
        setMode = terminfoSetMode,
        getModeStatus = terminfoModeStatus,
        assumedStateRef = newAssumedStateRef,
        outputColorMode = colorMode,
        mkDisplayContext = terminfoDisplayContext,
        setOutputWindowTitle = const $ return ()
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
        writeDefaultAttr = \urlsEnabled ->
          writeByteString setDefaultAttr
            <> (if urlsEnabled then writeURLEscapes EndLink else mempty),
        -- <> writeByteString exitStrikethroughStyle
        writeRowEnd = writeByteString clearToEndOfLine,
        inlineHack = return ()
      }

-- | Generate the necessary escape sequences to change terminal attributes
terminfoWriteSetAttr :: ColorMode -> Bool -> FixedAttr -> Attr -> DisplayAttrDiff -> Write
terminfoWriteSetAttr colorMode urlsEnabled prevAttr _ diffs =
       writeStyleChanges prevAttr (styleDiffs diffs)
    <> writeColorDiff (foreColorDiff diffs) Foreground colorMode
    <> writeColorDiff (backColorDiff diffs) Background colorMode
    <> writeUrlChange urlsEnabled (urlDiff diffs)

writeStyleChanges :: FixedAttr -> [StyleStateChange] -> Write
writeStyleChanges prevAttr changes =
  let requiredChanges = foldl' applyStyleChange mempty changes
      requiredChanges' = updateFixedAttrStyle (fixedStyle prevAttr) requiredChanges
   in writeByteString $ setStyleSeq requiredChanges'

applyStyleChange :: RequiredStyleChanges -> StyleStateChange -> RequiredStyleChanges
applyStyleChange requiredChanges styleStateChange =
  case styleStateChange of
    ApplyStandout       -> mempty
    RemoveStandout      -> mempty
    ApplyItalic         -> requiredChanges {italicChange = Set}
    RemoveItalic        -> requiredChanges {italicChange = Exit}
    ApplyStrikethrough  -> requiredChanges {strikethroughChange = Set}
    RemoveStrikethrough -> requiredChanges {strikethroughChange = Exit}
    ApplyUnderline      -> requiredChanges {underlineChange = Set}
    RemoveUnderline     -> requiredChanges {underlineChange = Exit}
    ApplyReverseVideo   -> requiredChanges {reverseVideoChange = Set}
    RemoveReverseVideo  -> requiredChanges {reverseVideoChange = Exit}
    ApplyBlink          -> requiredChanges {blinkChange = Set}
    RemoveBlink         -> requiredChanges {blinkChange = Exit}
    ApplyDim            -> requiredChanges {intensityChange = SetDim}
    RemoveDim           -> requiredChanges {intensityChange = ExitIntensity}
    ApplyBold           -> requiredChanges {intensityChange = SetBold}
    RemoveBold          -> requiredChanges {intensityChange = ExitIntensity}

updateFixedAttrStyle :: Style -> RequiredStyleChanges -> RequiredStyleChanges
updateFixedAttrStyle prevStyle requiredChanges =
  RequiredStyleChanges
    { italicChange        = updateRequiredChange (hasStyle italic prevStyle) (italicChange requiredChanges),
      strikethroughChange = updateRequiredChange (hasStyle strikethrough prevStyle) (strikethroughChange requiredChanges),
      underlineChange     = updateRequiredChange (hasStyle underline prevStyle) (underlineChange requiredChanges),
      reverseVideoChange  = updateRequiredChange (hasStyle reverseVideo prevStyle) (reverseVideoChange requiredChanges),
      blinkChange         = updateRequiredChange (hasStyle blink prevStyle) (blinkChange requiredChanges),
      intensityChange     = updateIntensityChange (hasStyle dim prevStyle) (hasStyle bold prevStyle) (intensityChange requiredChanges)
    }

updateRequiredChange :: Bool -> MaybeSetExit -> MaybeSetExit
updateRequiredChange styleIsSet maybeRequireSet =
  if styleIsSet
    then case maybeRequireSet of
      NoChange -> NoChange
      Set -> NoChange
      Exit -> Exit
    else case maybeRequireSet of
      NoChange -> NoChange
      Set -> Set
      Exit -> NoChange

updateIntensityChange :: Bool -> Bool -> MaybeSetExitIntensity -> MaybeSetExitIntensity
updateIntensityChange False False maybeSetIntensity =
  case maybeSetIntensity of
    KeepCurrentIntensity -> KeepCurrentIntensity
    SetBold -> SetBold
    SetDim -> SetDim
    ExitIntensity -> KeepCurrentIntensity
updateIntensityChange False True maybeSetIntensity =
  case maybeSetIntensity of
    KeepCurrentIntensity -> KeepCurrentIntensity
    SetBold -> KeepCurrentIntensity
    SetDim -> SetDim
    ExitIntensity -> ExitIntensity
updateIntensityChange True False maybeSetIntensity =
  case maybeSetIntensity of
    KeepCurrentIntensity -> KeepCurrentIntensity
    SetBold -> SetBold
    SetDim -> KeepCurrentIntensity
    ExitIntensity -> ExitIntensity
updateIntensityChange True True _ = error "Should never have both bold and dim applied at the same time"

--     case maybeSetIntensity of
--         KeepCurrentIntensity -> KeepCurrentIntensity
--         SetBold              -> SetBold
--         SetDim               -> SetDim
--         ExitIntensity        -> KeepCurrentIntensity

writeColorDiff :: DisplayColorDiff -> ColorSide -> ColorMode -> Write
writeColorDiff NoColorChange _ _ = mempty
writeColorDiff ColorToDefault side _ =
  case side of
    Foreground -> writeByteString setDefaultForegroundColor
    Background -> writeByteString setDefaultBackgroundColor
writeColorDiff (SetColor c) side colorMode = writeByteString $ setColorSeq side c colorMode

writeUrlChange :: Bool -> URLDiff -> Write
writeUrlChange True urlDiff' = writeURLEscapes urlDiff'
writeUrlChange False _ = mempty

-- | Write the escape sequences that are used in some terminals to
-- include embedded hyperlinks. As of yet, this information isn't
-- included in termcap or terminfo, so this writes them directly
-- instead of looking up the appropriate capabilities.
writeURLEscapes :: URLDiff -> Write
writeURLEscapes (LinkTo url) = writeByteString $ startHyperlink url
writeURLEscapes EndLink = writeByteString endHyperlink
writeURLEscapes NoLinkChange = mempty
