-- | Escape sequences for focus events and some focus related functions
module Graphics.Vty.Platform.Windows.Input.Focus
  ( isFocusEvent,
    classifyFocusEvent,
  )
where

import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Graphics.Vty.Input.Events (Event (EvGainedFocus, EvLostFocus))
import Graphics.Vty.Platform.Windows.Input.Classify.Parse (expectChar, failParse, readChar, runParser)
import Graphics.Vty.Platform.Windows.Input.Classify.Types (KClass)

-- | Does the specified string begin with a focus event?
isFocusEvent :: ByteString -> Bool
isFocusEvent s =
  BS8.isPrefixOf focusIn s
    || BS8.isPrefixOf focusOut s

focusIn :: ByteString
focusIn = BS8.pack "\ESC[I"

focusOut :: ByteString
focusOut = BS8.pack "\ESC[O"

-- | Attempt to classify an input string as a focus event.
classifyFocusEvent :: ByteString -> KClass
classifyFocusEvent s = runParser s $ do
  unless (isFocusEvent s) failParse

  expectChar '\ESC'
  expectChar '['

  ty <- readChar
  case ty of
    'I' -> return EvGainedFocus
    'O' -> return EvLostFocus
    _ -> failParse
