{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Vty.Platform.Windows.Output.Color
  ( detectColorMode
  , defaultColorMode
  )
where

import Control.Exception (Exception(..), throwIO)
import Data.Typeable (Typeable)
import Graphics.Vty.Attributes.Color ( ColorMode(..) )

-- | Type of errors that can be thrown when configuring VTY
newtype VtyConfigurationError =
    VtyUnsupportedTermType String
    -- ^ Terminal type not supported by vty
    deriving (Show, Eq, Typeable)

instance Exception VtyConfigurationError where
    displayException (VtyUnsupportedTermType name) = "TERM type [" ++ name ++ "] is not supported at this time"

-- Windows console supports 
detectColorMode :: String -> IO ColorMode
detectColorMode termType =
  case termType of
    "xterm-256color" -> return $ ColorMode240 255
    "xterm"           -> return $ ColorMode240 255
    _                -> throwIO $ VtyUnsupportedTermType termType

defaultColorMode :: IO ColorMode
defaultColorMode = return $ ColorMode240 255