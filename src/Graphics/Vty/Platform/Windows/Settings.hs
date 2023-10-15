module Graphics.Vty.Platform.Windows.Settings
  ( WindowsSettings(..)
  , defaultSettings
  )
where

import Data.Maybe ( fromMaybe ) 
import Graphics.Vty.Attributes.Color ( ColorMode(..) )
import Graphics.Vty.Platform.Windows.Output.Color ( detectColorMode, defaultColorMode )
import System.Environment ( lookupEnv )
import System.IO ( Handle, stdin )
import Graphics.Win32.Misc ( getStdHandle, sTD_OUTPUT_HANDLE )
import System.Win32.Types

-- | Runtime library settings for interacting with Windows terminals.
data WindowsSettings = WindowsSettings
  { settingInputFd :: Handle
  -- ^ The input file descriptor to use.
  , settingOutputFd :: Handle
  -- ^ The output file descriptor to use.
  , settingTermName :: String
  -- ^ The terminal name used to look up terminfo capabilities.
  , settingColorMode :: ColorMode
  -- ^ The color mode used to know how many colors the terminal
  -- supports.
  }
  deriving (Show, Eq)


defaultSettings :: IO WindowsSettings
defaultSettings = do
    mb <- lookupEnv termVariable
    let termName = fromMaybe "xterm-256color" mb
    colorMode <- maybe defaultColorMode detectColorMode mb
    winStdOut <- getStdHandle sTD_OUTPUT_HANDLE
    hHandle <- hANDLEToHandle winStdOut

    return $ WindowsSettings { settingInputFd  = stdin
                             , settingOutputFd  = hHandle
                             , settingTermName  = termName
                             , settingColorMode = colorMode
                             }

termVariable :: String
termVariable = "TERM"