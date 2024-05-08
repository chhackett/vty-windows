-- | This module provides a function to build an 'Output' for Windows
-- terminals.
--
-- This module is exposed for testing purposes only; applications should
-- never need to import this directly.
module Graphics.Vty.Platform.Windows.Output
  ( buildOutput
  )
where

import Graphics.Vty.Attributes.Color (ColorMode(..))
import Graphics.Vty.Config (VtyUserConfig(..))
import Graphics.Vty.Platform.Windows.Settings (WindowsSettings(..))
import Graphics.Vty.Platform.Windows.Output.XTermColor (reserveTerminal)
import Graphics.Vty.Output (Output)

-- | Returns an `Output` for the terminal.
--
-- The specific Output implementation used is Xterm like because Windows
-- supports xterm like terminal capabilities.
buildOutput :: VtyUserConfig -> WindowsSettings -> IO Output
buildOutput config settings = do
    let outHandle = settingOutputFd settings
        termName = settingTermName settings

    colorMode <- case configPreferredColorMode config of
        Nothing -> return FullColor
        Just m  -> return m

    reserveTerminal termName outHandle colorMode
