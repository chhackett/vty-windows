-- | Provides function to initialize the Vty data structure. This is the entry
-- point for Vty applications developed for Windows. In cross-platform
-- development however, users should use the Graphics.Vty.CrossPlatform module
-- instead.
module Graphics.Vty.Platform.Windows
  ( mkVty,
    mkVtyWithSettings
  )
where

import Control.Monad (when)

import Graphics.Vty (Vty, installCustomWidthTable, mkVtyFromPair)

import Graphics.Vty.Config (VtyUserConfig(..))
import Graphics.Vty.Platform.Windows.Settings
    ( defaultSettings, WindowsSettings(settingTermName) )
import Graphics.Vty.Platform.Windows.Input ( buildInput )
import Graphics.Vty.Platform.Windows.Output ( buildOutput )

-- | Given a user configuration, initialize the Vty environment
mkVty :: VtyUserConfig -> IO Vty
mkVty userConfig = mkVtyWithSettings userConfig =<< defaultSettings

-- | Create a Vty handle. At most one handle should be created at a time
-- for a given terminal device.
--
-- The specified configuration is added to the the configuration
-- loaded by 'userConfig' with the 'userConfig' configuration taking
-- precedence. See "Graphics.Vty.Config".
--
-- For most applications @mkVty defaultConfig@ is sufficient.
mkVtyWithSettings :: VtyUserConfig -> WindowsSettings -> IO Vty
mkVtyWithSettings userConfig settings = do
    when (configAllowCustomUnicodeWidthTables userConfig /= Just False) $
        installCustomWidthTable (configDebugLog userConfig)
                                (Just $ settingTermName settings)
                                (configTermWidthMaps userConfig)

    input <- buildInput userConfig settings
    out <- buildOutput settings
    mkVtyFromPair input out