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