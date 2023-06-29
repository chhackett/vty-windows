module Graphics.Vty.Platform.Windows
  ( mkVty
  )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Graphics.Vty (Vty, installCustomWidthTable, mkVtyFromPair)

import Graphics.Vty.Config (VtyUserConfig(..))
-- import Graphics.Vty.Platform.Windows.Config
import Graphics.Vty.Platform.Windows.Settings
import Graphics.Vty.Platform.Windows.Input
import Graphics.Vty.Platform.Windows.Output

-- | Create a Vty handle. At most one handle should be created at a time
-- for a given terminal device.
--
-- The specified configuration is added to the the configuration
-- loaded by 'userConfig' with the 'userConfig' configuration taking
-- precedence. See "Graphics.Vty.Config".
--
-- For most applications @mkVty defaultConfig@ is sufficient.
mkVty :: VtyUserConfig -> Maybe WindowsSettings -> IO Vty
mkVty userConfig mWindowsConfig = do
    settings <- fromMaybe <$> defaultSettings <*> pure mWindowsConfig

    when (configAllowCustomUnicodeWidthTables userConfig /= Just False) $
        installCustomWidthTable (configDebugLog userConfig)
                                (Just $ settingTermName settings)
                                (configTermWidthMaps userConfig)

    input <- buildInput userConfig settings
    out <- buildOutput settings
    mkVtyFromPair input out