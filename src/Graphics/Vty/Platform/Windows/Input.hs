{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides the input layer for Vty, including methods
-- for initializing an 'Input' structure and reading 'Event's from the
-- terminal.
module Graphics.Vty.Platform.Windows.Input (buildInput) where

import Control.Concurrent.STM (TVar)
import Data.Maybe (isNothing)
import Graphics.Vty.Config (VtyUserConfig (..))
import Graphics.Vty.Image (DisplayRegion)
import Graphics.Vty.Input (Input (restoreInputState, shutdownInput))
import Graphics.Vty.Platform.Windows.Input.Loop (initInput)
import Graphics.Vty.Platform.Windows.Input.Terminfo (classifyMapForTerm)
import Graphics.Vty.Platform.Windows.Settings (WindowsSettings (settingInputFd, settingTermName))
import Graphics.Vty.Platform.Windows.WindowsInterfaces (configureInput, isEnvMintty)

-- | Set up the terminal with the specified user configuration and windows settings for input.
-- Returns an 'Input'.
--
-- The table used to determine the 'Events' to produce for the input
-- bytes comes from 'classifyMapForTerm' which is then overridden by
-- the applicable entries from the configuration's 'inputMap'.
--
-- The terminal device's mode flags are configured by the
-- 'attributeControl' function.
buildInput :: TVar (Maybe DisplayRegion) -> VtyUserConfig -> WindowsSettings -> IO Input
buildInput screenSizeVar userConfig settings  = do
    let tName = settingTermName settings
        handle = settingInputFd settings
    let inputOverrides = [(s,e) | (t,s,e) <- configInputMap userConfig, isNothing t || t == Just tName]
        activeInputMap = classifyMapForTerm `mappend` inputOverrides
    isMintty <- isEnvMintty handle
    (setAttrs, unsetAttrs) <- configureInput handle isMintty
    setAttrs
    input <- initInput screenSizeVar userConfig handle activeInputMap isMintty

    return $ input
        { shutdownInput = do
            shutdownInput input
            unsetAttrs
        , restoreInputState = restoreInputState input >> unsetAttrs
        }
