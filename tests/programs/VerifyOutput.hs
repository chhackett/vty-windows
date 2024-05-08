{-# LANGUAGE ScopedTypeVariables #-}

module VerifyOutput where

import Control.Monad
import Graphics.Vty
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Platform.Windows.Output (buildOutput)
import Graphics.Vty.Platform.Windows.Settings (defaultSettings)
import System.IO (IOMode (..), hClose, hPutBufNonBlocking, openFile)
import Verify
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Output

tests :: IO [Test]
tests = do
  putStrLn "testing output: "
  return
    [ verify
        "verify terminal could output a picture"
        smokeTestTermNonMac
    ]

smokeTestTermNonMac :: Image -> Property
smokeTestTermNonMac i = liftIOResult $ do
  smokeTestTerm i

smokeTestTerm :: Image -> IO Result
smokeTestTerm i = do
  s <- defaultSettings
  t <- buildOutput defaultConfig s
  reserveDisplay t
  dc <- displayContext t (100, 100)
  -- always show the cursor to produce tests for terminals with no
  -- cursor support.
  let pic = (picForImage i) {picCursor = Cursor 0 0}
  outputPicture dc pic
  setCursorPos t 0 0
  when (supportsCursorVisibility t) $ do
    hideCursor t
    showCursor t
  releaseDisplay t
  releaseTerminal t
  -- hClose testHandle
  return succeeded
