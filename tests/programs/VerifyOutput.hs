{-# LANGUAGE ScopedTypeVariables #-}
module VerifyOutput where

import Graphics.Vty
import Graphics.Vty.Platform.Windows.Settings
import Graphics.Vty.Platform.Windows.Output

import Verify
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Output

import Control.Monad
import System.IO (IOMode(..), hPutBufNonBlocking, openFile, hClose, stdout)

tests :: IO [Test]
tests = do
    putStrLn "testing output: "
    return [ verify "verify terminal could output a picture"
                    smokeTestTermNonMac
           ]

smokeTestTermNonMac :: Image -> Property
smokeTestTermNonMac i = liftIOResult $ do
    smokeTestTerm i

smokeTestTerm :: Image -> IO Result
smokeTestTerm i = do
    -- nullOut <- openHandleToNull
    -- testHandle <- openFile "C:\\temp\\verifyOutput.txt" WriteMode
    s <- defaultSettings
    t <- buildOutput $ s
            { --settingOutputFd = stdout
              settingColorMode = NoColor
            }
    -- putStrLn $ "context color count: " ++ show (contextColorCount t)
    reserveDisplay t
    dc <- displayContext t (100,100)
    -- always show the cursor to produce tests for terminals with no
    -- cursor support.
    let pic = (picForImage i) { picCursor = Cursor 0 0 }
    outputPicture dc pic
    setCursorPos t 0 0
    when (supportsCursorVisibility t) $ do
        hideCursor t
        showCursor t
    releaseDisplay t
    releaseTerminal t
    -- hClose testHandle
    return succeeded


openHandleToNull = openFile "\\\\.\\NUL" WriteMode