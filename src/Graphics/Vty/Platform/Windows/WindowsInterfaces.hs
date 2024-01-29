{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables #-}

-- | This module provides wrappers around Win32 API calls. These functions provide initialization,
-- shutdown, and input event handling.
module Graphics.Vty.Platform.Windows.WindowsInterfaces
  ( readBuf,
    configureInput,
    configureOutput,
    isEnvMintty
  ) where

#include "windows_cconv.h"

import Graphics.Vty.Platform.Windows.WindowsConsoleInput
import Graphics.Vty.Input.Events ( Event(EvResize), InternalEvent(InputEvent) )

import Control.Concurrent (yield)
import Control.Concurrent.STM ( TChan, atomically, writeTChan )
import Control.Exception (try, SomeException(..))
import Control.Monad (foldM)
import Data.Bits ((.|.), (.&.), shiftL)
import Codec.Binary.UTF8.String (encodeChar)
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import GHC.Ptr ( Ptr )
import System.IO ( Handle, hGetContents )
import System.Win32.Types ( HANDLE, withHandleToHANDLE, DWORD )
import System.Win32.Console
import System.Process (StdStream(..), createProcess, shell,
                       std_in, std_out, waitForProcess)
import System.Exit


foreign import ccall "windows.h WaitForSingleObject" c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD

-- | Read the contents of the Windows input buffer. The contents are parsed and either written to
-- the TChan queue for Window size events, or written to the Word8 buffer for keyboard events and 
-- VT sequences. Returns the # of bytes written to the Word8 buffer.
readBuf :: TChan InternalEvent -> Ptr WinConsoleInputEvent -> Handle -> Ptr Word8 -> Int -> IO Int
readBuf eventChannel inputEventPtr handle bufferPtr maxInputRecords = do
    ret <- withHandleToHANDLE handle (`c_WaitForSingleObject` 500)
    yield -- otherwise, the above foreign call causes the loop to never
          -- respond to the killThread
    if ret /= 0
    then readBuf eventChannel inputEventPtr handle bufferPtr maxInputRecords
    else readBuf' eventChannel inputEventPtr handle bufferPtr maxInputRecords

readBuf' :: TChan InternalEvent -> Ptr WinConsoleInputEvent -> Handle -> Ptr Word8 -> Int -> IO Int
readBuf' eventChannel inputEventPtr handle bufferPtr maxInputRecords = do
    inputEvents <- readConsoleInput inputEventPtr maxInputRecords handle
    (offset, _) <- foldM handleInputEvent (0, Nothing) inputEvents
    return offset
    where
        handleInputEvent :: (Int, Maybe Int) -> WinConsoleInputEvent -> IO (Int, Maybe Int)
        handleInputEvent (offset, mSurrogateVal) inputEvent = do
            case inputEvent of
                KeyEventRecordU (KeyEventRecordC isKeyDown _ _ _ cwChar _) -> do
                    -- Process the character if this is a 'key down' event,
                    -- AND the char is not NULL
                    if isKeyDown && cwChar /= 0
                    then processCWChar (offset, mSurrogateVal) $ fromEnum cwChar
                    else return (offset, Nothing)
                WindowBufferSizeRecordU (WindowBufferSizeRecordC (COORD x y)) -> do
                    let resize = EvResize (fromIntegral x) (fromIntegral y)
                    atomically $ writeTChan eventChannel (InputEvent resize)
                    return (offset, Nothing)
                _ -> return (offset, Nothing)

        processCWChar :: (Int, Maybe Int) -> Int -> IO (Int, Maybe Int)
        processCWChar (offset, Nothing) charVal = do
            if isSurrogate charVal
            then return (offset, Just charVal)
            else encodeAndWriteToBuf offset charVal
            where
                isSurrogate :: Int -> Bool
                isSurrogate c = 0xD800 <= c && c < 0xDC00
        processCWChar (offset, Just surogateVal) charVal = do
            let charVal' = (((surogateVal .&. 0x3FF) `shiftL` 10) .|. (charVal .&. 0x3FF)) + 0x10000
            encodeAndWriteToBuf offset charVal'

        encodeAndWriteToBuf :: Int -> Int -> IO (Int, Maybe Int)
        encodeAndWriteToBuf offset charVal = do
            let utf8Char = encodeChar $ toEnum charVal
            mapM_ (\(w, offset') -> pokeElemOff bufferPtr (offset + offset') w) $ zip utf8Char [0..]
            return (offset + length utf8Char, Nothing)

-- | Configure Windows to correctly handle input for a Vty application
configureInput :: Handle -> Bool -> IO (IO (), IO ())
configureInput inputHandle isMintty = do
  withHandleToHANDLE inputHandle $ \wh -> do
    if isMintty
    then do
      settings <- getSttySettings inputHandle
      return (configureStty "raw -echo" inputHandle, configureStty settings inputHandle)
    else do
        original <- getConsoleMode wh
        let setMode = setConsoleMode wh $ eNABLE_VIRTUAL_TERMINAL_INPUT .|. eNABLE_EXTENDED_FLAGS
        pure (setMode,
              setConsoleMode wh original)

-- | Commands to configure the @stty@ command-line utility.
getSttySettings :: Handle -> IO String
getSttySettings inputHandle = configureStty' "-g" inputHandle

-- | Create an @stty@ process, wait for it to complete, and return its output.
configureStty :: String -> Handle -> IO ()
configureStty params inputHandle = do
  _ <- configureStty' params inputHandle
  return ()

configureStty' :: String -> Handle -> IO String
configureStty' params inputHandle = do
  let stty = (shell $ "stty " ++ params) {
        std_in  = UseHandle inputHandle
      , std_out = CreatePipe
      }
  (_, mbStdout, _, rStty) <- createProcess stty
  exStty <- waitForProcess rStty
  case exStty of
    e@ExitFailure{} -> error $ show e
    ExitSuccess     -> maybe (return "") hGetContents mbStdout

-- | Configure Windows to correctly handle output for a Vty application
configureOutput :: Handle -> Bool -> IO (IO ())
configureOutput outputHandle isMintty = do
    withHandleToHANDLE outputHandle $ \wh -> do
        setConsoleOutputCP 65001
        if isMintty
        then return (return ())
        else do
            original <- getConsoleMode wh
            setConsoleMode wh $ eNABLE_VIRTUAL_TERMINAL_PROCESSING .|. eNABLE_PROCESSED_OUTPUT
            pure (setConsoleMode wh original)

isEnvMintty :: Handle -> IO Bool
isEnvMintty handle = do
  result <- try $ withHandleToHANDLE handle $ \wh -> getConsoleMode wh
  return $
    case result of
          Left (_ :: SomeException) -> True
          Right _ -> False
