{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Graphics.Vty.Platform.Windows.WindowsInterfaces
  ( readBuf,
    defaultDebugLog,
    configureOutput
  ) where

#include "windows_cconv.h"

import Graphics.Vty.Platform.Windows.WindowsConsoleInput
import Graphics.Vty.Input.Events

import Control.Concurrent (yield)
import Control.Concurrent.STM ( TChan, atomically, writeTChan )
import Control.Monad (foldM)
import Data.Bits ((.|.), (.&.), shiftL)
import Codec.Binary.UTF8.String (encodeChar)
import Data.Word (Word8)
import Foreign.C.Types ( CWchar(..) )
import Foreign.Storable (Storable(..))
import GHC.Ptr ( Ptr )
import System.IO ( Handle )
import System.Win32.Types ( HANDLE, withHandleToHANDLE, DWORD )
import System.Win32.Console

foreign import ccall "windows.h WaitForSingleObject" c_WaitForSingleObject
    :: HANDLE -> DWORD -> IO DWORD

-- Thanks to ShrykeWindGrace for the following code to allow other threads to execute
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
                KeyEventRecordU keyEvent@(KeyEventRecordC isKeyDown _ _ _ cwChar _) -> do
                    logInput $ "Key event: " ++ show keyEvent
                    if isKeyDown
                    then processCWChar (offset, mSurrogateVal) $ readBytes cwChar
                    else return (offset, Nothing)
                WindowBufferSizeRecordU (WindowBufferSizeRecordC (COORD x y)) -> do
                    logInput $ "Window size changed, new size: " ++ show (x, y)
                    let resize = EvResize (fromIntegral x) (fromIntegral y)
                    atomically $ writeTChan eventChannel (InputEvent resize)
                    return (offset, Nothing)
                FocusEventRecordU (FocusEventRecordC hasFocus) -> do
                    logInput $ "Received focus event. hasFocus=" ++ show hasFocus
                    let focusEvent = if hasFocus then EvGainedFocus else EvLostFocus
                    atomically $ writeTChan eventChannel (InputEvent focusEvent)
                    return (offset, Nothing)
                anEvent -> do
                    logInput $ "Received some event - ignoring..." ++ show anEvent
                    return (offset, Nothing)

        readBytes :: CWchar -> Int
        readBytes = fromEnum

        processCWChar :: (Int, Maybe Int) -> Int -> IO (Int, Maybe Int)
        processCWChar (offset, Nothing) charVal = do
            if isSurrogate charVal
            then return (offset, Just charVal)
            else do
                let utf8Char = encodeChar $ toEnum charVal
                writeToBuffer utf8Char offset
                return (offset + length utf8Char, Nothing)
        processCWChar (offset, Just surogateVal) charVal = do
            let utf8Char = encodeChar $ toEnum $ (((surogateVal .&. 0x3FF) `shiftL` 10) .|. (charVal .&. 0x3FF)) + 0x10000
            writeToBuffer utf8Char offset
            return (offset + length utf8Char, Nothing)

        writeToBuffer :: [Word8] -> Int -> IO ()
        writeToBuffer cs offset = mapM_ (\(w, offset') -> pokeElemOff bufferPtr (offset + offset') w) $ zip cs [0..]

        isSurrogate :: Int -> Bool
        isSurrogate charVal = 0xD800 <= charVal && charVal < 0xDC00

-- writeBuf :: Handle -> Ptr Word8 -> Int -> IO Int
-- writeBuf = hPutBufNonBlocking

-- bufferToString :: Ptr Word8 -> Int -> IO String
-- bufferToString ptr n = do
--   foldM nextChar [] [(n-1), (n-2)..0]
--   where
--     nextChar :: String -> Int -> IO String
--     nextChar theWords idx = do
--       theWord <- peekElemOff ptr idx
--       return (w2c theWord:theWords)

-- waitForInput :: Handle -> IO ()
-- waitForInput _ = return ()

-- For now will default to the DEL character. Not sure if this is correct though.
-- getTtyEraseChar :: Handle -> IO (Maybe Char)
-- getTtyEraseChar _ = return $ Just $ toEnum 127


-- This function is necessary to configure Windows to correctly handle output in 'virtual terminal' mode
configureOutput :: Handle -> IO (IO ())
configureOutput outputHandle = do
    logOutput "Configuring output..."
    withHandleToHANDLE outputHandle $ \wh -> do
        original <- getConsoleMode wh
        setConsoleMode wh $ eNABLE_VIRTUAL_TERMINAL_PROCESSING .|. eNABLE_PROCESSED_OUTPUT
        pure (setConsoleMode wh original)

logInput :: String -> IO ()
logInput input = appendFile "C:\\temp\\input.txt" $ input ++ "\n"

-- appendFile fails when unicode characters are a part of the string. lame
logOutput :: String -> IO ()
logOutput output = appendFile "C:\\temp\\output.txt" $ output ++ "\n"

defaultDebugLog :: String -> IO ()
defaultDebugLog msg = appendFile "C:\\temp\\debug.txt" $ msg ++ "\n"
