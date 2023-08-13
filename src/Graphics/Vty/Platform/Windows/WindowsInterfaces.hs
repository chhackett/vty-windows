{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Graphics.Vty.Platform.Windows.WindowsInterfaces
  ( readBuf,
    configureInput,
    configureOutput
  ) where

#include "windows_cconv.h"

import Graphics.Vty.Platform.Windows.WindowsConsoleInput
import Graphics.Vty.Input.Events ( Event(EvResize), InternalEvent(InputEvent) )

import Control.Concurrent (yield)
import Control.Concurrent.STM ( TChan, atomically, writeTChan )
import Control.Monad (foldM)
import Data.Bits ((.|.), (.&.), shiftL)
import Codec.Binary.UTF8.String (encodeChar)
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import GHC.Ptr ( Ptr )
import System.IO ( Handle )
import System.Win32.Types ( HANDLE, withHandleToHANDLE, DWORD )
import System.Win32.Console

foreign import ccall "windows.h WaitForSingleObject" c_WaitForSingleObject
    :: HANDLE -> DWORD -> IO DWORD

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
                    if isKeyDown
                    then processCWChar (offset, mSurrogateVal) $ fromEnum cwChar
                    else return (offset, Nothing)
                WindowBufferSizeRecordU (WindowBufferSizeRecordC (COORD x y)) -> do
                    let resize = EvResize (fromIntegral x) (fromIntegral y)
                    atomically $ writeTChan eventChannel (InputEvent resize)
                    return (offset, Nothing)
                -- Drop focus events for now, since we should receive equivalent virtual
                -- terminal sequences when focus mode is enabled.
                FocusEventRecordU (FocusEventRecordC _) -> return (offset, Nothing)
                _ -> return (offset, Nothing)

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


-- Configure Windows to correctly handle input/output in virtual terminal mode
configureInput :: Handle -> IO (IO (), IO ())
configureInput inputHandle = do
    withHandleToHANDLE inputHandle $ \wh -> do
        original <- getConsoleMode wh
        let setMode = setConsoleMode wh $ eNABLE_VIRTUAL_TERMINAL_INPUT .|. eNABLE_EXTENDED_FLAGS
        pure (setMode,
              setConsoleMode wh original)

configureOutput :: Handle -> IO (IO ())
configureOutput outputHandle = do
    withHandleToHANDLE outputHandle $ \wh -> do
        original <- getConsoleMode wh
        setConsoleMode wh $ eNABLE_VIRTUAL_TERMINAL_PROCESSING .|. eNABLE_PROCESSED_OUTPUT
        pure (setConsoleMode wh original)
