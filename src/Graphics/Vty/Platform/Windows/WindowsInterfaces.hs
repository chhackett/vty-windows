{-# LANGUAGE CPP #-}

-- | This module provides wrappers around Win32 API calls. These functions provide initialization,
-- shutdown, and input event handling.
module Graphics.Vty.Platform.Windows.WindowsInterfaces
  ( writeBuf,
    readBuf,
    configureInput,
    configureOutput,
    configureHandle
  )
where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Concurrent (yield)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Monad (foldM, when, void)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import GHC.Ptr (Ptr)
import Graphics.Vty.Input.Events (Event (EvResize), InternalEvent (InputEvent))
import System.IO (Handle, hPutBufNonBlocking)
import System.Win32.Console
  ( COORD (..),
    eNABLE_EXTENDED_FLAGS,
    eNABLE_PROCESSED_OUTPUT,
    eNABLE_VIRTUAL_TERMINAL_INPUT,
    eNABLE_VIRTUAL_TERMINAL_PROCESSING,
    getConsoleMode,
    setConsoleMode,
    setConsoleOutputCP,
    KEY_EVENT_RECORD(..),
    WINDOW_BUFFER_SIZE_RECORD(..),
    INPUT_RECORD(..),
    readConsoleInput
  )
import System.Win32.Event (waitForSingleObject)
import System.Win32.File
  ( createFile,
    fILE_SHARE_WRITE,
    gENERIC_READ,
    gENERIC_WRITE,
    oPEN_EXISTING,
  )
import System.Win32.Types (HANDLE, iNVALID_HANDLE_VALUE, withHandleToHANDLE)

-- | Write a 'ByteString' to a Handle
writeBuf :: Handle -> ByteString -> IO ()
writeBuf handle s =
  BS8.useAsCStringLen s $ \(buf, len) -> do
    void $ hPutBufNonBlocking handle (castPtr buf) (fromIntegral len)

-- | Read the contents of the Windows input buffer. The contents are parsed and either written to
-- the TChan queue for Window size events, or written to the Word8 buffer for keyboard events and
-- VT sequences. Returns the # of bytes written to the Word8 buffer.
readBuf :: TChan InternalEvent -> Ptr INPUT_RECORD -> Handle -> Ptr Word8 -> Int -> IO Int
readBuf eventChannel inputEventPtr handle bufferPtr maxInputRecords = do
  ret <- withHandleToHANDLE handle (`waitForSingleObject` 500)
  yield -- otherwise, the above foreign call causes the loop to never
        -- respond to the killThread
  if ret /= 0
    then readBuf eventChannel inputEventPtr handle bufferPtr maxInputRecords
    else readBuf' eventChannel inputEventPtr handle bufferPtr maxInputRecords

readBuf' :: TChan InternalEvent -> Ptr INPUT_RECORD -> Handle -> Ptr Word8 -> Int -> IO Int
readBuf' eventChannel inputRecordPtr handle bufferPtr maxInputRecords = do
  withHandleToHANDLE handle $ \handle' -> do
    numEvents <- readConsoleInput handle' maxInputRecords inputRecordPtr
    (numBytes, _) <- foldM handleInputEvent (0, Nothing) [0 .. (fromIntegral numEvents - 1)]
    return numBytes
    where
      handleInputEvent :: (Int, Maybe Int) -> Int -> IO (Int, Maybe Int)
      handleInputEvent (offset, mSurrogateVal) idx = do
        inputRecord <- peekElemOff inputRecordPtr idx
        case inputRecord of
          KeyEvent (KEY_EVENT_RECORD isKeyDown _ _ _ cwChar _) -> do
            -- Process the character if this is a 'key down' event, AND the char is not NULL
            if isKeyDown && cwChar /= 0
            then processCWChar (offset, mSurrogateVal) $ fromEnum cwChar
            else return (offset, Nothing)
          WindowBufferSizeEvent (WINDOW_BUFFER_SIZE_RECORD (COORD x y)) -> do
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
        mapM_ (\(w, offset') -> pokeElemOff bufferPtr (offset + offset') w) $ zip utf8Char [0 ..]
        return (offset + length utf8Char, Nothing)

-- | Configure Windows to correctly handle input for a Vty application
configureInput :: Handle -> IO (IO (), IO ())
configureInput _ = do
  inHandle <- configureHandle "CONIN$"
  original <- getConsoleMode inHandle
  let setMode = setConsoleMode inHandle $ eNABLE_VIRTUAL_TERMINAL_INPUT .|. eNABLE_EXTENDED_FLAGS
  pure (setMode, setConsoleMode inHandle original)

-- | Configure Windows to correctly handle output for a Vty application
configureOutput :: Handle -> IO (IO ())
configureOutput _ = do
  outHandle <- configureHandle "CONOUT$"
  original <- getConsoleMode outHandle
  setConsoleOutputCP 65001
  setConsoleMode outHandle $ eNABLE_VIRTUAL_TERMINAL_PROCESSING .|. eNABLE_PROCESSED_OUTPUT
  pure $ setConsoleMode outHandle original

configureHandle :: String -> IO HANDLE
configureHandle handleName = do
  outHandle <-
    createFile
      handleName
      (gENERIC_WRITE .|. gENERIC_READ)
      fILE_SHARE_WRITE
      Nothing
      oPEN_EXISTING
      0
      Nothing
  when (outHandle == iNVALID_HANDLE_VALUE) $ fail $ "Unable to configure terminal for input/output with handle: " ++ handleName
  return outHandle