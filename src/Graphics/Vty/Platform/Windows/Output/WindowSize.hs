module Graphics.Vty.Platform.Windows.Output.WindowSize
  ( getWindowSize
  , setWindowSize
  )
where

import Foreign.C.Types (CInt (..))
import System.IO (Handle)
import System.Win32.Console
  ( CONSOLE_SCREEN_BUFFER_INFO (dwSize),
    COORD (..),
    getConsoleScreenBufferInfo,
  )
import System.Win32.Types (HANDLE, withHandleToHANDLE)

foreign import ccall "set_screen_size" cSetScreenSize :: CInt -> CInt -> HANDLE -> IO CInt

getWindowSize :: Handle -> IO (Int, Int)
getWindowSize hOut = do
  bufferInfo <- withHandleToHANDLE hOut getConsoleScreenBufferInfo
  let coord = dwSize bufferInfo
  return (fromIntegral $ xPos coord, fromIntegral $ yPos coord)

  -- \| Resize the console window to the specified size. Throws error on failure.
setWindowSize :: Handle -> (Int, Int) -> IO ()
setWindowSize hOut (w, h) = do
  result <- withHandleToHANDLE hOut $ cSetScreenSize (fromIntegral w) (fromIntegral h)
  if result == 0
    then return ()
    else error $ "Unable to setup window size. Got error code: " ++ show result
