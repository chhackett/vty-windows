{-# LANGUAGE AllowAmbiguousTypes #-}

module Graphics.Vty.Platform.Windows.Output.WindowSizer
  ( WindowSizer (..)
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

class Monad a => WindowSizer a where
  getWindowSize :: Handle -> a (Int, Int)
  setWindowSize :: Handle -> (Int, Int) -> a ()

foreign import ccall "set_screen_size" cSetScreenSize :: CInt -> CInt -> HANDLE -> IO CInt

instance WindowSizer IO where
  getWindowSize handle = do
    bufferInfo <- withHandleToHANDLE handle getConsoleScreenBufferInfo
    let coord = dwSize bufferInfo
    return (fromIntegral $ xPos coord, fromIntegral $ yPos coord)

  -- \| Resize the console window to the specified size. Throws error on failure.
  setWindowSize hOut (w, h) = do
    result <- withHandleToHANDLE hOut $ cSetScreenSize (fromIntegral w) (fromIntegral h)
    if result == 0
      then return ()
      else error $ "Unable to setup window size. Got error code: " ++ show result
