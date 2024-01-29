{-# LANGUAGE CPP #-}

module Graphics.Vty.Platform.Windows.ScreenSize
  (isScreenSizeEvent, classifyScreenSize) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Graphics.Vty.Platform.Windows.Input.Classify.Types
    ( KClass(..) )
import Graphics.Vty.Platform.Windows.Input.Classify.Parse
import Graphics.Vty.Input.Events (Event(..))
-- import System.IO.FD

-- Options to get screen size in mintty terminals:
-- 1 - Send cap expression '\ESC[18t', parse the response with format '\ESC[8;YY;XXt'
-- 2 - Set cursor position to 999,999, then send 'report cursor' sequence and parse the response with format '\ESC[YY;XXR'
-- 3 - Use ioctl interface to read the TIOCGWINSZ struct.
        -- Apparently mingw does not include ioctl header file or implementation. I can't get the c_getWindowSize function to compile. 
        -- Error is 'can't find ioctl.h'

-- foreign import ccall "gwinsz.h vty_c_get_window_size" c_getWindowSize :: Fd -> IO CLong

-- getWindowSize :: IO (Int, Int)
-- getWindowSize = do
--   (a,b) <- (`divMod` 65536) `fmap` c_getWindowSize stdout
--   return (fromIntegral b, fromIntegral a)

-- foreign import ccall "gwinsz.h vty_c_set_window_size" c_setWindowSize :: Fd -> CLong -> IO ()

-- setWindowSize :: Fd -> (Int, Int) -> IO ()
-- setWindowSize fd (w, h) = do
--     let val = (h `shiftL` 16) + w
--     c_setWindowSize fd $ fromIntegral val


isScreenSizeEvent :: BS.ByteString -> Bool 
isScreenSizeEvent bytes = BSC.isPrefixOf csiPrefix bytes && BSC.isSuffixOf (BSC.pack "t") bytes

csiPrefix :: BS.ByteString
csiPrefix = BSC.pack "\ESC["

-- | Attempt to classify an input string as a screen size event.
classifyScreenSize :: BS.ByteString -> KClass
classifyScreenSize s = runParser s $ do
    when (not $ isScreenSizeEvent s) failParse

    expectChar '\ESC'
    expectChar '['
    expectChar '8'
    expectChar ';'
    height <- readInt
    expectChar ';'
    width <- readInt
    ty <- readChar
    case ty of
        't' -> return $ EvResize width height
        _   -> failParse
