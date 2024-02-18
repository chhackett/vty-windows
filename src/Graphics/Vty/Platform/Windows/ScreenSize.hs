{-# LANGUAGE CPP #-}

module Graphics.Vty.Platform.Windows.ScreenSize
  (getMinttyScreenSize, isScreenSizeEvent, classifyScreenSize) where

import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)
import Graphics.Vty.Platform.Windows.Input.Classify.Types
    ( KClass(..) )
import Graphics.Vty.Platform.Windows.Input.Classify.Parse
import Graphics.Vty.Input.Events (InternalEvent(..), Event(..))
import Graphics.Vty.Output (Output(..))
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as P
-- import GHC.IO.Handle.FD
-- import GHC.IO.FD
-- import System.IO (Handle)
-- import Foreign.C.Types (CInt(..), CLong(..))


-- foreign import ccall "gwinsz.h vty_c_get_window_size" c_getWindowSize :: Fd -> IO CLong

-- getWindowSize :: Fd -> IO (Int,Int)
-- getWindowSize fd = do
--     (a,b) <- (`divMod` 65536) `fmap` c_getWindowSize fd
--     return (fromIntegral b, fromIntegral a)

-- foreign import ccall "gwinsz.h vty_c_set_window_size" c_setWindowSize :: Fd -> CLong -> IO ()

-- setWindowSize :: Fd -> (Int, Int) -> IO ()
-- setWindowSize fd (w, h) = do
--     let val = (h `shiftL` 16) + w
--     c_setWindowSize fd $ fromIntegral val


isScreenSizeEvent :: ByteString -> Bool 
isScreenSizeEvent bytes = BS8.isPrefixOf csiPrefix bytes && BS8.isSuffixOf (BS8.pack "t") bytes

csiPrefix :: ByteString
csiPrefix = BS8.pack "\ESC["

-- | Attempt to classify an input string as a screen size event.
classifyScreenSize :: ByteString -> KClass
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

getMinttyScreenSize :: (ByteString -> IO()) -> IO ()
getMinttyScreenSize writeOutput = do
  appendFile "C:\\temp\\debug.log" $ "getScreenSize...\n"
  let buf = BS8.pack "\ESC[18t"
  writeOutput buf
  -- return (100, 100)
  -- internalEvent <- atomically $ readTChan inputChannel
  -- case internalEvent of
  --   InputEvent evt ->
  --     case evt of
  --       EvResize x y -> return (x, y)
  --       e            -> error $ "unexpected event received: " ++ show e
  --   _     -> error "unexpected resume event received"

parseScreenSizeEvent :: ByteString -> Maybe (Int, Int)
parseScreenSizeEvent bytes =
  case (P.parse screenSizeParser "" bytes) of
            Left err  -> error $ show err
            Right res  -> Just res

screenSizeParser :: P.Parser (Int, Int)
screenSizeParser = do
    void $ P.char '\ESC'
    void $ P.char '['
    rows <- numDefaultOne
    void $ P.char ';'
    cols <- numDefaultOne
    void $ P.char 't'
    return (rows, cols)

numDefaultOne :: P.Parser Int
numDefaultOne = do
    n <- P.many P.digit
    case length n of
      0 -> return 1
      _ -> return $ read n

-- Options to get screen size in mintty terminals:
-- Send cap expression '\ESC[18t', parse the response with format '\ESC[YY;XXt'
-- Set cursor position to 999,999, then send 'report cursor' sequence and parse the response with format '\ESC[YY;XXR'
-- Use ioctl interface to read the TIOCGWINSZ structure.
