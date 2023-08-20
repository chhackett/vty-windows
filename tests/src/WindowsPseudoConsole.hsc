{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module WindowsPseudoConsole 
  ( openPseudoTerminal
  )
where

#include <fcntl.h>
#include <windows.h>
#include "windows_cconv.h"

import GHC.Ptr ( Ptr, castPtr )
import Foreign.C.Types ( CInt(..) )
import Foreign.C.String ( withCStringLen )
import System.IO (Handle, IOMode(..), hPutBufNonBlocking, openFile, hClose)
import System.Win32.Types ( HANDLE )
import Foreign.Marshal.Alloc (alloca)
import Foreign (peek)

-- Functions for tests...
-- writeString :: NativeHandle -> String -> IO ByteCount
-- writeString handle output = withCStringLen output $
--     \ (buf, len) -> hPutBufNonBlocking handle (castPtr buf) len

-- closeHandle :: NativeHandle -> IO ()
-- closeHandle = hClose

-- Windows equivalent to /dev/null is \\.\NUL apparently
-- openHandleToNull :: IO NativeHandle
-- openHandleToNull = openFile "NUL" WriteMode

foreign import ccall "create_pseudo_console" cCreatePseudoConsole :: CInt -> CInt -> HANDLE -> HANDLE -> IO CInt

-- See this post for details on how to create a pseudo console session on windows
-- https://docs.microsoft.com/en-us/windows/console/creating-a-pseudoconsole-session
openPseudoTerminal :: IO (Handle, Handle)
openPseudoTerminal = undefined
  -- do
  -- handles <- createPseudoConsole
  -- return (outputRead handles, inputWrite handles)

createPseudoConsole :: IO (HANDLE, HANDLE)
createPseudoConsole = undefined
  -- alloca $ \ptr1 -> do
  --   alloca $ \ptr2 -> do
  --     cCreatePseudoConsole 80 32 ptr1 ptr2
  --     peek ptr2)
  -- where
  --   callWithHandle :: Ptr HANDLE -> IO (HANDLE, HANDLE)
  --   callWithHandle handle =
