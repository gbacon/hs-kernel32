{-# LANGUAGE ForeignFunctionInterface #-}

module Win32.Kernel32 (getComputerName) where

import Control.Monad (when, unless)
import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import System.Win32.Types (DWORD, LPDWORD, LPTSTR, LPVOID,
                           peekTStringLen, peekTString, withTString)

#include <windows.h>

foreign import stdcall unsafe "GetComputerNameW"
  win32_getComputerName :: LPTSTR -> LPDWORD -> IO Bool

foreign import stdcall unsafe "GetLastError"
  win32_getLastError :: IO DWORD

foreign import stdcall unsafe "FormatMessageW"
  win32_formatMessage :: DWORD
                      -> LPVOID
                      -> DWORD
                      -> DWORD
                      -> LPTSTR
                      -> DWORD
                      -> LPVOID
                      -> IO DWORD

getComputerName :: IO String
getComputerName =
  withTString maxBuf $
    \buf ->
      alloca $ \len -> do
        poke len (fromIntegral maxLength)

        success <- win32_getComputerName buf len
        unless success $ failWithLastError "GetComputerName"

        len' <- peek len
        peekTStringLen (buf, (fromIntegral len'))
  where
    maxBuf = replicate maxLength '\0'
    maxLength = #const MAX_COMPUTERNAME_LENGTH

failWithLastError :: String -> IO a
failWithLastError name = do
  code <- win32_getLastError
  withTString errbuf $
    \buf -> do
      gotmsg <- win32_formatMessage flags
                                    nullPtr
                                    code
                                    lang
                                    buf
                                    (fromIntegral errlen)
                                    nullPtr
      fmtcode <- win32_getLastError
      when (gotmsg == 0) $
        fail $ name ++ " failed: " ++ show (code, fmtcode)

      msg <- peekTString buf
      fail $ name ++ ": " ++ filter notEOL msg
  where
    errbuf = replicate errlen '\0'
    errlen = 300
    flags = #const FORMAT_MESSAGE_FROM_SYSTEM
            .|.
            #const FORMAT_MESSAGE_IGNORE_INSERTS
    lang = 0
    notEOL c = c /= '\n' && c /= '\r'
