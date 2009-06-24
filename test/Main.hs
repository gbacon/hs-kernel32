module Main where

import Win32.Kernel32 (getComputerName)

main :: IO ()
main = getComputerName >>= putStrLn
