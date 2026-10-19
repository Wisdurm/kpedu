module Main where

import Kpedu.Render
import Kpedu.Input
import Kpedu.State
import System.Console.ANSI
import System.IO
import Control.Exception

main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI then do
    setTitle "Kpedu"
    useAlternateScreenBuffer
    loop defaultState `catch`
      (\(_ :: SomeException) -> useNormalScreenBuffer)
    else
    putStrLn "Cout does not support ANSI escape codes."

loop :: KpeduState -> IO ()
loop st = do
  hSetBuffering stdin LineBuffering
  render st
  hFlush stdout
  hSetBuffering stdin NoBuffering
  nst <- input st
  loop nst
