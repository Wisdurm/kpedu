module Main where

import Kpedu.Render
import Kpedu.Input
import Kpedu.State
import Kpedu.Utils as Utils (openPath)
import System.Environment
import System.Console.ANSI
import System.IO
import Control.Exception

main :: IO ()
main = do
  args <- getArgs
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI then do
    setTitle "Kpedu"
    disableLineWrap
    hideCursor
    useAlternateScreenBuffer
    st <- case args of [] -> return defaultState
                       (file:_) -> Utils.openPath defaultState file
    loop st `catch`
      (\(_ :: SomeException) -> useNormalScreenBuffer >> showCursor >>
                                enableLineWrap)
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
