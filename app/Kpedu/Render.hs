module Kpedu.Render (render) where

import Kpedu.State
import Kpedu.Buffer
import System.Console.ANSI
import Control.Monad
import qualified Data.Text as T

render :: KpeduState -> IO ()
render st = do
  sizes <- getTerminalSize
  case sizes of
    Just (h,w) -> do
      clearScreen
      lineNumbers 0 h
      modeLine h w
      bufferContent (buffer st)
    Nothing -> return ()

modeLine :: Int -> Int -> IO ()
modeLine h w = do
  setSGR [SetColor Background Dull White]
  setSGR [SetColor Foreground Dull Black]
  setCursorPosition (h-2) 0
  let text = "moikka"
      output = text ++ take (w - length text) (repeat ' ')
  putStr output
  setSGR [Reset]

lineNumbers :: Int -> Int -> IO ()
lineNumbers n h
  | n == h = return ()
  | otherwise = do
      putStrLn . show $ n
      lineNumbers (n+1) h

bufferContent :: Buffer -> IO ()
bufferContent (Buffer (xs,ys) col) = do
  setCursorPosition 0 0
  forM_ (reverse xs)
    (\line -> do setCursorColumn 2
                 putStrLn . T.unpack $ line)
  forM_ (ys)
    (\line -> do setCursorColumn 2
                 putStrLn . T.unpack $ line )
  -- Cursor
  setSGR [SetColor Background Dull White]
  setCursorPosition (length xs) (col+2)
  putStr " "
  setSGR [Reset]

  print (Buffer (xs,ys) col)
  --setCursorPosition (col st) (col st + 2)
