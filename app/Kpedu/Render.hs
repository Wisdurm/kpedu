module Kpedu.Render (render) where

import Kpedu.State
import System.Console.ANSI
import Control.Monad

render :: KpeduState -> IO ()
render st = do
  sizes <- getTerminalSize
  case sizes of
    Just (h,w) -> do
      clearScreen
      lineNumbers 0 h
      modeLine h w
      setCursorPosition (row st) (col st + 2)
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
