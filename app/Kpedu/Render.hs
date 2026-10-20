module Kpedu.Render (render) where

import Kpedu.State
import Kpedu.Buffer
import System.Console.ANSI
import Control.Monad
import qualified Data.Text as T

lineOffset :: Int
lineOffset = 2

logo :: String
logo = "\
\  █▀▓   ▒▄  █▀▀▀▀▀▀▄   ▄▀▀▀▀▀▄  █▀▀▀▀▀▀▄  █▀▒   ▓▄\n\
\▓ ░   ░ ▒ ▓ ░▀▀▀▄ ▓ ▓ ▄▀▀▀▄ ▒ ▓ ▒▀▀▀▄ ▓ ▓ ░   ░ ▓\n\
\▒ ▒  ▄▀ ░ ▒ ▒   ░ ░ ▒ ▓▄▄ ░▄░ ▒ ▓   ░ ▒ ▒ ▒   ░ ▒\n\
\▒ ▀▀▀ ▄▀  ░ ▓▒░░█▄▀ ▒ ░▒▒▌    ░ ░   ░ ░ ░ ░   ▒ ░\n\
\█ █▀▀▄ ▀▄ ▓ ░▀▀▀▀   █ ▓   ▓▀▓ █ ▒   ▓ ▒ █ █   ▓ ▓\n\
\█ █   ▒ ▒ ▒ ▒       █ ▀▄▄▄▀ ░ █ ▓▄▄▄▀ ░ █ ▀▄▄▄▀ ░\n\
\█▀    ▀▄░ ░▄▓        ▀▄▄▄▄▄▀  █▄▄▄▄▄▄▀   ▀▄▄▄▄▄▀ \n"

render :: KpeduState -> IO ()
render st = do
  sizes <- getTerminalSize
  case sizes of
    Just (h,w) -> do
      clearScreen
      lineNumbers 0 h
      modeLine h w st
      if tutorial st then tutorialScreen
        else bufferContent (buffer st)
    Nothing -> return ()

modeLine :: Int -> Int -> KpeduState -> IO ()
modeLine h w st = do
  setSGR [SetColor Background Dull White]
  setSGR [SetColor Foreground Dull Black]
  setCursorPosition (h-lineOffset) 0
  let text = if cmdMode st
             then ":\"" ++ command st ++ "\""
             else "moikka"
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
    (\line -> do setCursorColumn lineOffset
                 putStrLn . T.unpack $ line)
  forM_ (ys)
    (\line -> do setCursorColumn lineOffset
                 putStrLn . T.unpack $ line )
  -- Cursor
  setSGR [SetColor Background Dull White]
  setCursorPosition (length xs) (col+lineOffset)
  putStr " "
  setSGR [Reset]

  --print (Buffer (xs,ys) col)
  --setCursorPosition (col st) (col st + lineOffset)

tutorialScreen :: IO ()
tutorialScreen = do
  setCursorPosition 1 0
  forM_ (lines logo)
    (\line -> do setCursorColumn 5
                 putStrLn line)
  putStrLn ""
  setCursorColumn 3
  putStr "Welcome to kpedu. Move via hjkl and press x to type commands."
