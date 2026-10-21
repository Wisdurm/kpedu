module Kpedu.Render (render) where

import Kpedu.State
import Kpedu.Buffer
import System.Console.ANSI
import Control.Monad
import qualified Data.Text as T

lineOffset :: Int
lineOffset = 3

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
      setCursorPosition 0 0
      lineNumbers (buffer st) 1 h
      modeLine h w st
      if tutorial st then tutorialScreen
        else bufferContent (buffer st) (h-2)
    Nothing -> return ()

modeLine :: Int -> Int -> KpeduState -> IO ()
modeLine h w st = do
  setSGR [SetColor Background Dull White]
  setSGR [SetColor Foreground Dull Black]
  setCursorPosition (h-2) 0
  let text = if cmdMode st
             then ":\"" ++ (reverse . command $ st) ++ "\""
             else filePath st
      output = text ++ take (w - length text) (repeat ' ')
  putStr output
  setSGR [Reset]

lineNumbers :: Buffer -> Int -> Int -> IO ()
lineNumbers buf n h
  | n == (h-1) = return ()
  | otherwise = do
      let (Buffer (xs, ys) _) = buf
          ftake = if length xs + length ys < h then 0
                  else max 0 ((length xs + 1) - (h `div` 2))
      putStrLn . show $ (n + ftake)
      lineNumbers buf (n+1) h

bufferContent :: Buffer -> Int -> IO ()
bufferContent (Buffer (xs,ys) col) h = do
  setCursorPosition 0 0
  if length xs + length ys < h then do
    forM_ (reverse xs)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . T.unpack $ line)
    forM_ (ys)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . T.unpack $ line )
    -- Cursor
    setCursorPosition (length xs) (col+lineOffset)
    else do
    let ftake = min (h `div` 2) (length xs)
    forM_ (reverse . take (h `div` 2) $ xs)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . T.unpack $ line)
    forM_ (take (h - ftake)  ys)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . T.unpack $ line )
    -- Cursor
    setCursorPosition ftake (col+lineOffset)
  setSGR [SetColor Background Dull White]
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
