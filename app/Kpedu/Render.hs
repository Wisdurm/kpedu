module Kpedu.Render where

import Kpedu.State
import Kpedu.Buffer
import System.Console.ANSI
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isPrint)

tabWidth :: Int
tabWidth = 8

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
      case bufferType st of
        TextFile content -> do
          lineNumbers content 1 h
          bufferContent content (h-2) (w - lineOffset)
        Tutorial -> tutorialScreen
        Folder files -> folderContent files (h-2)
      modeLine h w st
    Nothing -> return ()

modeLine :: Int -> Int -> KpeduState -> IO ()
modeLine h w st = do
  setSGR [SetColor Background Dull White]
  setSGR [SetColor Foreground Dull Black]
  setCursorPosition (h-2) 0
  let fileName = case filePath st of "" -> "No file open"
                                     _ -> filePath st
      content = if cmdMode st
                then ":\"" ++ (reverse . command $ st) ++ "\""
                else if unsaved st then "*" ++ fileName
                     else fileName
      text = case notice st of
        [] -> content
        msg -> content ++ " - " ++ notice st
      output = text ++ take (w - length text) (repeat ' ')
  putStr output
  setSGR [Reset]

lineNumbers :: Buffer -> Int -> Int -> IO ()
lineNumbers buf n h
  | n == (h-1) = return ()
  | otherwise = do
      let (Buffer (xs, ys) _) = buf
          ftake = if length xs + length ys < (h-2) then 0
                  else max 0 ((length xs + 1) - (h `div` 2))
      if n + ftake > length xs + length ys
        then putStrLn "~"
        else putStrLn . show $ (n + ftake)
      lineNumbers buf (n+1) h

bufferContent :: Buffer -> Int -> Col -> IO ()
bufferContent (Buffer (xs,ys) col) h w = do
  setCursorPosition 0 0
  let dropBeg = if col >= w then col - w + 1
                else 0
      cursorLine = case ys of [] -> T.empty
                              (y:_) -> T.drop dropBeg $ y

  if length xs + length ys < h then do
    forM_ (reverse xs)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . formatLine . T.unpack . T.take w . T.drop dropBeg $ line)
    forM_ (ys)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . formatLine . T.unpack . T.take w . T.drop dropBeg $ line )
    -- Cursor
    setCursorPosition (length xs) ((formatPos cursorLine col) + lineOffset)
    else do
    let ftake = min (h `div` 2) (length xs)
    forM_ (reverse . take (h `div` 2) $ xs)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . formatLine . T.unpack . T.take w . T.drop dropBeg $ line)
    forM_ (take (h - ftake)  ys)
      (\line -> do setCursorColumn lineOffset
                   putStrLn . formatLine . T.unpack . T.take w . T.drop dropBeg $ line )
    -- Cursor
    setCursorPosition ftake ((formatPos cursorLine col) + lineOffset)
  setSGR [SetColor Background Dull White]
  putStr " "
  setSGR [Reset]

  --print (Buffer (xs,ys) col)

formatLine :: String -> String
formatLine line = go line 0
  where go [] len = []
        go ('\t':line) len = let rlen = tabWidth - (len `mod` tabWidth)
                             in (take rlen  . repeat $  ' ') ++ go line (len + rlen)
        go (c:line) len = formatChar c : go line (len + (cWidth c undefined))

formatChar :: Char -> Char
formatChar c
  | isPrint c = c
  | otherwise = '?'

formatPos :: Text -> Int -> Int
formatPos line pos = T.foldl (\sum c -> sum + (cWidth c sum)) 0 (T.take pos line)

cWidth :: Char -> Int -> Int
cWidth '\t' l = tabWidth - (l `mod` tabWidth)
cWidth c _
  | isPrint c = charWidth c
  | otherwise = 1

tutorialScreen :: IO ()
tutorialScreen = do
  setCursorPosition 1 0
  forM_ (lines logo)
    (\line -> do setCursorColumn 5
                 putStrLn line)
  putStrLn ""
  let lines = ["Welcome to kpedu. Move via C-fbnp and press C-x to type commands."
              ,"You should probably be using Emacs."
              ,"Some useful commands might be:"
              ,"\topen PATH - To open files and folders"
              ,"\twrite (PATH) - To save files"
              ,"\tquit - To exit"]
  forM_ lines (\line -> setCursorColumn 1 >> putStrLn line)

folderContent :: FSZipper -> Int -> IO ()
folderContent (xs, ys) h = do
  if length xs + length ys < h then do
    forM_ (reverse xs) putStrLn
    setSGR [SetColor Background Dull White]
    putStr " "
    setSGR [Reset]
    forM_ (ys) putStrLn
    else do
    let ftake = min (h `div` 2) (length xs)
    forM_ (reverse . take (h `div` 2) $ xs) putStrLn
    setSGR [SetColor Background Dull White]
    putStr " "
    setSGR [Reset]
    forM_ (take (h - ftake) ys) putStrLn

-- | Get the designated render width of a character: 0 for a combining
-- character, 1 for a regular character, 2 for a wide character.
-- (Wide characters are rendered as exactly double width in apps and
-- fonts that support it.) (From Pandoc.)
charWidth :: Char -> Int
charWidth c
  | c < '\x0300'                     = 1
  | c >= '\x0300' && c <= '\x036F'   = 0
  |  -- combining
    c >= '\x0370' && c <= '\x10FC'   = 1
  | c >= '\x1100' && c <= '\x115F'   = 2
  | c >= '\x1160' && c <= '\x11A2'   = 1
  | c >= '\x11A3' && c <= '\x11A7'   = 2
  | c >= '\x11A8' && c <= '\x11F9'   = 1
  | c >= '\x11FA' && c <= '\x11FF'   = 2
  | c >= '\x1200' && c <= '\x2328'   = 1
  | c >= '\x2329' && c <= '\x232A'   = 2
  | c >= '\x232B' && c <= '\x2E31'   = 1
  | c >= '\x2E80' && c <= '\x303E'   = 2
  | c == '\x303F'                    = 1
  | c >= '\x3041' && c <= '\x3247'   = 2
  | c >= '\x3248' && c <= '\x324F'   = 1
  | -- ambiguous
    c >= '\x3250' && c <= '\x4DBF'   = 2
  | c >= '\x4DC0' && c <= '\x4DFF'   = 1
  | c >= '\x4E00' && c <= '\xA4C6'   = 2
  | c >= '\xA4D0' && c <= '\xA95F'   = 1
  | c >= '\xA960' && c <= '\xA97C'   = 2
  | c >= '\xA980' && c <= '\xABF9'   = 1
  | c >= '\xAC00' && c <= '\xD7FB'   = 2
  | c >= '\xD800' && c <= '\xDFFF'   = 1
  | c >= '\xE000' && c <= '\xF8FF'   = 1
  | -- ambiguous
    c >= '\xF900' && c <= '\xFAFF'   = 2
  | c >= '\xFB00' && c <= '\xFDFD'   = 1
  | c >= '\xFE00' && c <= '\xFE0F'   = 1
  | -- ambiguous
    c >= '\xFE10' && c <= '\xFE19'   = 2
  | c >= '\xFE20' && c <= '\xFE26'   = 1
  | c >= '\xFE30' && c <= '\xFE6B'   = 2
  | c >= '\xFE70' && c <= '\xFEFF'   = 1
  | c >= '\xFF01' && c <= '\xFF60'   = 2
  | c >= '\xFF61' && c <= '\x16A38'  = 1
  | c >= '\x1B000' && c <= '\x1B001' = 2
  | c >= '\x1D000' && c <= '\x1F1FF' = 1
  | c >= '\x1F200' && c <= '\x1F251' = 2
  | c >= '\x1F300' && c <= '\x1F773' = 1
  | c >= '\x20000' && c <= '\x3FFFD' = 2
  | otherwise                        = 1
