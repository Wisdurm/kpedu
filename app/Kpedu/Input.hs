module Kpedu.Input where

import Kpedu.State
import Kpedu.Buffer
import Kpedu.Utils
import System.Exit
import System.Directory
import Data.List (isPrefixOf)

-- | Commands which can be autofilled
commands :: [String]
commands = ["open"
           ,"fault"
           ,"quit"
           ,"tutorial"
           ,"write"]

input :: KpeduState -> IO KpeduState
input st = do
  input <- getChar
  if cmdMode st
    then
    case input of
      '\t' -> fillCommand st
      '\n' -> runCommand st
      '\ESC' -> return st { command = "", cmdMode = False}
      '\DEL' -> return st { command = drop 1 (command st) }
      _ -> return st { command = input : (command st) }
    else
    case input of
      '\CAN' -> return st { cmdMode = True }
      '\DEL' -> return st { buffer = deleteCharBack (buffer st) }
      '\ESC' -> return st { buffer = deleteChar (buffer st) }
      '\EOT' -> return st { buffer = deleteChar (buffer st) }
      '\STX' -> return st { buffer = colBack (buffer st) }
      '\ACK' -> return st { buffer = colForward (buffer st) }
      '\DLE' -> return st { buffer = lineUp (buffer st) }
      '\SO' -> return st { buffer = lineDown (buffer st) }
      _ -> return st { buffer = insertChar (buffer st) input
                     , unsaved = True
                     , tutorial = False }

runCommand :: KpeduState -> IO KpeduState
runCommand st = do
  st <- (case reverse . command $ st of
            "fault" -> return undefined
            "q" -> exitWith ExitSuccess
            "quit" -> exitWith ExitSuccess
            "tutorial" -> return st { tutorial = True }
            'w':' ':file -> saveFilePath st file
            'w':'r':'i':'t':'e':' ':file -> saveFilePath st file
            "w" -> saveFile st
            "write" -> saveFile st
            'o':' ':file -> openFile st file
            'o':'p':'e':'n':' ':file -> openFile st file
            _ -> return st
        )
  return st{ cmdMode = False, command = "" }

fillCommand :: KpeduState -> IO KpeduState
fillCommand st = do
  case reverse . command $ st of
    'o':' ':file -> do
      p <- fillPath file
      return st { command = reverse ('o':' ' : p) }
    'o':'p':'e':'n':' ':file -> do
      p <- fillPath file
      return st { command = reverse ("open " ++ p) }
    com -> case filter (isPrefixOf com) commands of
             [] -> return st
             (x:_) -> return st { command = reverse x }

fillPath :: FilePath -> IO FilePath
fillPath path = do
  let dir = reverse (dropWhile (/='/') (reverse path))
      file = reverse (takeWhile (/='/') (reverse path))

  p <- doesPathExist dir
  suggestions <- if p then listDirectory dir
                 else do d <- getCurrentDirectory
                         listDirectory d
  if null suggestions then return path
    else
    if null file then return . (dir++) . head $ suggestions
    else do
      already <- doesPathExist path
      if already then
        -- If already have valid path, try other option
        -- (should probably loop but that requires state so eww)
        case filter (/=file) suggestions of
          [] -> return path
          (x:_) -> return . (dir++) $ x
        else
        let fill = filter (isPrefixOf file) suggestions
        in if null fill then return path
           else return . (dir++) . head $ fill
