module Kpedu.Input where

import Kpedu.State
import Kpedu.Buffer
import Kpedu.Utils
import System.Exit
import System.FilePath
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
    case bufferType st of
      TextFile buffer ->
        case input of
          '\CAN' -> return st { cmdMode = True }
          '\DEL' -> return st { bufferType = TextFile $ deleteCharBack buffer }
          '\ESC' -> return st { bufferType = TextFile $ deleteChar buffer }
          '\EOT' -> return st { bufferType = TextFile $ deleteChar buffer }
          '\STX' -> return st { bufferType = TextFile $ colBack buffer }
          '\ACK' -> return st { bufferType = TextFile $ colForward buffer }
          '\DLE' -> return st { bufferType = TextFile $ lineUp buffer }
          '\SO' -> return st { bufferType = TextFile $ lineDown buffer }
          _ -> return st { bufferType = TextFile $ insertChar buffer input
                         , unsaved = True }
      Tutorial ->
        case input of
          '\CAN' -> return st { cmdMode = True }
          _ -> return st
      Folder files ->
        case input of
          '\CAN' -> return st { cmdMode = True }
          '\DLE' -> return st { bufferType = Folder $ fszUp files }
          '\SO' -> return st { bufferType = Folder $ fszDown files }
          '\n' -> openPath st (head . snd $ files)
          _ -> return st

runCommand :: KpeduState -> IO KpeduState
runCommand st = do
  st <- (case reverse . command $ st of
            "fault" -> return undefined
            "q" -> exitWith ExitSuccess
            "quit" -> exitWith ExitSuccess
            "tutorial" -> return st { bufferType = Tutorial }
            'w':' ':file -> saveFilePath st file
            'w':'r':'i':'t':'e':' ':file -> saveFilePath st file
            "w" -> saveFile st
            "write" -> saveFile st
            'o':' ':file -> openPath st file
            'o':'p':'e':'n':' ':file -> openPath st file
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
  let dir = dropFileName path
      file = takeFileName path

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
