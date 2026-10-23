module Kpedu.Input where

import Kpedu.State
import Kpedu.Buffer
import Kpedu.Utils
import System.Exit

input :: KpeduState -> IO KpeduState
input st = do
  input <- getChar
  if cmdMode st
    then
    case input of
      '\t' -> return st { command = "moi" }
      '\n' -> runCommand st
      '\ESC' -> return st { command = "", cmdMode = False}
      '\DEL' -> return st { command = drop 1 (command st) }
      _ -> return st { command = input : (command st) }
    else
    case input of
      '\CAN' -> return st { cmdMode = True }
      '\DEL' -> return st { buffer = deleteChar (buffer st) }
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
