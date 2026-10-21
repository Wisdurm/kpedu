module Kpedu.Input where

import Kpedu.State
import Kpedu.Buffer
import Kpedu.Utils

input :: KpeduState -> IO KpeduState
input st = do
  input <- getChar
  if cmdMode st
    then
    case input of
      '\n' -> runCommand st
      '\ESC' -> return st { command = "", cmdMode = False}
      '\DEL' -> return st { command = drop 1 (command st) }
      _ -> return st { command = input : (command st) }
    else
    case input of
      'x' -> return st { cmdMode = True }
      '\DEL' -> return st { buffer = deleteChar (buffer st) }
      'd' -> return st { buffer = deleteChar (buffer st) }
      'h' -> return st { buffer = colBack (buffer st) }
      'l' -> return st { buffer = colForward (buffer st) }
      'k' -> return st { buffer = lineUp (buffer st) }
      'j' -> return st { buffer = lineDown (buffer st) }
      _ -> return st { buffer = insertChar (buffer st) input
                     , tutorial = False }

runCommand :: KpeduState -> IO KpeduState
runCommand st = do
  st <- (case reverse . command $ st of
            "fault" -> return undefined
            "tutorial" -> return st { tutorial = True }
            'o':'p':'e':'n':' ':file -> openFile st file
            _ -> return st
        )
  return st{ cmdMode = False, command = "" }
