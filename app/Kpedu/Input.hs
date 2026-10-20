module Kpedu.Input where

import Kpedu.State
import Kpedu.Buffer

input :: KpeduState -> IO KpeduState
input st = do
  input <- getChar
  if cmdMode st
    then
    case input of
      '\n' -> runCommand st
      _ -> return st { command = command st ++ [input] }
    else
    case input of
      'x' -> return st { cmdMode = True }
      'd' -> return st { buffer = deleteChar (buffer st) }
      'h' -> return st { buffer = colBack (buffer st) }
      'l' -> return st { buffer = colForward (buffer st) }
      'k' -> return st { buffer = lineUp (buffer st) }
      'j' -> return st { buffer = lineDown (buffer st) }
      _ -> return st { buffer = insertChar (buffer st) input
                     , tutorial = False }

runCommand :: KpeduState -> IO KpeduState
runCommand st = do
  return (case command st of
             "fault" -> undefined
             "tutorial" -> st { tutorial = True }
             _ -> st
             )
    { cmdMode = False, command = "" }
