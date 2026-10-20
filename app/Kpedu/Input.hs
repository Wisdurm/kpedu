module Kpedu.Input where

import Kpedu.State
import Kpedu.Buffer

input :: KpeduState -> IO KpeduState
input st = do
  input <- getChar
  case input of
    'd' -> return st { buffer = deleteChar (buffer st) }
    'h' -> return st { buffer = colBack (buffer st) }
    'l' -> return st { buffer = colForward (buffer st) }
    'k' -> return st { buffer = lineUp (buffer st) }
    'j' -> return st { buffer = lineDown (buffer st) }
    _ -> return st { buffer = insertChar (buffer st) input}
