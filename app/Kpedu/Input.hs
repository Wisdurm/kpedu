module Kpedu.Input where

import Kpedu.State

input :: KpeduState -> IO KpeduState
input st = do
  input <- getChar
  case input of
    'h' -> return st { col = max 0 (col st - 1) }
    'l' -> return st { col = col st + 1 }
    'j' -> return st { row = row st + 1 }
    'k' -> return st { row = max 0 (row st - 1) }
    _ -> return st
