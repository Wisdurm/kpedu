module Kpedu.State where

import Kpedu.Buffer

data KpeduState = KpeduState {  buffer :: Buffer
                             ,  command :: String
                             ,  cmdMode :: Bool
                             ,  tutorial :: Bool
                             }

defaultState :: KpeduState
defaultState = KpeduState {  buffer = initBuffer "Hello\nWorld!"
                          ,  command = ""
                          ,  cmdMode = False
                          ,  tutorial = True
                          }
