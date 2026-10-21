module Kpedu.State where

import Kpedu.Buffer

data KpeduState = KpeduState {  buffer :: Buffer
                             ,  filePath :: String
                             ,  command :: String
                             ,  cmdMode :: Bool
                             ,  tutorial :: Bool
                             }

defaultState :: KpeduState
defaultState = KpeduState {  buffer = initBuffer "Hello\nWorld!"
                          ,  filePath = ""
                          ,  command = ""
                          ,  cmdMode = False
                          ,  tutorial = True
                          }
