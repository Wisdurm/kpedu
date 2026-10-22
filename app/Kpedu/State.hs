module Kpedu.State where

import Kpedu.Buffer

data KpeduState = KpeduState {  buffer :: Buffer
                             ,  filePath :: String
                             ,  unsaved :: Bool
                             ,  command :: String
                             ,  cmdMode :: Bool
                             ,  tutorial :: Bool
                             }

defaultState :: KpeduState
defaultState = KpeduState {  buffer = initBuffer "Hello\nWorld!"
                          ,  filePath = ""
                          ,  unsaved = False
                          ,  command = ""
                          ,  cmdMode = False
                          ,  tutorial = True
                          }
