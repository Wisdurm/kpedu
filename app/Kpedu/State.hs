module Kpedu.State where

import Kpedu.Buffer

data KpeduState = KpeduState {  buffer :: Buffer
                             }

defaultState :: KpeduState
defaultState = KpeduState {  buffer = initBuffer "Hello\nWorld!"
                          }
