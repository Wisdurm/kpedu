module Kpedu.State where

data KpeduState = KpeduState {  row :: Int
                             ,  col :: Int
                             }

defaultState :: KpeduState
defaultState = KpeduState {  row = 0
                          ,  col = 0
                          }
