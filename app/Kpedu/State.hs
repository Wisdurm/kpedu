module Kpedu.State where

import Kpedu.Buffer

type FSZipper = ([FilePath],[FilePath])
data BufferType = TextFile Buffer | Folder FSZipper | Tutorial

data KpeduState = KpeduState {  bufferType :: BufferType
                             ,  filePath :: String
                             ,  unsaved :: Bool
                             ,  command :: String
                             ,  cmdMode :: Bool
                             ,  notice :: String
                             }

defaultState :: KpeduState
defaultState = KpeduState {  bufferType = Tutorial
                          ,  filePath = ""
                          ,  unsaved = False
                          ,  command = ""
                          ,  cmdMode = False
                          ,  notice = "Welcome"
                          }

fszUp :: FSZipper -> FSZipper
fszUp (x:xs,ys) = ((xs), x:ys)
fszUp ([],ys) = ([], ys)

fszDown :: FSZipper -> FSZipper
fszDown (xs,y:ys) = ((y:xs), ys)
fszDown (xs,[]) = (xs, [])
