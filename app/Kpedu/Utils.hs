module Kpedu.Utils where

import Kpedu.State
import Kpedu.Buffer
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

openFile :: KpeduState -> FilePath -> IO KpeduState
openFile st path = do
  b <- doesPathExist path
  if b then do
    content <- T.readFile path
    let buf = Buffer ([], T.lines content ) 0
    return st { buffer = buf, filePath = path, tutorial = False }
    else return st
