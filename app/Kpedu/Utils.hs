{-# LANGUAGE OverloadedStrings #-}
module Kpedu.Utils where

import Kpedu.State
import Kpedu.Buffer
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)

openFile :: KpeduState -> FilePath -> IO KpeduState
openFile st path = do
  b <- doesPathExist path
  content <- if b then T.readFile path
             else return ""
  let buf = Buffer ([], T.lines content ) 0
  return st { buffer = buf, filePath = path, tutorial = False, unsaved = False }

saveFile :: KpeduState -> IO KpeduState
saveFile st = do
  let path = filePath st
      (Buffer (xs,ys) _) = buffer st
      content = T.unlines (reverse xs ++ ys)
  if not . null $ path then do
    T.writeFile path content
    return st { unsaved = False }
    else return st

saveFilePath :: KpeduState -> FilePath -> IO KpeduState
saveFilePath st path = do
  let (Buffer (xs,ys) _) = buffer st
      content = T.unlines (reverse xs ++ ys)
  if not . null $ path then do
    T.writeFile path content
    return st { unsaved = False, filePath = path }
    else return st
