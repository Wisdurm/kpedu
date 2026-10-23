{-# LANGUAGE OverloadedStrings #-}
module Kpedu.Utils where

import Kpedu.State
import Kpedu.Buffer
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)

openPath :: KpeduState -> FilePath -> IO KpeduState
openPath st path = do
  dirP <- doesDirectoryExist path
  if dirP then do
    content <- getDirectoryContents path
    fullDir <- makeAbsolute path
    setCurrentDirectory path
    return st { bufferType = Folder ([], content)
              , filePath = fullDir}
    else do
    b <- doesFileExist path
    p <- makeAbsolute . dropFileName $ path
    setCurrentDirectory p
    content <- if b then T.readFile path
               else return ""
    let buf = Buffer ([], T.lines content ) 0
    return st { bufferType = TextFile buf
              , filePath = path
              , unsaved = False }

saveFile :: KpeduState -> IO KpeduState
saveFile st = do
  let path = filePath st
      TextFile (Buffer (xs,ys) _) = bufferType st
      content = T.unlines (reverse xs ++ ys)
  if not . null $ path then do
    T.writeFile path content
    return st { unsaved = False }
    else return st

saveFilePath :: KpeduState -> FilePath -> IO KpeduState
saveFilePath st path = do
  let TextFile (Buffer (xs,ys) _) = bufferType st
      content = T.unlines (reverse xs ++ ys)
  if not . null $ path then do
    T.writeFile path content
    return st { unsaved = False, filePath = path }
    else return st
