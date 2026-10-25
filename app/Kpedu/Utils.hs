{-# LANGUAGE OverloadedStrings #-}
module Kpedu.Utils where

import Kpedu.State
import Kpedu.Buffer
import System.Directory
import System.FilePath
import System.IO.Error
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)
import Control.Exception
import Control.Monad

openPath :: KpeduState -> FilePath -> IO KpeduState
openPath st path = do
  dirP <- doesDirectoryExist path
  if dirP then do
    r <- tryJust (guard . isPermissionError) $ getDirectoryContents path
    case r of
      Left ex -> return st { notice = "Not allowed to open folder" }
      Right content -> do
        fullDir <- makeAbsolute path
        setCurrentDirectory path
        return st { bufferType = Folder ([], content)
                  , filePath = fullDir
                  , notice = ""}
    else do
    b <- doesFileExist path
    p <- makeAbsolute . dropFileName $ path
    setCurrentDirectory p
    r <- try $ (if b then T.readFile path
                else return "") :: IO (Either IOError T.Text)
    case r of
      Left _ -> return st { notice = "Can't open file" }
      Right content -> do
        let buf = Buffer ([], T.lines content ) 0
        return st { bufferType = TextFile buf
                  , filePath = path
                  , unsaved = False
                  , notice = "" }

saveFile :: KpeduState -> IO KpeduState
saveFile st = do
  let path = filePath st
      TextFile (Buffer (xs,ys) _) = bufferType st
      content = T.unlines (reverse xs ++ ys)
  if not . null $ path then do
    T.writeFile path content
    return st { unsaved = False
              , notice = "" }
    else return st

saveFilePath :: KpeduState -> FilePath -> IO KpeduState
saveFilePath st path = do
  let TextFile (Buffer (xs,ys) _) = bufferType st
      content = T.unlines (reverse xs ++ ys)
  if not . null $ path then do
    T.writeFile path content
    return st { unsaved = False
              , filePath = path
              , notice = ""}
    else return st
