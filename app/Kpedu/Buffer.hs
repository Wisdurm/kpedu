{-# LANGUAGE OverloadedStrings #-}
module Kpedu.Buffer where

import Data.Text (Text)
import qualified Data.Text as T

-- Definitely isn't the most efficient way to do this, but who cares.

type Col = Int
data Buffer = Buffer ([Text], [Text]) Col
  deriving (Show)

initBuffer :: String -> Buffer
initBuffer t = Buffer ([], T.lines . T.pack $ t) 0

colForward :: Buffer -> Buffer
colForward (Buffer p r) = Buffer p (r+1)

colBack :: Buffer -> Buffer
colBack (Buffer p r) = Buffer p (max 0 (r-1))

lineDown :: Buffer -> Buffer
lineDown (Buffer (ctx, xs) r) =
  Buffer ((take 1 xs ++ ctx), (drop 1 xs)) r

lineUp :: Buffer -> Buffer
lineUp (Buffer (ctx, xs) r) =
  Buffer ((drop 1 ctx), (take 1 ctx ++ xs)) r

insertChar :: Buffer -> Char -> Buffer
insertChar buf '\n' =
  let (Buffer (ctx, (xs)) r) = lineDown buf
  in Buffer (ctx, T.empty:xs) r
insertChar (Buffer (ctx, []) r) c = Buffer (ctx, [T.singleton c]) r
insertChar (Buffer (ctx, (x:xs)) r) c =
  Buffer (ctx, ((T.snoc (T.take r x) c) `T.append` T.drop r x) : xs) (r+1)

deleteChar :: Buffer -> Buffer
deleteChar (Buffer (ctx, []) r) = Buffer (ctx, []) r
deleteChar (Buffer (ctx, ("":xs)) r) = Buffer (ctx, xs) r
deleteChar (Buffer (ctx, (x:xs)) r) =
  Buffer (ctx, ((T.take r x) `T.append` T.drop (r+1) x) : xs) r
