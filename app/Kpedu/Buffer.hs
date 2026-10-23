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
colForward (Buffer (xs,ys) r) = let len = case ys of (y:_) -> T.length y
                                                     [] -> 0
                                in Buffer (xs,ys) (min (r+1) len)

colBack :: Buffer -> Buffer
colBack (Buffer p r) = Buffer p (max 0 (r-1))

lineDown :: Buffer -> Buffer
lineDown (Buffer (ctx, xs) r) =
  let len = case drop 1 xs of (x:_) -> T.length x
                              [] -> 0
  in Buffer ((take 1 xs ++ ctx), (drop 1 xs)) (min r len)

lineUp :: Buffer -> Buffer
lineUp (Buffer (ctx, xs) r) =
  let len = case take 1 ctx of (x:_) -> T.length x
                               [] -> 0
  in Buffer ((drop 1 ctx), (take 1 ctx ++ xs)) (min r len)

insertChar :: Buffer -> Char -> Buffer
insertChar (Buffer (ctx, (x:xs)) r) '\n' =
  let lineBeg = T.take r x
      lineEnd = T.drop r x
  in Buffer (lineBeg : ctx, lineEnd:xs) 0
insertChar (Buffer (ctx, []) r) c = Buffer (ctx, [T.singleton c]) r
insertChar (Buffer (ctx, (x:xs)) r) c =
  Buffer (ctx, ((T.snoc (T.take r x) c) `T.append` T.drop r x) : xs) (r+1)

deleteChar :: Buffer -> Buffer
deleteChar (Buffer (ctx, []) r) = Buffer (ctx, []) r
deleteChar (Buffer (ctx, ("":xs)) r) = Buffer (ctx, xs) r
deleteChar (Buffer (ctx, (x:xs)) r)
  | r >= T.length x && (not . null $ xs) =
    Buffer (ctx, x `T.append` head xs : tail xs) r
  | otherwise = Buffer (ctx, ((T.take r x) `T.append` T.drop (r+1) x) : xs) r

deleteCharBack :: Buffer -> Buffer
deleteCharBack (Buffer (ctx, []) r) = Buffer (ctx, []) r
deleteCharBack (Buffer (ctx, ("":xs)) r) = Buffer (ctx, xs) r
deleteCharBack (Buffer ((c:ctx), (x:xs)) 0) =
  let (Buffer (ctx2, x2:xs2) _) = lineUp (Buffer (c `T.append` x : ctx, xs) 0)
  in Buffer (ctx2, (x2:xs2)) (T.length c)
deleteCharBack (Buffer (ctx, (x:xs)) r) =
  Buffer (ctx, ((T.take (r-1) x) `T.append` T.drop r x) : xs) (max 0 (r-1))
