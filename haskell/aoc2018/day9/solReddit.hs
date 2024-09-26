module Main where

import qualified Data.Sequence as S
import Data.Sequence (Seq(..), ViewL(..), (<|))
import qualified Data.IntMap.Strict as M
import Data.List (foldl')

rotate :: Seq a -> Int -> Seq a
rotate Empty _ = S.empty
rotate s     n = let (l, r) = S.splitAt (n `mod` S.length s) s in r <> l

play :: Int -> Int -> Int
play numPlayers numMarbles =
  let players = cycle $ [1..numPlayers]
      marbles = [1..numMarbles]
  in  maximum . fst $
      foldl' go (M.singleton 0 0, S.singleton 0) $ zip players marbles
  where
    go (scores, board) (player, marble) =
      if marble `mod` 23 == 0
      then let
        (val :< newBoard) = S.viewl $ rotate board (-7)
        newScores         = M.insertWith (+) player (marble + val) scores
      in
        (newScores, newBoard)
      else
        (scores, (marble <| rotate board 2))

main :: IO ()
main = do
  print $ play 471 72026
  print $ play 471 (72026 * 100)
