module Puzzle where

import Prelude hiding (sin)

import System.IO.Unsafe

import Control.Monad(foldM)
import Data.Char(isDigit, digitToInt, intToDigit)
import Data.List(intersperse)
import Data.Maybe
import Data.MemoTrie(memo)


import Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import Data.IntSet(IntSet)
import qualified Data.IntSet as IS

import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S


logm :: String -> a -> a
logm _ a = a
--logm s a = unsafePerformIO (putStr s) `seq` a

type Val = Int
type Coord = (Int,Int)

type Puzzle = Map Coord Square

type Square = Set Int

puzzCoords = [(x,y) | x<-[1..9],y<-[1..9]]

baseSquare = S.fromList [1..9]
s2l :: Square -> [Val]
s2l = S.toList
srm :: Val -> Square -> Square
srm = S.delete
sget :: Square -> Val
sget = S.findMin
slen :: Square -> Int
slen = S.size
slist :: Square -> [Val]
slist = S.toList
sin :: Val -> Square -> Bool
sin = S.member
pcv :: Puzzle -> [(Coord,Square)]
pcv = M.assocs
p2s :: Coord -> Puzzle -> Square
p2s c p = p M.! c
pins :: Coord -> Square -> Puzzle -> Puzzle
pins = M.insert
emptyPuzzle :: Puzzle
emptyPuzzle = M.fromList $ zip puzzCoords $ repeat baseSquare

readPuzzle :: String -> Maybe Puzzle
readPuzzle s0 =
    let rp0 = zip puzzCoords $ take (9*9) $ mapMaybe f s0
        rp = filter (\(c,i)->i>0) rp0
        f c | or [c == '.', c=='0'] = Just 0
            | isDigit c             = Just $ digitToInt c
            | otherwise             = Nothing
    in if length rp0 < 9*9 then Nothing
       else foldM (\p (c,d) -> assign c d p
                  ) emptyPuzzle rp

center w s = let l = length s
                 p1 = (w - l) `div` 2
                 p2 = w - l - p1
             in replicate p1 ' ' ++ s ++ replicate p2 ' '

showSquare :: Square -> String
showSquare s = map intToDigit $ s2l s

showPuzzle :: Puzzle -> String
showPuzzle p =
    let w = 1 + maximum [slen $ p2s c p | c <- puzzCoords]
    in concat $ intersperse "\n" [concat [center w $ showSquare $ p2s (x,y) p | x<-[1..9]] | y <- [1..9]]

units :: Coord -> [[Coord]]
units = memo units'
    where units' (x,y) = [ [(a + c x, b + c y)|a<-[1..3],b<-[1..3]]
                         , zip [1..9] (repeat y)
                         , zip (repeat x) [1..9]]
          c x = 3 * ((x-1) `div` 3)

peers :: Coord -> [Coord]
peers = memo peers'
    where peers' c = S.toList $ S.delete c $ S.fromList $ concat $ units c




assign :: Coord -> Val -> Puzzle -> Maybe Puzzle
assign (x,y) d p =
    ("\nassign " ++ show ((x,y),d) ++"\n") `logm`
    foldM (eliminate (x,y)) p $ s2l $ srm d $ p2s (x,y) p

eliminate :: Coord -> Puzzle -> Val -> Maybe Puzzle
eliminate c p d =
    let s  = p2s c p
    in if not $ sin d s
       then Just p
       else do
         let s' = ("\neliminating "++show (c,d)++":\n"++showPuzzle p++"\n") `logm`
                  srm d s
         let p' = pins c s' p
         p'' <- {-# SCC "p''" #-}
                case slen s' of
                  0 -> "\neliminated to emptiness\n" `logm`
                       Nothing
                  1 -> ("\neliminated "++show c++" down to one; peers: "++show (peers c)++"\n") `logm`
                       let d' = sget s'
                       in foldM (\p c' -> eliminate c' p d'
                                ) p' $ peers c
                  _ -> Just p'
         
         foldM (\p l -> {-# SCC "p'''" #-}
                        case [c | c<-l, sin d $ p2s c p] of
                          [] -> "\neliminated all of a number\n" `logm`
                                Nothing
                          (c:[]) -> "\nfound a unique in a unit\n" `logm`
                                    assign c d p
                          _ -> Just p
               ) p'' $ units c

solve :: Puzzle -> [Puzzle]
solve p =
    let sl = map (\(c,s)->(slen s, c)) $ pcv p
        (minsize,c) = minimum [(s,c)|(s,c)<-sl,s>1]
    in if 1 == maximum (map fst sl)
       then [p]
       else ("\nasdf" ++ show (minsize,c)++"\n") `logm`
            concat [(case assign c d p of
                       Nothing -> []
                       Just p' -> solve p')
                    | d <- slist $ p2s c p]

solveOnce p = listToMaybe $ solve p

verifyPuzzle :: Puzzle -> Bool
verifyPuzzle p = 
    and ([slen s == 1|(_,s)<-pcv p] ++
         [and [sget s /= sget (p2s pe p) | pe <- peers c]
          |(c,s)<-pcv p]
        )







































