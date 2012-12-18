#!/usr/bin/env runghc


import qualified Data.Set as S
import Debug.Trace


trace' :: Show a => a -> a
trace' a = trace (show a) a


{-
E = TR
T = lambda | +TR | -TR
R = a | i | (E)
-}


data Terminal    = Ta | Ti | TOParens | TCParens | TPlus | TMinus deriving (Show, Ord, Eq)
data NonTerminal = NE | NT | NR deriving (Show)
data Symbol      = T Terminal | N NonTerminal deriving (Show)
type Rule        = NonTerminal -> [[Symbol]]


rules :: Rule
rules NE = [[N NT, N NR]]
rules NT = [[], [T TPlus, N NT, N NR], [T TMinus, N NT, N NR]]
rules NR = [[T Ta], [T Ti], [T TOParens, N NE, T TCParens]]


is_terminal :: Symbol -> Bool
is_terminal x = case x of
    N nt -> False
    T  t -> True


from_n :: Symbol -> NonTerminal
from_n x = case x of
    N nt -> nt
    T _  -> error "trying to convert terminal symbol to non-terminal"


from_t :: Symbol -> Terminal
from_t x = case x of
    T t -> t
    N _ -> error "trying to convert non-terminal symbol to terminal"


{-
-- this won't work
produce_ :: [Symbol] -> S.Set [Terminal]
produce_ xs =
    let (xs1, xs2) = trace' $ span is_terminal xs
    in  case xs2 of
            []      -> (S.singleton . map from_t) xs
            x : xs3 ->
                let nt  = from_n x
                    yss = rules nt
                    zss = map (\ ys -> xs1 ++ ys ++ xs3) yss
                in  S.unions $ map produce_ zss


produce :: NonTerminal -> S.Set [Terminal]
produce nt = produce_ [N nt]
-}


first_k_ :: Int -> [Symbol] -> S.Set [Terminal]
first_k_ k xs =
    let (xs1, xs2) = span is_terminal xs
    in  if length xs1 >= k || null xs2
            then (S.singleton . map from_t . take k) xs1
            else
                let x : xs3 = xs2
                    nt  = from_n x
                    yss = rules nt
                    zss = map (\ ys -> xs1 ++ ys ++ xs3) yss
                in  S.unions $ map (first_k_ k) zss


first_k :: Int -> NonTerminal -> S.Set [Terminal]
first_k k nt = first_k_ k [N nt]


produce1 :: NonTerminal -> [[Terminal]]
produce1 nt = produce1_ [N nt]


produce1_ :: [Symbol] -> [[Terminal]]
produce1_ xs =
    let (xs1, xs2) = trace' $ span is_terminal xs
    in  case xs2 of
            []      -> [map from_t xs]
            x : xs3 ->
                let nt  = from_n x
                    yss = rules nt
                    zss = map (\ ys -> xs1 ++ ys ++ xs3) yss
                in  concatMap produce1_ zss


main :: IO ()
main = do
    let words = first_k 3 NE
        words1 = take 10 $ produce1 NE
    print words



