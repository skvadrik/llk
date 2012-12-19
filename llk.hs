#!/usr/bin/env runghc


import qualified Data.Set as S
import           Debug.Trace

import Grammar


trace' :: Show a => a -> a
trace' a = trace (show a) a


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


main :: IO ()
main = do
    let words = first_k 3 axiom
    print words



