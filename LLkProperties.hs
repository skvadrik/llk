module LLkProperties
    ( first_k
    , find_order
    , is_ll
    , is_sll
    ) where


import qualified Data.Set    as S
import           Data.List        (foldl', delete)

import           Grammar


is_terminal :: Symbol -> Bool
is_terminal x = case x of
    N _ -> False
    T _ -> True


s2n :: Symbol -> NonTerminal
s2n x = case x of
    N nt -> nt
    T _  -> error "trying to convert terminal symbol to non-terminal"


s2t :: Symbol -> Terminal
s2t x = case x of
    T t -> t
    N _ -> error "trying to convert non-terminal symbol to terminal"


first_k :: Int -> [Symbol] -> S.Set [Terminal]
first_k k xs =
    let (xs1, xs2) = span is_terminal xs
    in  if length xs1 >= k || null xs2
            then (S.singleton . map s2t . take k) xs1
            else
                let x : xs3 = xs2
                    nt  = s2n x
                    yss = rules nt
                    zss = map (\ ys -> xs1 ++ ys ++ xs3) yss
                in  S.unions $ map (first_k k) zss


right_contexts :: NonTerminal -> [[Symbol]]
right_contexts n =
    let rs = S.foldl' (\ rs n -> rs ++ rules n) [] nonterminals
        f r = case dropWhile is_terminal r of
            N n1 : r1 | n1 == n -> [r1]
            _                   -> []
    in  foldl' (\ rs1 r -> f r ++ rs1) [] rs


is_sll_ :: Int -> NonTerminal -> Bool
is_sll_ k n =
    let rs = rules n
        f r1 r2 =
            let rcontexts = right_contexts n
                xs = S.unions $ map (\ ctx -> first_k k (r1 ++ ctx)) rcontexts
                ys = S.unions $ map (\ ctx -> first_k k (r2 ++ ctx)) rcontexts
            in  S.intersection xs ys == S.empty
    in  foldl' (\ b1 r1 -> foldl' (\ b2 r2 -> b2 && f r1 r2) b1 (delete r1 rs)) True rs


is_sll :: Int -> Bool
is_sll k = S.foldl' (\ b n -> b && is_sll_ k n) True nonterminals


is_ll_ :: Int -> NonTerminal -> Bool
is_ll_ k n =
    let rs = rules n
        g r1 r2 ch =
            let xs = first_k k (r1 ++ ch)
                ys = first_k k (r2 ++ ch)
            in  S.intersection xs ys == S.empty
        f r1 r2 =
            let rcontexts = right_contexts n
            in  foldl' (\ b ctx -> b && g r1 r2 ctx) True rcontexts
    in  foldl' (\ b1 r1 -> foldl' (\ b2 r2 -> b2 && f r1 r2) b1 (delete r1 rs)) True rs


is_ll :: Int -> Bool
is_ll k = S.foldl' (\ b n -> b && is_ll_ k n) True nonterminals


find_order :: Int
find_order =
    let try k
            | is_ll k    = k
            | otherwise  = try (k + 1)
    in  try 1



