import qualified Data.Set     as S
import           Data.List         (foldl', delete)
import           Debug.Trace

import Grammar


trace' :: Show a => a -> a
trace' a = trace (show a) a


is_terminal :: Symbol -> Bool
is_terminal x = case x of
    N _ -> False
    T _ -> True


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

{-
first_k :: Int -> NonTerminal -> S.Set [Terminal]
first_k k n = first_k_ k [N n]


follow_k :: Int -> NonTerminal -> S.Set [Terminal]
follow_k k n =
    let rs1 = S.foldl' (\ rs n -> rs ++ rules n) [] nonterminals
        f r =
            let r1 = dropWhile is_terminal r
            in  case r1 of
                    N n1 : _ | n1 == n -> [r1]
                    _                  -> []
        rs2 = foldl' (\ rs r -> f r ++ rs) [] rs1
        rs3 = S.unions $ map (first_k_ k) rs2
    in  rs3
-}

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
                xs = S.unions $ map (\ ctx -> first_k_ k (r1 ++ ctx)) rcontexts
                ys = S.unions $ map (\ ctx -> first_k_ k (r2 ++ ctx)) rcontexts
            in  S.intersection xs ys == S.empty
    in  foldl' (\ b1 r1 -> foldl' (\ b2 r2 -> b2 && f r1 r2) b1 (delete r1 rs)) True rs


is_sll :: Int -> Bool
is_sll k = S.foldl' (\ b n -> b && is_sll_ k n) True nonterminals


is_ll_ :: Int -> NonTerminal -> Bool
is_ll_ k n =
    let rs = rules n
        g r1 r2 ch =
            let xs = first_k_ k (r1 ++ ch)
                ys = first_k_ k (r2 ++ ch)
            in  S.intersection xs ys == S.empty
        f r1 r2 =
            let rcontexts = right_contexts n
            in  foldl' (\ b ctx -> b && g r1 r2 ctx) True rcontexts
    in  foldl' (\ b1 r1 -> foldl' (\ b2 r2 -> b2 && f r1 r2) b1 (delete r1 rs)) True rs


is_ll :: Int -> Bool
is_ll k = S.foldl' (\ b n -> b && is_ll_ k n) True nonterminals


find_k :: Int
find_k =
    let try k
            | is_ll k    = k
            | otherwise  = try (k + 1)
    in  try 1


main :: IO ()
main = do
    let k = find_k
    print $ "This grammar is LL-" ++ show k
    print $ is_sll k



