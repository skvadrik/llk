import qualified Data.Set            as S
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.List                (foldl', delete)
import           Debug.Trace

import Grammar


type Context   = S.Set [Terminal]
data Automaton = A
    { order         :: Int
    , symbol2contex :: M.HashMap Symbol Context
    , commands      :: M.HashMap (NonTerminal, Context) (M.HashMap Context [(Symbol, Context)])
    , open_states   :: S.Set (NonTerminal, Context)
    } deriving (Show)


hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0


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


find_order :: Int
find_order =
    let try k
            | is_ll k    = k
            | otherwise  = try (k + 1)
    in  try 1


get_all_contexts :: Int -> M.HashMap Symbol Context
get_all_contexts k =
    let xs = S.foldl' (\ m t -> M.insert (T t) (S.singleton [t]) m) M.empty terminals
        ys = S.foldl' (\ m n -> M.insert (N n) (first_k_ k [N n]) m) xs nonterminals
    in  ys


cartesian_k :: (Ord a, Eq a) => Int -> S.Set [a] -> S.Set [a] -> S.Set [a]
cartesian_k k xs ys =
    let f zs y =
            let zs' = S.map (take k . (++ y)) xs
            in  S.union zs' zs
    in  S.foldl' f S.empty ys


llk_table :: Int -> Automaton
llk_table k =
    let a = A k (get_all_contexts k) M.empty (S.singleton (axiom, S.singleton [Tlambda]))
    in  llk_table_ a


llk_table_ :: Automaton -> Automaton
llk_table_ a | open_states a == S.empty = a
llk_table_ a =
    let (n, ctx) = (head . S.toList) (open_states a)
        a'       = foldl' (\ a r -> step a (n, ctx) r) a (rules n)
    in  llk_table_ a'


step :: Automaton -> (NonTerminal, Context) -> [Symbol] -> Automaton
step (A k symb2ctx cmds open) (n, ctx) symbs =
    let f ctxs@(ctx : _) symb =
            let ctx1 = M.lookupDefault undefined symb symb2ctx
                ctx2 = cartesian_k k ctx1 ctx
            in  ctx2 : ctxs
        f [] _ = undefined
        g open (symb, ctx) = case symb of
            N n -> case M.lookup (n, ctx) cmds' of
                Just _  -> open
                Nothing -> S.insert (n, ctx) open
            _   -> open
        (ctx' : ctxs) = foldl' f [ctx] (reverse symbs)
        cmd   = zip symbs ctxs
        cmds' = M.insertWith M.union (n, ctx) (M.insert ctx' cmd M.empty) cmds
        open' = S.delete (n, ctx) $ foldl' g open cmd
    in  A k symb2ctx cmds' open'


main :: IO ()
main = do
    let k = find_order
        t = llk_table k

    print $ "This grammar is LL-" ++ show k
    print $ is_sll k
    print t



