module PDA
    ( PDA(..)
    , Context
    , llk_table
    , restrict_to_sll1
    ) where


import qualified Data.Set            as S
import qualified Data.HashMap.Strict as M
import           Data.List                (foldl')
import           Data.Tuple               (swap)

import           Grammar
import           Types
import           LLkProperties            (first_k)


get_all_contexts :: Int -> M.HashMap Symbol Context
get_all_contexts k =
    let xs = S.foldl' (\ m t -> M.insert (T t) (S.singleton [t]) m) M.empty terminals
        ys = S.foldl' (\ m n -> M.insert (N n) (first_k k [N n]) m) xs nonterminals
    in  ys


cartesian_k :: (Ord a, Eq a) => Int -> S.Set [a] -> S.Set [a] -> S.Set [a]
cartesian_k k xs ys =
    let f zs y =
            let zs' = S.map (take k . (++ y)) xs
            in  S.union zs' zs
    in  S.foldl' f S.empty ys


llk_table :: Int -> PDA
llk_table k =
    let a = A k (get_all_contexts k) M.empty (S.singleton (axiom, S.singleton [Tlambda]))
    in  llk_table_ a


llk_table_ :: PDA -> PDA
llk_table_ a | open_states a == S.empty = a
llk_table_ a =
    let (n, ctx) = (head . S.toList) (open_states a)
        a'       = foldl' (\ a r -> resolve_state a (n, ctx) r) a (rules n)
    in  llk_table_ a'


resolve_state :: PDA -> (NonTerminal, Context) -> [Symbol] -> PDA
resolve_state (A k symb2ctx cmds open) st@(_, ctx) symbs =
    let get_contexts []             _    = error ""
        get_contexts ctxs@(ctx : _) symb =
            let ctx1 = M.lookupDefault undefined symb symb2ctx
                ctx2 = cartesian_k k ctx1 ctx
            in  ctx2 : ctxs
        update_open open (symb, ctx) = case symb of
            N n -> case M.lookup (n, ctx) cmds' of
                Just _  -> open
                Nothing -> S.insert (n, ctx) open
            _   -> open
        (ctx' : ctxs) = foldl' get_contexts [ctx] (reverse symbs)
        cmd   = zip symbs ctxs
        cmds' = M.insertWith M.union st (S.foldl' (\ m c -> M.insert c cmd m) M.empty ctx') cmds
        open' = S.delete st $ foldl' update_open open cmd
    in  A k symb2ctx cmds' open'


restrict_to_sll1 :: M.HashMap (NonTerminal, Context) (M.HashMap [Terminal] [(Symbol, Context)])
    -> (M.HashMap (NonTerminal, Terminal) Int, M.HashMap Int [Symbol])
restrict_to_sll1 commands =
    let f (tbl, cmd2id, max) (n, _) cmds = M.foldlWithKey' (g n) (tbl, cmd2id, max) cmds
        g n (tbl, cmd2id, max) [t] rs =
            let ns = (fst . unzip) rs
            in  case M.lookup ns cmd2id of
                    Just k  ->
                        let tbl' = M.insert (n, t) k tbl
                        in  (tbl', cmd2id, max)
                    Nothing ->
                        let tbl'    = M.insert (n, t) max tbl
                            cmd2id' = M.insert ns max cmd2id
                            max'    = max + 1
                        in  (tbl', cmd2id', max')
        g _ _ _ _ = error "it is not an sll(1) parsing table"
        (sll1_tbl, cmd2id, _) = M.foldlWithKey' f (M.empty, M.empty, 0) commands
        id2cmd = (M.fromList . map swap . M.toList) cmd2id
    in  (sll1_tbl, id2cmd)



