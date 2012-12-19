#!/usr/bin/env runghc


import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Char    (isSpace, isAlphaNum)
import           Debug.Trace


trace' :: Show a => a -> a
trace' a = trace (show a) a


data Grammar     = G (S.Set Terminal) (S.Set NonTerminal) (M.Map NonTerminal (S.Set Rule)) NonTerminal deriving (Show)
data Terminal    = T String deriving (Show, Ord, Eq)
data NonTerminal = N String deriving (Show, Ord, Eq)
data Symbol      = ST Terminal | SN NonTerminal deriving (Show, Ord, Eq)
data Rule        = R [Symbol] deriving (Show, Ord, Eq)


check_name :: String -> String
check_name s = if filter (\ c -> isAlphaNum c || c == '_') s == s
    then s
    else error "bad name"


split_at :: Char -> String -> [String]
split_at _ "" = []
split_at c  s =
    let (s1, s2) = break (== c) s
    in  case s2 of
            ""              -> [s1]
            x : s3 | x == c -> s1 : split_at c s3
            _               -> error "error while spliting"


parse_grammar :: String -> Grammar
parse_grammar s =
    let ss = (filter (\ l -> notElem l ["", "\n"]) . split_at ';') s
    in  case ss of
            [v, w, r, a] ->
                let terminals        = trace' $ parse_terminals    v
                    nonterminals     = trace' $ parse_nonterminals w
                    rules            = trace' $ parse_rules        terminals nonterminals r
                    axiom            = trace' $ parse_axiom        nonterminals a
                in  G terminals nonterminals rules axiom
            _               -> error "bad grammar file"


parse_terminals :: String -> S.Set Terminal
parse_terminals =
    ( S.fromList
    . map T
    . split_at ','
    . init
    . tail
    . filter (not . isSpace)
    )


parse_nonterminals :: String -> S.Set NonTerminal
parse_nonterminals =
    ( S.fromList
    . map (N . check_name)
    . split_at ','
    . init
    . tail
    . filter (not . isSpace)
    )


parse_rules :: S.Set Terminal -> S.Set NonTerminal -> String -> M.Map NonTerminal (S.Set Rule)
parse_rules ts ns =
    ( M.fromList
    . map (parse_rule ts ns)
    . split_at ','
    . takeWhile (/= '}')
    . tail
    . dropWhile (/= '{')
    )


parse_rule :: S.Set Terminal -> S.Set NonTerminal -> String -> (NonTerminal, S.Set Rule)
parse_rule ts ns s =
    let (s1, s2) = break (== '=') s
        nt = case words s1 of
            [w] -> if S.member (N w) ns
                then N w
                else error "bad nonterminal in rule"
            _   -> error "bad rule lhs"
        rs =
            ( S.fromList
            . map
                ( R
                . map
                    ( (\ w -> if S.member (T w) ts
                        then (ST . T) w
                        else if S.member (N w) ns
                            then (SN . N . check_name) w
                            else error "bad symbol"
                      )
                    . trace'
                    )
                . words
                )
            . split_at '|'
            . tail
            ) s2
    in  (nt, rs)


parse_axiom :: S.Set NonTerminal -> String -> NonTerminal
parse_axiom ns =
    ( (\ w -> if S.member w ns then w else error "bad nonterminal")
    . N
    . check_name
    . filter (not . isSpace)
    )


main :: IO ()
main = do
    f <- readFile "1.g"
    let g = parse_grammar f
    print g


