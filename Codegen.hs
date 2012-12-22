module Codegen
    ( pda2cpp
    ) where


import qualified Data.HashMap.Strict as M
import           Data.List                 (foldl')
import qualified Data.Set            as S
import qualified Text.PrettyPrint    as PP
import           Text.PrettyPrint          (Doc, ($$), (<>), ($+$))

import Grammar
import PDA


pda2cpp :: Int -> Bool -> PDA -> String
pda2cpp order is_sll pda =
    case order of
        1 -> doc_sll1 pda
        k -> if is_sll
            then doc_sll k pda
            else doc_ll  k pda


doc_sll1 :: PDA -> String
doc_sll1 pda =
    let d0 = doc_includes
        d1 = doc_defines
        d2 = doc_symbols
        d3 = doc_parser pda
    in  PP.render $ d0 $$$ d1 $$$ d2 $$$ d3


doc_sll :: Int -> PDA -> String
doc_sll pda = error "ai bol'no!"


doc_ll :: Int -> PDA -> String
doc_ll pda = error "aiaiai! bol'no zhe"


------------------------------------------------------------SLL1
doc_includes :: Doc
doc_includes =
    PP.text "#include <stdio.h>"


doc_defines :: Doc
doc_defines =
    PP.text "#define STACK_SIZE 1024"


doc_symbols :: Doc
doc_symbols =
    let tsymbols = (map show . S.toList) terminals
        nsymbols = (map show . S.toList) nonterminals
        symbols = (PP.vcat . PP.punctuate PP.comma . map PP.text) (tsymbols ++ nsymbols)
    in  PP.text "enum Symbol" $$ wrap_in_braces symbols <> PP.semi


doc_parser :: PDA -> Doc
doc_parser pda =
    let (sll1_tbl, id2cmd) = restrict_to_sll1 (commands pda)
        d0 = doc_signature
        d1 = doc_init_table sll1_tbl
        d2 = doc_init_stack
        d3 = doc_loop id2cmd
        d4 = doc_return
        d5 = wrap_in_braces $ d1 $$$ d2 $$$ d3 $$ d4
    in  d0 $$ d5


doc_signature :: Doc
doc_signature = PP.text "bool parse (Symbol * p, Symbol * q)"


doc_init_table :: M.HashMap (NonTerminal, Terminal) Int -> Doc
doc_init_table tbl =
    let d1 =
            PP.text "int table "
            <> PP.brackets (PP.int (S.size nonterminals))
            <> PP.brackets (PP.int (S.size terminals))
            <> PP.semi
        f doc (n, t) k = doc
            $$ PP.text "table "
            <> PP.brackets ((PP.text . show) n)
            <> PP.brackets ((PP.text . show) t)
            <> PP.text " = "
            <> PP.int k
            <> PP.semi
        d2 = M.foldlWithKey' f PP.empty tbl
    in  d1 $$$ d2


doc_init_stack :: Doc
doc_init_stack =
    let d1 =
            PP.text "Symbol * stack = new Symbol "
            <> PP.brackets (PP.text "STACK_SIZE")
            <> PP.semi
        d2 = PP.text "const Symbol * stack_bottom = &stack[0];"
        d3 =
            PP.text "*stack = "
            <> (PP.text . show) axiom
            <> PP.semi
    in  d1 $$ d2 $$$ d3


doc_loop :: M.HashMap Int [Symbol] -> Doc
doc_loop id2cmd =
    let d0     = PP.text "while (p != q)" -- "while (stack != stack_bottom && p != q)"
        d_if   = PP.text "*stack == *p"
        d_then = PP.text "p++;" $$ PP.text "--stack;"
        d_else = doc_switch (PP.text "table [*stack][*p]") (doc_cases id2cmd $$ doc_default (PP.text "return false;"))
        d1     = doc_ifthenelse d_if d_then d_else
    in  d0 $$ wrap_in_braces d1


doc_cases :: M.HashMap Int [Symbol] -> Doc
doc_cases id2cmd =
    let doc_symbol (N n) = (PP.text . show) n
        doc_symbol (T t) = (PP.text . show) t
        f doc k ss =
            let doc' = case ss of
                    []      -> PP.text "--stack;"
                    s : ss' ->
                        foldl' (\ d s -> PP.text "*stack++ = " <> doc_symbol s <> PP.semi $$ d) PP.empty ss'
                        $$ PP.text "*stack = " <> doc_symbol s <> PP.semi
            in  doc $$ doc_casebreak k doc'
    in  M.foldlWithKey' f PP.empty id2cmd


doc_return :: Doc
doc_return = PP.text "return true;"


----------------------------------------------------------------------


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


wrap_in_braces :: Doc -> Doc
wrap_in_braces d =
    PP.text "{"
    $+$ PP.nest 4 d
    $$ PP.text "}"


doc_ifthenelse :: Doc -> Doc -> Doc -> Doc
doc_ifthenelse d1 d2 d3 =
    PP.text "if " <> PP.parens d1
    $$ wrap_in_braces d2
    $$ PP.text "else"
    $$ wrap_in_braces d3


doc_switch :: Doc -> Doc -> Doc
doc_switch d1 d2 =
    PP.text "switch " <> (PP.parens d1)
    $$ (wrap_in_braces d2)


doc_casebreak :: Int -> Doc -> Doc
doc_casebreak k d =
    PP.text "case " <> PP.int k <> PP.colon
    $$ PP.nest 4 (d $$ PP.text "break;")


doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


