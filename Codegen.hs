module Codegen
    ( pda2cpp
    ) where


import qualified Data.Set         as S
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint       (Doc, ($$), (<>), ($+$))

import Grammar
import PDA


pda2cpp :: PDA -> String
pda2cpp pda =
    let d0 = doc_includes
        d1 = doc_defines
        d2 = doc_symbols
        d3 = doc_parser pda
    in  PP.render $ d0 $$$ d1 $$$ d2 $$$ d3


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
    let d0 = PP.text "void parse ()"
        d1 =
            PP.text "Symbol table "
            <> PP.brackets (PP.int (S.size nonterminals))
            <> PP.brackets (PP.int (S.size terminals))
            <> PP.semi
        d2 =
            PP.text "Symbol stack "
            <> PP.brackets (PP.text "STACK_SIZE")
            <> PP.semi
        d3 = doc_init_table pda
        d4 = wrap_in_braces $ d1 $$$ d2 $$$ d3
    in  d0 $$ d4


doc_init_table :: PDA -> Doc
doc_init_table pda = PP.empty


----------------------------------------------------------------------

($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


wrap_in_braces :: Doc -> Doc
wrap_in_braces d = PP.text "{" $+$ PP.nest 4 d $$ PP.text "}"


