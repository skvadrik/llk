module Grammar where

data Terminal    = TOBrace | TCBrace | TPlus | TMinus | Ta | Ti | Tlambda deriving (Show, Eq, Ord)
data NonTerminal = NA | NB | NC deriving (Show, Eq, Ord)
data Symbol      = T Terminal | N NonTerminal deriving (Show, Eq, Ord)
type Rule        = NonTerminal -> [[Symbol]]

rules :: Rule
rules NA = [[N NC, N NB]]
rules NB = [[T TPlus, N NC, N NB], [T TMinus, N NC, N NB], [T Tlambda]]
rules NC = [[T TOBrace, N NA, T TCBrace], [T Ta], [T Ti]]

axiom :: NonTerminal
axiom = NB