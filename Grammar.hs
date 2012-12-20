module Grammar where

import qualified Data.Set      as S
import           Data.Hashable

data Terminal    = TOBrace | TCBrace | TPlus | TMinus | Ta | Ti | Tlambda deriving (Show, Eq, Ord)
data NonTerminal = NA | NB | NC deriving (Show, Eq, Ord)
data Symbol      = T Terminal | N NonTerminal deriving (Show, Eq, Ord)
type Rule        = NonTerminal -> [[Symbol]]

terminals :: S.Set Terminal
terminals = S.fromList [TOBrace, TCBrace, TPlus, TMinus, Ta, Ti, Tlambda]

nonterminals :: S.Set NonTerminal
nonterminals = S.fromList [NA, NB, NC]

rules :: Rule
rules NA = [[N NC, N NB]]
rules NB = [[T TPlus, N NC, N NB], [T TMinus, N NC, N NB], []]
rules NC = [[T TOBrace, N NA, T TCBrace], [T Ta], [T Ti]]

axiom :: NonTerminal
axiom = NA

instance Hashable Terminal where
    hash TOBrace = 1
    hash TCBrace = 2
    hash TPlus = 3
    hash TMinus = 4
    hash Ta = 5
    hash Ti = 6
    hash Tlambda = 7

instance Hashable NonTerminal where
    hash NA = 1
    hash NB = 2
    hash NC = 3

instance Hashable Symbol where
    hash (T t) = 2 * hash t
    hash (N n) = 2 * hash n + 1