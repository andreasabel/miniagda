module Tokens where

data Token = Id String
           | Data
           | Fun
           | Def
           | Mutual
           | Set
           -- size type
           | Size
           | Infty
           | Succ
           --
           | BrOpen
           | BrClose
           | PrOpen
           | PrClose
           | Sem
           | Col
           | Arrow
           | Eq
           | Lam
           | UScore
           | NotUsed -- so happy doesn't generate overlap case pattern warning
             deriving (Eq,Ord,Show)

