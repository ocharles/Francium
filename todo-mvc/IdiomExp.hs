{-# LANGUAGE TemplateHaskell #-}

module IdiomExp where

import Control.Applicative
import Control.Monad
import Data.List
import Language.Haskell.TH

i :: Q Exp -> Q Exp
i = runQ >=> go
  where go (AppE f x) =
          [|$(go f) <*>
            $(return x)|]
        go (InfixE (Just l) op (Just r)) =
          [|$(go op) <*>
            $(return l) <*>
            $(return r)|]
        go e@(TupE elems) =
          do names <-
               mapM (\_ -> newName "t") elems
             let lam =
                   LamE (map VarP names)
                        (TupE (map VarE names))
             return (foldl (\l r ->
                              InfixE (Just l)
                                     (VarE '(<*>))
                                     (Just r))
                           (AppE (VarE 'pure) lam)
                           elems)
        go (RecConE n exprs) =
          do names <-
               mapM (\_ -> newName "t") exprs
             let lam =
                   LamE (map VarP names)
                        (RecConE n
                                 (zipWith (\(f,_) n ->
                                             (f,VarE n))
                                          exprs
                                          names))
             return (foldl (\l r ->
                              InfixE (Just l)
                                     (VarE '(<*>))
                                     (Just r))
                           (AppE (VarE 'pure) lam)
                           (map (\(_,expr) -> expr) exprs))
        go (RecUpdE x exprs) =
          do names <-
               mapM (\_ -> newName "t") exprs
             let lam =
                   LamE (map VarP names)
                        (RecUpdE x
                                 (zipWith (\(f,_) n ->
                                             (f,VarE n))
                                          exprs
                                          names))
             return (foldl (\l r ->
                              InfixE (Just l)
                                     (VarE '(<*>))
                                     (Just r))
                           (AppE (VarE 'pure) lam)
                           (map (\(_,expr) -> expr) exprs))
        go (CaseE exp stmts) =
          do lam <-
               [|\t ->
                   $(return (CaseE (VarE 't) stmts))|]
             return (InfixE (Just (AppE (VarE 'pure) lam))
                            (VarE '(<*>))
                            (Just exp))
        go e = [|pure $(return e)|]
