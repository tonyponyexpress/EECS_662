--Author: Tony Nguyen
--Date: 10/28/19
--Assignment: Project 2

{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving ( Show, Eq )

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving ( Show,Eq )

type Env = [ ( String, BBAE ) ]

type Cont = [ ( String, TBBAE ) ]

--subst implementation pulled from 'Adding Identifiers' chapter in the textbook.
subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ ( Num x ) = ( Num x )
subst i v ( Plus l r ) = ( Plus ( subst i v l ) ( subst i v r ) )
subst i v ( Minus l r ) = ( Minus ( subst i v l ) ( subst i v r ) )
subst i v ( Bind i' v' b' ) = if i==i' then ( Bind i' ( subst i v v' ) b' )
                              else ( Bind i' ( subst i v v' ) ( subst i v b' ) )
subst i v ( Id i' ) = if i==i' then v
                               else ( Id i' )

evalS :: BBAE -> ( Maybe BBAE )
evalS ( Num n ) = Just ( Num n )
evalS ( Plus l r ) = do { ( Num l' ) <- evalS l;
                          ( Num r' ) <- evalS r;
                          Just ( Num ( l' + r' ) ) }
evalS ( Minus l r ) = do { ( Num l' ) <- evalS l;
                          ( Num r' ) <- evalS r;
                          Just ( Num ( l' - r' ) ) }
evalS ( Bind i v b ) = do { v' <- evalS v;
                            evalS ( subst i v' b ) }
evalS ( Id s ) = Nothing
evalS ( Boolean b ) = Just ( Boolean b )
evalS ( And l r ) = do { ( Boolean l' ) <- evalS l;
                         ( Boolean r' ) <- evalS r;
                         Just ( Boolean ( l' && r' ) ) }
evalS ( Leq l r ) = do { ( Num l' ) <- evalS l;
                         ( Num r' ) <- evalS r;
                         Just ( Boolean ( l' <= r' ) ) }
evalS ( IsZero x ) = do { ( Num x' ) <- evalS x;
                          Just ( Boolean ( x' == 0 ) ) }
evalS ( If c t f ) = do { ( Boolean c' ) <- evalS c;
                          if c' then evalS t
                                else evalS f }

evalM :: Env -> BBAE -> ( Maybe BBAE )
evalM _ ( Num n ) = Just ( Num n )
evalM e ( Plus l r ) = do { ( Num l' ) <- evalM e l;
                            ( Num r' ) <- evalM e r;
                            Just ( Num ( l' + r' ) ) }
evalM e ( Minus l r ) = do { ( Num l' ) <- evalM e l;
                             ( Num r' ) <- evalM e r;
                             Just ( Num ( l' - r' ) ) }
evalM e ( Bind i v b ) = do { v' <- evalM e v;
                              evalM( ( i, v' ):e ) b }
evalM e ( Id s ) = lookup s e
evalM _ ( Boolean b ) = Just ( Boolean b )
evalM e ( And l r ) = do { ( Boolean l' ) <- evalM e l;
                           ( Boolean r' ) <- evalM e r;
                           Just ( Boolean ( l' && r' ) ) }
evalM e ( Leq l r ) = do { ( Num l' ) <- evalM e l;
                           ( Num r' ) <- evalM e r;
                           Just ( Boolean ( l' <= r' ) ) }
evalM e ( IsZero x ) = do { ( Num x' ) <- evalM e x;
                            Just ( Boolean ( x' == 0 ) ) }
evalM e ( If c t f ) = do { ( Boolean c' ) <- evalM e c;
                            if c' then evalM e t
                                  else evalM e f }

testBBAE :: BBAE -> Bool
testBBAE e = if evalM [] e == evalS e then True
                                      else False

typeofM :: Cont -> BBAE -> ( Maybe TBBAE )
typeofM _ ( Num _ ) = Just ( TNum )
typeofM c ( Plus l r ) = do { TNum <- typeofM c l;
                              TNum <- typeofM c r;
                              Just ( TNum ) }
typeofM c ( Minus l r ) = do { TNum <- typeofM c l;
                               TNum <- typeofM c r;
                               Just ( TNum ) }
typeofM c ( Bind i v b ) = do { v' <- typeofM c v;
                                typeofM( ( i, v' ):c ) b }
typeofM c ( Id s ) = lookup s c
typeofM _ ( Boolean _ ) = Just ( TBool )
typeofM c ( And l r ) = do { TBool <- typeofM c l;
                             TBool <- typeofM c r;
                             Just ( TBool ) }
typeofM c ( Leq l r ) = do { TNum <- typeofM c l;
                             TNum <- typeofM c r;
                             Just ( TBool ) }
typeofM c ( IsZero x ) = do { TNum <- typeofM c x;
                              Just ( TBool ) }
typeofM c ( If x y z ) = do { TBool <- typeofM c x;
                              ty <- typeofM c y;
                              tz <- typeofM c z;
                              if ty == tz then Just ( ty )
                                          else Nothing }

evalT :: BBAE -> ( Maybe BBAE )
evalT e = do { typeofM [] e;
               evalM [] e }
