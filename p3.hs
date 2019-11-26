--Author: Tony Nguyen
--Date: 11/23/19
--Assignment: Project 3

{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving ( Show, Eq )

type Env = [ ( String, FAE ) ]

evalDynFAE :: Env -> FAE -> ( Maybe FAE )
evalDynFAE _ ( Num n ) = Just ( Num n )
evalDynFAE e ( Plus l r ) = do { ( Num l' ) <- evalDynFAE e l;
                                 ( Num r' ) <- evalDynFAE e r;
                                 Just( Num ( l' + r' ) ) }
evalDynFAE e ( Minus l r ) = do { ( Num l' ) <- evalDynFAE e l;
                                  ( Num r' ) <- evalDynFAE e r;
                                  Just( Num ( l' - r' ) ) }
evalDynFAE _ ( Lambda i b ) = Just ( Lambda i b )
evalDynFAE e ( App f a ) =  do { ( Lambda i b ) <- evalDynFAE e f;
                                 a' <- evalDynFAE e a;
                                 evalDynFAE( ( i, a' ):e ) b }
evalDynFAE e ( Id i ) = lookup i e

data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving ( Show, Eq )

type Env' = [ ( String, FAEValue ) ]

evalStatFAE :: Env' -> FAE -> ( Maybe FAEValue )
evalStatFAE _ ( Num n ) = Just ( NumV n )
evalStatFAE e ( Plus l r ) = do { ( NumV l' ) <- evalStatFAE e l;
                                  ( NumV r' ) <- evalStatFAE e r;
                                  Just( NumV( l' + r' ) ) }
evalStatFAE e ( Minus l r ) = do { ( NumV l' ) <- evalStatFAE e l;
                                   ( NumV r' ) <- evalStatFAE e r;
                                   Just( NumV( l' - r' ) ) }
evalStatFAE e ( Lambda i b ) = Just ( ClosureV i b e )
evalStatFAE e ( App f a ) = do { ( ClosureV i b e ) <- evalStatFAE e f;
                                 a' <- evalStatFAE e a;
                                 evalStatFAE( ( i, a' ):e ) b }
evalStatFAE e ( Id i ) = lookup i e

-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving ( Show, Eq )

elabFBAE :: FBAE -> FAE
elabFBAE ( NumD n ) = Num n
elabFBAE ( PlusD l r ) = Plus ( elabFBAE l ) ( elabFBAE r )
elabFBAE ( MinusD l r ) = Minus ( elabFBAE l ) ( elabFBAE r )
elabFBAE ( LambdaD i b ) = Lambda i ( elabFBAE b )
elabFBAE ( AppD f a ) = App ( elabFBAE f ) ( elabFBAE a )
elabFBAE ( BindD i v b ) = ( App ( Lambda i ( elabFBAE b ) ) ( elabFBAE v ) )
elabFBAE ( IdD i ) = Id i

evalFBAE :: Env' -> FBAE -> ( Maybe FAEValue )
evalFBAE e a = evalStatFAE e ( elabFBAE a )

-- FBAEC AST and Type Definitions

data FBAEC where
  NumE :: Int -> FBAEC
  PlusE :: FBAEC -> FBAEC -> FBAEC
  MinusE :: FBAEC -> FBAEC -> FBAEC
  TrueE :: FBAEC
  FalseE :: FBAEC
  AndE :: FBAEC -> FBAEC -> FBAEC
  OrE :: FBAEC -> FBAEC -> FBAEC
  NotE :: FBAEC -> FBAEC
  IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  LambdaE :: String -> FBAEC -> FBAEC
  AppE :: FBAEC -> FBAEC -> FBAEC
  BindE :: String -> FBAEC -> FBAEC -> FBAEC
  IdE :: String -> FBAEC
  deriving ( Show, Eq )

elabFBAEC :: FBAEC -> FAE
elabFBAEC ( NumE n ) = Num n
elabFBAEC ( PlusE l r ) = Plus ( elabFBAEC l ) ( elabFBAEC r )
elabFBAEC ( MinusE l r ) = Minus ( elabFBAEC l ) ( elabFBAEC r )
elabFBAEC ( TrueE ) = Lambda "t" ( Lambda "f" ( Id "t" ) )
elabFBAEC ( FalseE ) = Lambda "t" ( Lambda "f"  ( Id "f" ) )
elabFBAEC ( AndE l r ) = ( App ( App ( Lambda "m" ( Lambda "n" ( App ( App ( Id "m" ) ( Id "n" ) ) ( elabFBAEC FalseE ) ) ) ) ( elabFBAEC l ) ) ( elabFBAEC r ) )
elabFBAEC ( OrE l r ) = ( App ( App ( Lambda "m" ( Lambda "n" ( App ( App ( Id "m" ) ( elabFBAEC TrueE ) ) ( Id "n" ) ) ) ) ( elabFBAEC l ) ) ( elabFBAEC r ) )
elabFBAEC ( NotE a ) = ( Lambda "m" ( Lambda "n" ( App ( App ( elabFBAEC a ) ( Id "n" ) ) ( Id "m" ) ) ) )
elabFBAEC ( IfE a b c ) = App ( App ( App ( Lambda "x" ( Lambda "true" ( Lambda "false" ( App ( App ( Id "x" ) ( Id "true" ) ) ( Id "false" ) ) ) ) ) ( elabFBAEC a ) ) ( elabFBAEC b ) ) ( elabFBAEC c )
elabFBAEC ( LambdaE i b ) = Lambda i ( elabFBAEC b )
elabFBAEC ( AppE f a ) = App ( elabFBAEC f ) ( elabFBAEC a )
elabFBAEC ( BindE i v b ) = ( App ( Lambda i ( elabFBAEC b ) ) ( elabFBAEC v ) )
elabFBAEC ( IdE i ) = Id i

evalFBAEC :: Env' -> FBAEC -> ( Maybe FAEValue )
evalFBAEC e a = evalStatFAE e ( elabFBAEC a )

