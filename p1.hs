-- Author: Tony Nguyen
-- Date: 9/28/19
-- Brief: Load this file and enter "testCases" in the command line to run my tests.
-- Assignment: Project 1

{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving ( Show, Eq )

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving ( Show, Eq )

-- Evaluation Functions

evalM :: ABE -> ( Maybe ABE )
evalM ( Num n ) = Just ( Num n )
evalM ( Plus l r ) = do { ( Num l' ) <- evalM l;
                          ( Num r' ) <- evalM r;
                          if ( l' < 0 || r' < 0 ) then Nothing
                                                  else return ( Num( l' + r' ) ) }
evalM ( Minus l r ) = do { ( Num l' ) <- evalM l;
                           ( Num r' ) <- evalM r;
                           if ( l' < 0 || r' < 0 ) then Nothing
                                                   else return ( Num( l' - r' ) ) }
evalM ( Mult l r ) = do { ( Num l' ) <- evalM l;
                          ( Num r' ) <- evalM r;
                          if ( l' < 0 || r' < 0 ) then Nothing
                                                  else return ( Num( l' * r' ) ) }
evalM ( Div l r ) = do { ( Num l' ) <- evalM l;
                         ( Num r' ) <- evalM r;
                         if ( l' < 0 || r' <= 0 ) then Nothing
                                                  else return ( Num( l' `div` r' ) ) }
evalM ( Boolean x ) = Just ( Boolean x )
evalM ( And l r ) = do { ( Boolean l' ) <- evalM l;
                         ( Boolean r' ) <- evalM r;
                         return ( Boolean ( l' && r' ) ) }
evalM ( Leq l r ) = do { ( Num l' ) <- evalM l;
                         ( Num r' ) <- evalM r;
                         if ( l' < 0 || r' < 0 ) then Nothing
                                                 else return ( Boolean ( l' <= r' ) ) }
evalM ( IsZero x ) = do { ( Num x' ) <- evalM x;
                          if ( x' < 0 ) then Nothing
                                        else return ( Boolean ( x' == 0 ) ) }
evalM ( If x y z ) = do { ( Boolean x' ) <- evalM x;
                          if ( x' ) then evalM y
                                    else evalM z }


evalErr :: ABE -> ( Maybe ABE )
evalErr ( Num n ) = Just ( Num n )
evalErr ( Plus l r ) = do { ( Num l' ) <- evalErr l;
                           ( Num r' ) <- evalErr r;
                           if ( l' < 0 || r' < 0 ) then Nothing
                                                   else return ( Num( l' + r' ) ) }
evalErr ( Minus l r ) = do { ( Num l' ) <- evalErr l;
                             ( Num r' ) <- evalErr r;
                             if ( l' < 0 || r' < 0 ) then Nothing
                                                     else return ( Num( l' - r' ) ) }
evalErr ( Mult l r ) = do { ( Num l' ) <- evalErr l;
                            ( Num r' ) <- evalErr r;
                            if ( l' < 0 || r' < 0 ) then Nothing
                                                    else return ( Num( l' * r' ) ) }
evalErr ( Div l r ) = do { ( Num l' ) <- evalErr l;
                           ( Num r' ) <- evalErr r;
                           if ( l' < 0 || r' <= 0 ) then Nothing
                                                    else return ( Num( l' `div` r' ) ) }
evalErr ( Boolean x ) = Just ( Boolean x )
evalErr ( And l r ) = do { ( Boolean l' ) <- evalErr l;
                           ( Boolean r' ) <- evalErr r;
                           return ( Boolean ( l' && r' ) ) }
evalErr ( Leq l r ) = do { ( Num l' ) <- evalErr l;
                           ( Num r' ) <- evalErr r;
                           if ( l' < 0 || r' < 0 ) then Nothing
                                                   else return ( Boolean ( l' <= r' ) ) }
evalErr ( IsZero x ) = do { ( Num x' ) <- evalErr x;
                            if ( x' < 0 ) then Nothing
                                          else return ( Boolean ( x' == 0 ) ) }
evalErr ( If x y z ) = do { ( Boolean x' ) <- evalErr x;
                            if ( x' ) then evalErr y
                                      else evalErr z }

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM ( Num n ) = Just ( TNum )
typeofM ( Plus l r ) = do { TNum <- typeofM l;
                            TNum <- typeofM r;
                            return ( TNum ) }
typeofM ( Minus l r ) = do { TNum <- typeofM l;
                             TNum <- typeofM r;
                             return ( TNum ) }
typeofM ( Mult l r ) = do { TNum <- typeofM l;
                            TNum <- typeofM r;
                            return ( TNum ) }
typeofM ( Div l r ) = do { TNum <- typeofM l;
                           TNum <- typeofM r;
                           return ( TNum ) }
typeofM ( Boolean x ) = Just ( TBool )
typeofM ( And l r ) = do { TBool <- typeofM l;
                           TBool <- typeofM r;
                           return ( TBool ) }
typeofM ( Leq l r ) = do { TNum <- typeofM l;
                           TNum <- typeofM r;
                           return ( TBool ) }
typeofM ( IsZero x ) = do { TNum <- typeofM x;
                            return ( TBool ) }
typeofM ( If x y z ) = do { TBool <- typeofM x;
                            ty <- typeofM y;
                            tz <- typeofM z;
                            if ( ty == tz ) then return ( ty )
                                            else Nothing }
-- Combined interpreter

evalTypeM :: ABE -> ( Maybe ABE )
evalTypeM e = do { t <- typeofM e;
                   evalM e }

-- Optimizer

optimize :: ABE -> ABE
optimize e = case e of
             ( Plus x ( Num 0 ) ) -> x
             ( Plus ( Num 0 ) x ) -> x
             ( If ( Boolean True ) x y ) -> x
             ( If ( Boolean False) x y ) -> y

interpOptM :: ABE -> Maybe ABE
interpOptM e = evalM ( optimize e )

-- Test cases
testCases = do
            print ( "Beginning tests..." )
            print ( evalM ( Plus ( Num 1 ) ( Num 2 ) ) )
            print ( evalM ( Plus ( Num ( -1 ) ) ( Num 2 ) ) )
            print ( evalM ( Div ( Num 1 ) ( Num 0 ) ) )
            print ( typeofM ( Plus ( Num 1 ) ( Num 2 ) ) )
            print ( typeofM ( If ( Boolean True ) ( Num 1 ) ( Num 2 ) ) )
            print ( interpOptM ( Plus ( Num 1 ) ( Num 0 ) ) )
            print ( "Ending tests..." )
