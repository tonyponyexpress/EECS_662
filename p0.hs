-- Student: Tony Nguyen
-- KU ID: 2878004
-- Assignment: Project 0
-- Date: 9/13/19
-- NOTE: Load this module and enter "main" in GHCI to run the test cases.

{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple calculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }

lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

evalAE :: AE -> Int
evalAE ( Num n ) = if ( n < 0 ) then error "Numbers can't be negative."
                                else n
evalAE ( Plus l r ) = ( evalAE l ) + ( evalAE r )
evalAE ( Minus l r ) = let v1 = evalAE l in
                       let v2 = evalAE r in
                       if ( v1 >= v2 ) then ( v1 - v2 )
                                       else error "Cannot have negative result."
evalAE ( Mult l r ) = ( evalAE l ) * ( evalAE r )
evalAE ( Div l r ) = if ( evalAE r == 0 ) then error "Divide by zero."
                                          else div ( evalAE l ) ( evalAE r )
evalAE ( If0 x y z ) = if ( evalAE x == 0 ) then evalAE y
                                            else evalAE z

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe ( Num n ) = if ( n < 0 ) then Nothing
                                     else Just n
evalAEMaybe ( Plus l r ) = let l' = evalAEMaybe l in
                           let r' = evalAEMaybe r in
                           if ( l' == Nothing ) then Nothing
                                                else if ( r' == Nothing ) then Nothing
                                                else
                                                  let Just v1 = l' in
                                                  let Just v2 = r' in
                                                  Just ( v1 + v2 )
evalAEMaybe ( Minus l r ) = let l' = evalAEMaybe l in
                            let r' = evalAEMaybe r in
                            if ( l' == Nothing ) then Nothing
                                                 else if ( r' == Nothing ) then Nothing
                                                 else if ( l' >= r' ) then
                                                   let Just v1 = l' in
                                                   let Just v2 = r' in
                                                   Just ( v1 - v2 )
                                                 else Nothing
evalAEMaybe ( Mult l r ) = let l' = evalAEMaybe l in
                           let r' = evalAEMaybe r in
                           if ( l' == Nothing ) then Nothing
                                                else if ( r' == Nothing ) then Nothing
                                                else
                                                  let Just v1 = l' in
                                                  let Just v2 = r' in
                                                  Just ( v1 * v2 )
evalAEMaybe ( Div l r ) = let l' = evalAEMaybe l in
                          let r' = evalAEMaybe r in
                          if ( l' == Nothing ) then Nothing
                                               else if ( r' == Nothing ) then Nothing
                                               else if ( r' <= Just 0 ) then Nothing
                                               else
                                                 let Just v1 = l' in
                                                 let Just v2 = r' in
                                                 Just ( div ( v1 ) ( v2 ) )
evalAEMaybe ( If0 x y z ) = let x' = evalAEMaybe x in
                            let y' = evalAEMaybe y in
                            let z' = evalAEMaybe z in
                            if ( x' == Nothing ) then Nothing
                                                 else if ( x' == Just 0 ) then y'
                                                                          else z'

evalM :: AE -> Maybe Int
evalM ( Num n ) = if ( n < 0 ) then Nothing
                               else Just n
evalM ( Plus l r ) = do { l' <- ( evalM l );
                          r' <- ( evalM r );
                          Just ( l' + r' ) }
evalM ( Minus l r ) = do { l' <- ( evalM l );
                           r' <- ( evalM r );
                           if ( l' >= r' ) then Just ( l' - r' )
                                           else Nothing }
evalM ( Mult l r ) = do { l' <- ( evalM l );
                          r' <- ( evalM r );
                          Just ( l' * r' ) }
evalM ( Div l r ) = do { l' <- ( evalM l );
                         r' <- ( evalM r );
                         if ( r' <= 0 ) then Nothing
                                        else Just ( div ( l' ) ( r' ) ) }
evalM ( If0 x y z ) = do { x' <- ( evalM x );
                           y' <- ( evalM y );
                           z' <- ( evalM z );
                           if ( x' == 0 ) then Just y'
                                          else Just z' }

interpAE :: String -> Maybe Int
interpAE s = evalM ( parseAE s )

--Test cases
evalAEMaybeTest = do
                  print ( evalAEMaybe ( Num 10 ) )
                  print ( evalAEMaybe ( Num ( -1 ) ) )
                  print ( evalAEMaybe ( Plus ( Num 10 ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( Plus ( Num ( -10 ) ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( Minus ( Num 10 ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( Minus ( Num 5 ) ( Num 10 ) ) )
                  print ( evalAEMaybe ( Minus ( Num ( -10 ) ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( Mult ( Num 10 ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( Mult ( Num ( -10 ) ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( Div ( Num 10 ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( Div ( Num 10 ) ( Num 0 ) ) )
                  print ( evalAEMaybe ( If0 ( Num 0 ) ( Num 10 ) ( Num 5 ) ) )
                  print ( evalAEMaybe ( If0 ( Num ( -1 ) ) ( Num 10 ) ( Num 5 ) ) )

evalMTest = do
            print ( evalM ( Num 10 ) )
            print ( evalM ( Num ( -1 ) ) )
            print ( evalM ( Plus ( Num 10 ) ( Num 5 ) ) )
            print ( evalM ( Plus ( Num ( -10 ) ) ( Num 5 ) ) )
            print ( evalM ( Minus ( Num 10 ) ( Num 5 ) ) )
            print ( evalM ( Minus ( Num 5 ) ( Num 10 ) ) )
            print ( evalM ( Minus ( Num ( -10 ) ) ( Num 5 ) ) )
            print ( evalM ( Mult ( Num 10 ) ( Num 5 ) ) )
            print ( evalM ( Mult ( Num ( -10 ) ) ( Num 5 ) ) )
            print ( evalM ( Div ( Num 10 ) ( Num 5 ) ) )
            print ( evalM ( Div ( Num 10 ) ( Num 0 ) ) )
            print ( evalM ( If0 ( Num 0 ) ( Num 10 ) ( Num 5 ) ) )
            print ( evalM ( If0 ( Num ( -1 ) ) ( Num 10 ) ( Num 5 ) ) )

interpAETest = do
               print ( interpAE "10" )
               print ( interpAE "-1" )
               print ( interpAE "10+5" )
               print ( interpAE "-10+5" )
               print ( interpAE "10-5" )
               print ( interpAE "5-10" )
               print ( interpAE "-10-5" )
               print ( interpAE "10*5" )
               print ( interpAE "-10*5" )
               print ( interpAE "10/5" )
               print ( interpAE "10/0" )

evalAETest = do
             print ( evalAE ( Num 10 ) )
             print ( evalAE ( Plus ( Num 10) ( Num 5 ) ) )
             print ( evalAE ( Minus ( Num 10 ) ( Num 5 ) ) )
             print ( evalAE ( Mult ( Num 10 ) ( Num 5 ) ) )
             print ( evalAE ( Div ( Num 10 ) ( Num 5 ) ) )
             print ( evalAE ( If0 ( Num 0 ) ( Num 10 ) ( Num 5 ) ) )

testCases = do
            print ( "Begin tests!" )
            print ( "evalAEMaybe Testing: " )
            evalAEMaybeTest
            print ( "evalAEMaybe Testing Complete." )
            print ( "evalM Testing: " )
            evalMTest
            print ( "evalM Testing Complete." )
            print ( "interpAE Testing: " )
            interpAETest
            print ( "interpAE Testing Complete." )
            print ( "evalAE Testing: " )
            evalAETest
            print ( "evalAE Testing Complete." )
            print ( "All tests are finished!" )

main :: IO()
main = do
       testCases
