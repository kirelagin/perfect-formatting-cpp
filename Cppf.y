{
module Cppf where

import Data.Char
}

%name cppf
%error { parseError }
%tokentype { Token }
%token
    -- Selection
    IF              { TIf }
    ELSE            { TElse }
    SWITCH          { TSwitch }
    
    -- Iteration
    WHILE           { TWhile }
    DO              { TDo }
    FOR             { TFor }

    -- Jump
    BREAK           { TBreak }
    CONTINUE        { TContinue }
    RETURN          { TReturn }
    GOTO            { TGoto }
    
    -- Types
    SIMPLETYPE      { TSimpleType $$ }
    STORAGECLASS    { TStorageClass $$ }
    FUNCTIONSPEC    { TFunctionSpec $$ }
    CVQUALIFIER     { TCVQualifier $$ }
    
    -- Syntax
    ';'             { TSemicolon }
    ','             { TComma }
    '('             { TParenL }
    ')'             { TParenR }
    '{'             { TBraceL }
    '}'             { TBraceR }
    '['             { TSquareL }
    ']'             { TSquareR }
    '\''            { TSquote }
    '"'             { TDQuote }

    -- Operators
    '+'             { TPlus }
    '-'             { TMinus }
    '*'             { TAst }
    '/'             { TSlash }
    '%'             { TPercent }
    '&'             { TAmp }
    '|'             { TBar }
    '^'             { TCirc }
    '!'             { TExclam }
    '~'             { TTilde }
    '>'             { TGt }
    '<'             { TLt }
    '.'             { TDot }
    '++'            { TPlusplus }
    '--'            { TMinusminus }
    '='             { TAssign }
    '=='            { TEq }
    '!='            { TNeq }
    '&&'            { TAnd }
    '||'            { TOr }
    '<<'            { TShiftL }
    '>>'            { TShiftR }
    '>='            { TGte }
    '<='            { TLte }
    '->'            { TArrow }

    -- Literals
    NUM             { TNum $$ }
    ID              { TId $$ }
    STRING          { TString $$ }

%%

TranslationUnit : {- empty -}    { [] }

{

parseError _ = error "FAIL"


data Token = TIf
           | TElse
           | TSwitch

           | TWhile
           | TDo
           | TFor

           | TBreak
           | TContinue
           | TReturn
           | TGoto
           
           | TSimpleType String
           | TStorageClass String
           | TFunctionSpec String
           | TCVQualifier String
           
           | TSemicolon
           | TComma
           | TParenL
           | TParenR
           | TBraceL
           | TBraceR
           | TSquareL
           | TSquareR
           | TSquote
           | TDQuote

           | TPlus
           | TMinus
           | TAst
           | TSlash
           | TPercent
           | TAmp
           | TBar
           | TCirc
           | TExclam
           | TTilde
           | TGt
           | TLt
           | TDot
           | TPlusplus
           | TMinusminus
           | TAssign
           | TEq
           | TNeq
           | TAnd
           | TOr
           | TShiftL
           | TShiftR
           | TGte
           | TLte
           | TArrow
           
           | TNum       String
           | TId        String
           | TString    String
    deriving (Show, Eq)

tokenise [] = []
}
