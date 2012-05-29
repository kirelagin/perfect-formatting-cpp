{
{-# LANGUAGE ViewPatterns #-}

module Cppf where

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.Instances
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
    '\''            { TSQuote }
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
    '='             { TAssign }
    '++'            { TPlusplus }
    '--'            { TMinusminus }
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

parseError t = error (show t)


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
           | TSQuote
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


-- *****
-- LEXER

tokenise [] = []

tokenise (stripPrefix "if" -> Just cs)      = TIf       : tokenise cs
tokenise (stripPrefix "else" -> Just cs)    = TElse     : tokenise cs
tokenise (stripPrefix "switch" -> Just cs)  = TSwitch   : tokenise cs

tokenise (stripPrefix "while" -> Just cs)   = TWhile    : tokenise cs
tokenise (stripPrefix "do" -> Just cs)      = TDo       : tokenise cs
tokenise (stripPrefix "for" -> Just cs)     = TFor      : tokenise cs

tokenise (stripPrefix "break" -> Just cs)   = TBreak    : tokenise cs
tokenise (stripPrefix "continue" -> Just cs)= TContinue : tokenise cs
tokenise (stripPrefix "return" -> Just cs)  = TReturn   : tokenise cs
tokenise (stripPrefix "goto" -> Just cs)    = TGoto     : tokenise cs

tokenise (stripPrefix "static" -> Just cs)    = TStorageClass "static" : tokenise cs
tokenise (stripPrefix "extern" -> Just cs)    = TStorageClass "extern" : tokenise cs

tokenise (stripPrefix "inline" -> Just cs)    = TFunctionSpec "inline"   : tokenise cs
tokenise (stripPrefix "virtual" -> Just cs)   = TFunctionSpec "virtual"  : tokenise cs
tokenise (stripPrefix "explicit" -> Just cs)  = TFunctionSpec "explicit" : tokenise cs

tokenise (stripPrefix "const" -> Just cs)    = TCVQualifier "const"    : tokenise cs
tokenise (stripPrefix "volatile" -> Just cs) = TCVQualifier "volatile" : tokenise cs


tokenise ('/':'/':cs) = tokenise $ dropWhile (/='\n') cs

tokenise ('=':'=':cs) = TEq     : tokenise cs
tokenise ('!':'=':cs) = TNeq    : tokenise cs
tokenise ('&':'&':cs) = TAnd    : tokenise cs
tokenise ('|':'|':cs) = TOr     : tokenise cs
tokenise ('<':'<':cs) = TShiftL : tokenise cs
tokenise ('>':'>':cs) = TShiftR : tokenise cs
tokenise ('>':'=':cs) = TGte    : tokenise cs
tokenise ('<':'=':cs) = TLte    : tokenise cs
tokenise ('-':'>':cs) = TArrow  : tokenise cs

tokenise (';':cs) = TSemicolon  : tokenise cs
tokenise (',':cs) = TComma      : tokenise cs
tokenise ('(':cs) = TParenL     : tokenise cs
tokenise (')':cs) = TParenR     : tokenise cs
tokenise ('{':cs) = TBraceL     : tokenise cs
tokenise ('}':cs) = TBraceR     : tokenise cs
tokenise ('[':cs) = TSquareL    : tokenise cs
tokenise (']':cs) = TSquareR    : tokenise cs

tokenise ('+':cs) = TPlus       : tokenise cs
tokenise ('-':cs) = TMinus      : tokenise cs
tokenise ('*':cs) = TAst        : tokenise cs
tokenise ('/':cs) = TSlash      : tokenise cs
tokenise ('%':cs) = TPercent    : tokenise cs
tokenise ('&':cs) = TAmp        : tokenise cs
tokenise ('|':cs) = TBar        : tokenise cs
tokenise ('^':cs) = TCirc       : tokenise cs
tokenise ('!':cs) = TExclam     : tokenise cs
tokenise ('~':cs) = TTilde      : tokenise cs
tokenise ('>':cs) = TGt         : tokenise cs
tokenise ('<':cs) = TLt         : tokenise cs
tokenise ('.':cs) = TDot        : tokenise cs
tokenise ('=':cs) = TAssign     : tokenise cs

tokenise ('\'':cs) = TSQuote : TString s : TSQuote : tokenise rest
    where (s, rest) = readString "'" cs
tokenise ('"':cs) = TDQuote  : TString s : TDQuote : tokenise rest
    where (s, rest) = readString "\"" cs

tokenise l@(c:cs)
    | isSpace c = tokenise cs
    | isDigit c = readNum l
    | isAlpha c = readId l
    | otherwise = error $ "Unexpected '" ++ (c : "'")
 where
    readNum l = TNum n : tokenise rest where
        (n, rest) = span (liftM2 (||) (=='.') isDigit) l
    readId l = TId i : tokenise rest where
        (i, rest) = span isAlphaNum l

readString p ('\\':c:cs) = ('\\':c:ss, rest)
    where (ss, rest) = readString p cs
readString p (stripPrefix p -> Just rest) = ("", rest)
readString p (c:cs) = (c:ss, rest)
    where (ss, rest) = readString p cs
}
