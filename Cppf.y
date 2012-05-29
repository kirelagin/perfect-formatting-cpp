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

TranslationUnit :: { String }
                : {- empty -}    { [] }
                | DeclarationSeq { intercalate "\n" $1 ++ "\n" }


-- Statements

Statement :: { [String] }
          : DeclarationStatement    { $1 }
          | CompoundStatement       { $1 }
          -- FIXME

CompoundStatement :: { [String] }
                  : '{' StatementSeq '}'    { "{" : (indented $2 ++ ["}"]) }
                  | '{' '}'                 { ["{}"] }

StatementSeq :: { [String] }
             : Statement StatementSeq   { $1 ++ $2 }
             | Statement                { $1 }

DeclarationStatement :: { [String] }
                     : BlockDeclaration { $1 }

-- Declarations

DeclarationSeq :: { [String] }
               : Declaration DeclarationSeq { $1 ++ $2 }
               | Declaration                { $1 }

Declaration :: { [String] }
            : BlockDeclaration      { $1 }
            | FunctionDefinition    { $1 }

BlockDeclaration :: { [String] }
                 : SimpleDeclaration    { [$1] }

SimpleDeclaration :: { String }
                  : MyTypeSpecifier InitDeclaratorList ';'                    { $1 ++ " " ++ $2 ++ ";" }
                  | MyTypeSpecifier ';'                                       { $1 ++ ";" }

MyTypeSpecifier :: { String }
                : DeclSpecifierSeq TypeSpecifier    { $1 ++ " " ++ $2 }
                | TypeSpecifier                     { $1 }

DeclSpecifierSeq :: { String }
                 : DeclSpecifier DeclSpecifierSeq   { $1 ++ " " ++ $2 }
                 | DeclSpecifier                    { $1 }

DeclSpecifier :: { String }
              : STORAGECLASS    { $1 }
              | FUNCTIONSPEC    { $1 }
              | CVQUALIFIER     { $1 }

TypeSpecifier :: { String }
              : ID  { $1 }

InitDeclaratorList :: { String }
                   : InitDeclarator                         { $1 }
                   | InitDeclarator ',' InitDeclaratorList  { $1 ++ ", " ++ $3 }

InitDeclarator :: { String }
               : Declarator Initializer { $1 ++ " " ++ $2 }
               | Declarator             { $1 }

Declarator :: { String }
           : DirectDeclarator       { $1 }
           | PtrOperator Declarator { $1 ++ (if $1 == "*" then "" else " ") ++ $2 }

DirectDeclarator :: { String }
                 : ID                                                                   { $1 }
                 | '(' Declarator ')'                                                   { "(" ++ $2 ++ ")" }
                 | DirectDeclarator '(' ParameterDeclarationClause ')' CVQualifierSeq   { $1 ++ "(" ++ $3 ++ ")" ++ " " ++ $5 }
                 | DirectDeclarator '(' ParameterDeclarationClause ')'                  { $1 ++ "(" ++ $3 ++ ")" }

PtrOperator :: { String }
            : '&'                   { "&" }
            | '*'                   { "*" }
            | '*' CVQualifierSeq    { "*" ++ $2 }

CVQualifierSeq :: { String }
               : CVQUALIFIER CVQualifierSeq { $1 ++ " " ++ $2 }
               | CVQUALIFIER { $1 }

Initializer :: { String }
            : '=' ID    {  "= " ++ $2 } -- FIXME

ParameterDeclarationClause :: { String }
                           : {- empty -}                { "" }
                           | ParameterDeclarationList   { $1 }

ParameterDeclarationList :: { String }
                         : ParameterDeclaration                                 { $1 }
                         | ParameterDeclaration ',' ParameterDeclarationList    { $1 ++ ", " ++ $3 }

ParameterDeclaration :: { String }
                     : MyTypeSpecifier Declarator             { $1 ++ " " ++ $2 }
                     | MyTypeSpecifier AbstractDeclarator     { $1 ++ " " ++ $2 }
                     | MyTypeSpecifier                        { $1 }

AbstractDeclarator :: { String }
           : DirectAbstractDeclarator       { $1 }
           | PtrOperator AbstractDeclarator { $1 ++ (if $1 == "*" then " " else " ") ++ $2 }
           | PtrOperator { $1 }

DirectAbstractDeclarator :: { String }
                 : '(' AbstractDeclarator ')'                                                   { "(" ++ $2 ++ ")" }
                 | DirectAbstractDeclarator '(' ParameterDeclarationClause ')' CVQualifierSeq   { $1 ++ "(" ++ $3 ++ ")" ++ " " ++ $5 }
                 | DirectAbstractDeclarator '(' ParameterDeclarationClause ')'                  { $1 ++ "(" ++ $3 ++ ")" }


-- Function definitions

FunctionDefinition :: { [String] }
                   : MyTypeSpecifier Declarator CompoundStatement   { mergeLines [$1 ++ " " ++ $2] $3 }



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


mergeLines :: [String] -> [String] -> [String]
mergeLines l1 l2 = (init l1) ++ ((last l1 ++ " " ++ head l2) : tail l2)

indented :: [String] -> [String]
indented = map ("    " ++)


-- *****
-- LEXER

tokenise [] = []

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
    readId l = idOrKwd i : tokenise rest where
        (i, rest) = span isAlphaNum l

readString p ('\\':c:cs) = ('\\':c:ss, rest)
    where (ss, rest) = readString p cs
readString p (stripPrefix p -> Just rest) = ("", rest)
readString p (c:cs) = (c:ss, rest)
    where (ss, rest) = readString p cs

idOrKwd ((== "if") -> True)      = TIf
idOrKwd ((== "else") -> True)    = TElse
idOrKwd ((== "switch") -> True)  = TSwitch

idOrKwd ((== "while") -> True)   = TWhile
idOrKwd ((== "do") -> True)      = TDo
idOrKwd ((== "for") -> True)     = TFor

idOrKwd ((== "break") -> True)   = TBreak
idOrKwd ((== "continue") -> True)= TContinue
idOrKwd ((== "return") -> True)  = TReturn
idOrKwd ((== "goto") -> True)    = TGoto

idOrKwd ((== "static") -> True)    = TStorageClass "static"
idOrKwd ((== "extern") -> True)    = TStorageClass "extern"

idOrKwd ((== "inline") -> True)    = TFunctionSpec "inline"
idOrKwd ((== "virtual") -> True)   = TFunctionSpec "virtual"
idOrKwd ((== "explicit") -> True)  = TFunctionSpec "explicit"

idOrKwd ((== "const") -> True)    = TCVQualifier "const"
idOrKwd ((== "volatile") -> True) = TCVQualifier "volatile"

idOrKwd other = TId other

}
