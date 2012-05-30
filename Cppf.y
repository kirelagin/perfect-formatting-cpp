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
    CLASSKEY        { TClassKey $$ }
    
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
    '?'             { TQuestion }
    ':'             { TColon }
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
    TYPEID          { TTypeId $$ }
    STRING          { TString $$ }

%%

TranslationUnit :: { String }
                : {- empty -}    { [] }
                | DeclarationSeq { intercalate "\n" $1 ++ "\n" }


-- Expressions

Literal :: { String }
        : NUM               { $1 }
        | '\'' STRING '\''  { "'" ++ $2 ++ "'" }
        | '"' STRING '"'    { "\"" ++ $2 ++ "\"" }

PrimaryExpression :: { String }
                  : Literal             { $1 }
                  | '(' Expression ')'  { "(" ++ $2 ++ ")" }
                  | ID                  { $1 }

PostfixExpression :: { String }
                  : PrimaryExpression                               { $1 }
                  | PostfixExpression '[' Expression ']'            { $1 ++ "[" ++ $3 ++ "]" }
                  | PostfixExpression '(' ExpressionList ')'        { $1 ++ "(" ++ $3 ++ ")" }
                  | PostfixExpression '(' ')'                       { $1 ++ "()" }
                  | PostfixExpression '.' ID                        { $1 ++ "." ++ $3 }
                  | PostfixExpression '->' ID                       { $1 ++ "->" ++ $3 }
                  | PostfixExpression '++'                          { $1 ++ "++" }
                  | PostfixExpression '--'                          { $1 ++ "--" }

ExpressionList :: { String }
               : AssignmentExpression ',' ExpressionList    { $1 ++ ", " ++ $3 }
               | AssignmentExpression                       { $1 }

UnaryExpression :: { String }
                : PostfixExpression             { $1 }
                | '++' CastExpression           { "++" ++ $2 }
                | '--' CastExpression           { "--" ++ $2 }
                | UnaryOperator CastExpression  { $1 ++ $2 }

UnaryOperator :: { String }
              : '*' { "*" }
              | '&' { "&" }
              | '+' { "+" }
              | '-' { "-" }
              | '!' { "!" }
              | '~' { "~" }

CastExpression :: { String }
               : UnaryExpression    { $1 }

PmExpression :: { String }
             : CastExpression       { $1 }

MultiplicativeExpression :: { String }
                         : PmExpression                                 { $1 }
                         | MultiplicativeExpression '*' PmExpression    { $1 ++ " * " ++ $3 }
                         | MultiplicativeExpression '/' PmExpression    { $1 ++ " / " ++ $3 }
                         | MultiplicativeExpression '%' PmExpression    { $1 ++ " % " ++ $3 }

AdditiveExpression :: { String }
                   : MultiplicativeExpression            { $1 }
                   | AdditiveExpression '+' MultiplicativeExpression     { $1 ++ " + " ++ $3 }
                   | AdditiveExpression '-' MultiplicativeExpression     { $1 ++ " - " ++ $3 }

ShiftExpression :: { String }
                : AdditiveExpression                        { $1 }
                | ShiftExpression '<<' AdditiveExpression   { $1 ++ " << " ++ $3 }
                | ShiftExpression '>>' AdditiveExpression   { $1 ++ " >> " ++ $3 }

RelationalExpression :: { String }
                     : ShiftExpression                              { $1 }
                     | RelationalExpression '<' ShiftExpression     { $1 ++ " < " ++ $3 }
                     | RelationalExpression '>' ShiftExpression     { $1 ++ " > " ++ $3 }
                     | RelationalExpression '<=' ShiftExpression     { $1 ++ " <= " ++ $3 }
                     | RelationalExpression '>=' ShiftExpression     { $1 ++ " >= " ++ $3 }

EqualityExpression :: { String }
                   : RelationalExpression                           { $1 }
                   | EqualityExpression '==' RelationalExpression   { $1 ++ " == " ++ $3 }
                   | EqualityExpression '!=' RelationalExpression   { $1 ++ " != " ++ $3 }

AndExpression :: { String }
              : EqualityExpression                      { $1 }
              | AndExpression '&' EqualityExpression    { $1 ++ " & " ++ $3 }

ExclusiveOrExpression :: { String }
                      : AndExpression                           { $1 }
                      | ExclusiveOrExpression '^' AndExpression { $1 ++ " ^ " ++ $3 }

InclusiveOrExpression :: { String }
                      : ExclusiveOrExpression                           { $1 }
                      | InclusiveOrExpression '|' ExclusiveOrExpression { $1 ++ " | " ++ $3 }

LogicalAndExpression :: { String }
                     : InclusiveOrExpression                            { $1 }
                     | LogicalAndExpression '&&' InclusiveOrExpression  { $1 ++ " && " ++ $3 }

LogicalOrExpression :: { String }
                    : LogicalAndExpression                          { $1 }
                    | LogicalOrExpression '||' LogicalAndExpression { $1 ++ " || " ++ $3 }

ConditionalExpression :: { String }
                      : LogicalOrExpression                                         { $1 }
                      | LogicalOrExpression '?' Expression ':' AssignmentExpression { $1 ++ " ? " ++ $3 ++ " : " ++ $5 }

AssignmentExpression :: { String }
                     : ConditionalExpression                                        { $1 }
                     | LogicalOrExpression AssignmentOperator AssignmentExpression  { $1 ++ " " ++ $2 ++ " " ++ $3 }

AssignmentOperator :: { String }
                   : '='   { "=" }

Expression :: { String }
           : AssignmentExpression ',' Expression    { $1 ++ ", " ++ $3 }
           | AssignmentExpression                   { $1 }

-- Statements

Statement :: { [String] }
          : DeclarationStatement    { $1 }
          | CompoundStatement       { $1 }
          | ExpressionStatement     { $1 }
          | SelectionStatement      { $1 }
--          | IterationStatement      { $1 }
          | JumpStatement           { $1 }

ExpressionStatement :: { [String] }
                    : Expression ';'    { [$1 ++ ";"] }
                    | ';'               { [";"] }

CompoundStatement :: { [String] }
                  : '{' StatementSeq '}'    { "{" : (indented $2 ++ ["}"]) }
                  | '{' '}'                 { ["{}"] }

StatementSeq :: { [String] }
             : Statement StatementSeq   { $1 ++ $2 }
             | Statement                { $1 }

SelectionStatement :: { [String] }
                   : IF '(' Condition ')' CompoundStatement ELSE CompoundStatement  { mergeManyLines [["if (" ++ $3 ++ ")"], $5, ["else"], $7] }
                   | IF '(' Condition ')' Statement                 { mergeLines ["if (" ++ $3 ++ ")"] $5 }

Condition :: { String }
          : Expression                                          { $1 }
          | MyTypeSpecifier Declarator '=' AssignmentExpression { $1 ++ $2 ++ " = " ++ $4 }

JumpStatement :: { [String ] }
              : BREAK ';'               { ["break;"] }
              | CONTINUE ';'            { ["continue;"] }
              | RETURN Expression ';'   { ["return " ++ $2 ++ ";"] }
              | RETURN ';'              { ["return;"] }

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
--                : DeclSpecifierSeq TypeSpecifier    { $1 ++ " " ++ $2 }
--                | TypeSpecifier                     { $1 }
                : DeclSpecifierSeq  { $1 }

DeclSpecifierSeq :: { String }
                 : DeclSpecifier DeclSpecifierSeq   { $1 ++ " " ++ $2 }
                 | DeclSpecifier                    { $1 }

DeclSpecifier :: { String }
              : STORAGECLASS    { $1 }
              | FUNCTIONSPEC    { $1 }
              | CVQUALIFIER     { $1 }
              | TypeSpecifier   { $1 }

TypeSpecifier :: { String }
              : TYPEID          { $1 }
              | CLASSKEY TYPEID { $1 ++ " " ++ $2 }

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
            : '=' InitializerClause {  "= " ++ $2 }

InitializerClause :: { String }
                  : AssignmentExpression        { $1 }
                  | '{' InitializerList '}'     { "{" ++ $2 ++ "}" }
                  | '{' '}'                     { "{}" }

InitializerList :: { String }
                : InitializerClause ',' InitializerList { $1 ++ ", " ++ $3 }
                | InitializerClause                     { $1 }

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
           | TClassKey String
           
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
           | TQuestion
           | TColon
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
           | TTypeId    String
           | TString    String
    deriving (Show, Eq)


mergeLines :: [String] -> [String] -> [String]
mergeLines l1 l2 = (init l1) ++ ((last l1 ++ " " ++ head l2) : tail l2)

mergeManyLines :: [[String]] -> [String]
mergeManyLines = foldr1 mergeLines

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
tokenise ('?':cs) = TQuestion   : tokenise cs
tokenise (':':cs) = TColon      : tokenise cs

tokenise ('\'':cs) = TSQuote : TString s : TSQuote : tokenise rest
    where (s, rest) = readString "'" cs
tokenise ('"':cs) = TDQuote  : TString s : TDQuote : tokenise rest
    where (s, rest) = readString "\"" cs

tokenise l@(c:cs)
    | isSpace c = tokenise cs
    | isDigit c = readNum l
    | isNonDigit c = readId l
    | otherwise = error $ "Unexpected '" ++ (c : "'")
 where
    readNum l = TNum n : tokenise rest where
        (n, rest) = span (liftM2 (||) (=='.') isDigit) l
    readId l = idOrSmth i : tokenise rest where
        (i, rest) = span isGoodForId l

isNonDigit = liftM2 (||) isAlpha (=='_')
isGoodForId = liftM2 (||) isAlphaNum (=='_')

readString p ('\\':c:cs) = ('\\':c:ss, rest)
    where (ss, rest) = readString p cs
readString p (stripPrefix p -> Just rest) = ("", rest)
readString p (c:cs) = (c:ss, rest)
    where (ss, rest) = readString p cs

idOrSmth i
  | i == "if"     = TIf
  | i == "else"   = TElse
  | i == "switch" = TSwitch

  | i == "while" = TWhile
  | i == "do"    = TDo
  | i == "for"   = TFor

  | i == "break"    = TBreak
  | i == "continue" = TContinue
  | i == "return"   = TReturn
  | i == "goto"     = TGoto

  | i == "static" = TStorageClass i
  | i == "extern" = TStorageClass i

  | i == "inline"   = TFunctionSpec i
  | i == "virtual"  = TFunctionSpec i
  | i == "explicit" = TFunctionSpec i

  | i == "const"    = TCVQualifier i
  | i == "volatile" = TCVQualifier i

  | i == "class"  = TClassKey i
  | i == "struct" = TClassKey i
  | i == "union"  = TClassKey i

  | i == "char"     = TTypeId i
  | i == "bool"     = TTypeId i
  | i == "short"    = TTypeId i
  | i == "int"      = TTypeId i
  | i == "long"     = TTypeId i
  | i == "signed"   = TTypeId i
  | i == "unsigned" = TTypeId i
  | i == "float"    = TTypeId i
  | i == "double"   = TTypeId i
  | i == "void"     = TTypeId i

idOrSmth i@(c:cs) | isUpper c = TTypeId i
idOrSmth other = TId other

}
