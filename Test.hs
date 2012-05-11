{-# LANGUAGE QuasiQuotes #-}

import Data.Maybe
import Test.HUnit

import Cppf

testLexer = "Lexer" ~: test $
    [ tokenise "\
      \  int a;\
    \ " ~?= [
        TId "int", TId "a", TSemicolon
      ]
    , tokenise "\
    \   void *a;\
    \   int bla123;\
    \ " ~?= [
        TId "void", TAst, TId "a", TSemicolon,
        TId "int", TId "bla123", TSemicolon
      ]
    , tokenise "\
    \   if (avar + 2 == 81) {\
    \       bool81a  = false ;\
    \   }\
    \ " ~?= [
        TIf, TParenL, TId "avar", TPlus, TNum "2", TEq, TNum "81", TParenR, TBraceL,
            TId "bool81a", TAssign, TId "false", TSemicolon,
        TBraceR
      ]
    , tokenise "\
    \   void* foo(int a, int &b, int& c)\
    \   {\
    \       char c = 'w';\
    \       c = (int)a%(bool)42;\
    \       return *d;\
    \   }\
    \ " ~?= [
        TId "void", TAst, TId "foo", TParenL, TId "int", TId "a", TComma, TId "int", TAmp, TId "b", TComma, TId "int", TAmp, TId "c", TParenR,
        TBraceL,
            TId "char", TId "c", TAssign, TSQuote, TString "w", TSQuote, TSemicolon,
            TId "c", TAssign, TParenL, TId "int", TParenR, TId "a", TPercent, TParenL, TId "bool", TParenR, TNum "42", TSemicolon,
            TReturn, TAst, TId "d", TSemicolon,
        TBraceR
      ]
    , tokenise "\
    \   char* str1 = \"hello\"\"world!\";\
    \ " ~?= [
        TId "char", TAst, TId "str1", TAssign, TDQuote, TString "hello", TDQuote, TDQuote, TString "world!", TDQuote, TSemicolon
      ]
    , tokenise "\
    \   char *str2 = \"hi\\nthere\";\
    \ " ~?= [
        TId "char", TAst, TId "str2", TAssign, TDQuote, TString "hi\\nthere", TDQuote, TSemicolon
      ]
    , tokenise "\
    \   char* str3 = \"now\\\"quote\\\"\";\
    \ " ~?= [
        TId "char", TAst, TId "str3", TAssign, TDQuote, TString "now\\\"quote\\\"", TDQuote, TSemicolon
      ]
    ]

--testParser = "Parser" ~: test $
--    [
--    -- From example
--      shouldParse "1 2 10 + *"
--    -- E -> nE', E' -> eps
--    , parse "1" ~?== Node NtE [Leaf Tnum, Node NtE' []]
--    -- E -> nE', E' =>* n+eps
--    , parse "1 1 +" ~?== Node NtE [Leaf Tnum, Node NtE' [Node NtE [Leaf Tnum, Node NtE' []], Node NtA [Leaf Tplus], Node NtE' []]]
--    , parse "1 1 -" ~?== Node NtE [Leaf Tnum, Node NtE' [Node NtE [Leaf Tnum, Node NtE' []], Node NtA [Leaf Tminus], Node NtE' []]]
--    , parse "1 1 *" ~?== Node NtE [Leaf Tnum, Node NtE' [Node NtE [Leaf Tnum, Node NtE' []], Node NtA [Leaf Tmul], Node NtE' []]]
--    -- E -> nE', E' =>* n*E', E' =>* nn+-
--    , parse "1 1 * 1 1 + -" ~?==
--        Node NtE [Leaf Tnum, Node NtE' [Node NtE [Leaf Tnum, Node NtE' []], Node NtA [Leaf Tmul], Node NtE' [Node NtE [Leaf Tnum, Node NtE' [Node NtE [Leaf Tnum, Node NtE' []], Node NtA [Leaf Tplus], Node NtE' []]], Node NtA [Leaf Tminus], Node NtE' []]]]
--    -- E -> nE', E' =>* E-eps
--    , parse "1 1  1+-" ~?==
--        Node NtE [Leaf Tnum,Node NtE' [Node NtE [Leaf Tnum,Node NtE' [Node NtE [Leaf Tnum,Node NtE' []],Node NtA [Leaf Tplus],Node NtE' []]],Node NtA [Leaf Tminus],Node NtE' []]]
--    -- Unexpected Tend
--    , shouldFail "1 1 + 1"
--    , shouldFail "n n"
--    , shouldFail "1 1 + 1 1 *"
--    -- Illegal token
--    , shouldFail "1 1 %"
--    -- More tokens when end Tend expected
--    , shouldFail "1 1 + *"
--    ]

main = runTestTT $ test [testLexer{-, testParser-}]

--actual ~?== expected = (actual :: Either ParseError Tree) ~?= (Right expected)
--shouldParse s = isJust (parse s) ~? "Failed parsing valid expression: " ++ show s
--shouldFail s = isNothing (parse s) ~? "Parsed invalid expression: " ++ show s
