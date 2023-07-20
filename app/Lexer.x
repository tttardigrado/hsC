{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
$white+                                     ;
"//".*                                      ;

-- Attributions
"="                                         { \_ -> TSet   }
"+="                                        { \_ -> TAddEq }
"-="                                        { \_ -> TSubEq }
"*="                                        { \_ -> TMulEq }
"/="                                        { \_ -> TDivEq }
"%="                                        { \_ -> TModEq }

-- Arithmetic Ops
"+"                                         { \_ -> TAdd }
"-"                                         { \_ -> TSub }
"*"                                         { \_ -> TMul }
"/"                                         { \_ -> TDiv }
"%"                                         { \_ -> TMod }

-- Boolean Ops
"&&"                                        { \_ -> TAnd }
"||"                                        { \_ -> TOr  }
"!"                                         { \_ -> TNot }


--Comparations
"=="                                        { \_ -> TEq  }
"!="                                        { \_ -> TNeq }
"<"                                         { \_ -> TLt  }
">"                                         { \_ -> TGt  }
"<="                                        { \_ -> TLeq }
">="                                        { \_ -> TGeq }

-- Constants (Ints && Bools)
$digit+                                     { \s -> TInt (read s) }
"true"                                      { \_ -> TBool True    }
"false"                                     { \_ -> TBool False   }


-- Keywords
"if"                                        { \_ -> TIf    }
"else"                                      { \_ -> TElse  }
"while"                                     { \_ -> TWhile }
"print"                                     { \_ -> TPrint }
"let"                                       { \_ -> TLet   }
"skip"                                      { \_ -> TSkip  }
"for"                                       { \_ -> TFor   }

-- Identifier
[$alpha \_] [$alpha $digit \_]*             { \s -> TIdent s }

-- Punctuation
"("                                         { \_ -> TLParen }
")"	                                        { \_ -> TRParen }
"{"                                         { \_ -> TLCurly }
"}"	                                        { \_ -> TRCurly }
";"	                                        { \_ -> TSemi   }


{

data Token
  = TInt  Int       -- int value
  | TBool Bool      -- bool value
  | TIdent String   -- variable identifier
  | TSet            -- :=
  | TAddEq          -- +=
  | TSubEq          -- -=
  | TMulEq          -- *=
  | TDivEq          -- /=
  | TModEq          -- %=
  | TLet            -- let
  | TAdd            -- +
  | TSub            -- -
  | TMul            -- *
  | TDiv            -- /
  | TMod            -- %
  | TEq             -- ==
  | TNeq            -- !=
  | TLt             -- <
  | TGt             -- >
  | TLeq            -- <=
  | TGeq            -- >=
  | TAnd            -- &&
  | TOr             -- ||
  | TNot            -- !
  | TLParen         -- (
  | TRParen         -- )
  | TLCurly         -- {
  | TRCurly         -- }
  | TSemi           -- ;
  | TIf             -- if
  | TThen           -- then
  | TElse           -- else
  | TWhile          -- while
  | TFor            -- for
  | TPrint          -- print
  | TSkip           -- skip
  deriving Show

scanTokens = alexScanTokens

}