{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
$white+                                     ;
"//".*                                      ;

-- Types
"int"                                       { \_ -> TTInt  }
"bool"                                      { \_ -> TTBool }
"void"                                      { \_ -> TTVoid }

-- Attributions
"="                                         { \_ -> TSet   }
"+="                                        { \_ -> TAddEq }
"-="                                        { \_ -> TSubEq }
"*="                                        { \_ -> TMulEq }
"/="                                        { \_ -> TDivEq }
"%="                                        { \_ -> TModEq }
"<<="                                       { \_ -> TSLEq }
">>="                                       { \_ -> TSREq }

-- Arithmetic Ops
"+"                                         { \_ -> TAdd }
"-"                                         { \_ -> TSub }
"*"                                         { \_ -> TMul }
"/"                                         { \_ -> TDiv }
"%"                                         { \_ -> TMod }
"<<"                                        { \_ -> TSL  }
">>"                                        { \_ -> TSR  }

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
"True"                                      { \_ -> TBool True    }
"False"                                     { \_ -> TBool False   }


-- Keywords
"if"                                        { \_ -> TIf    }
"else"                                      { \_ -> TElse  }
"while"                                     { \_ -> TWhile }
"print"                                     { \_ -> TPrint }
"let"                                       { \_ -> TLet   }
"for"                                       { \_ -> TFor   }
"break"                                     { \_ -> TBreak }
"continue"                                  { \_ -> TCont  }
"fun"                                       { \_ -> TFun   }
"return"                                    { \_ -> TRet   }

-- Identifier
[$alpha \_] [$alpha $digit \_]*             { \s -> TIdent s }

-- Punctuation
"("                                         { \_ -> TLParen }
")"	                                        { \_ -> TRParen }
"{"                                         { \_ -> TLCurly }
"}"	                                        { \_ -> TRCurly }
";"	                                        { \_ -> TSemi   }
":"	                                        { \_ -> TDColon }
"?"                                         { \_ -> TQMark  }
","                                         { \_ -> TComma  }

{

data Token
  = TInt  Int       -- int value
  | TBool Bool      -- bool value
  | TIdent String   -- variable identifier
  -- Types
  | TTInt           -- int
  | TTBool          -- bool
  | TTVoid          -- void
  -- Assignement
  | TSet            -- =
  | TAddEq          -- +=
  | TSubEq          -- -=
  | TMulEq          -- *=
  | TDivEq          -- /=
  | TModEq          -- %=
  | TSLEq           -- <<=
  | TSREq           -- >>=
  -- Operations
  | TAdd            -- +
  | TSub            -- -
  | TMul            -- *
  | TDiv            -- /
  | TMod            -- %
  | TSL             -- <<
  | TSR             -- >>
  | TEq             -- ==
  | TNeq            -- !=
  | TLt             -- <
  | TGt             -- >
  | TLeq            -- <=
  | TGeq            -- >=
  | TAnd            -- &&
  | TOr             -- ||
  | TNot            -- !
  -- Punctuation
  | TLParen         -- (
  | TRParen         -- )
  | TLCurly         -- {
  | TRCurly         -- }
  | TSemi           -- ;
  | TDColon         -- :
  | TQMark          -- ?
  | TComma          -- ,
  -- Identifiers
  | TLet            -- let
  | TIf             -- if
  | TThen           -- then
  | TElse           -- else
  | TWhile          -- while
  | TFor            -- for
  | TBreak          -- break
  | TCont           -- continue
  | TPrint          -- print
  | TFun            -- fun
  | TRet            -- return
  deriving (Show)


scanTokens = alexScanTokens

}