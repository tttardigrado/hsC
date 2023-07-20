{
module Parser where
import Lexer
import Syntax
}

%name parser
%tokentype { Token }
%error { parseError }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!' '=' '+=' '-=' '*=' '/=' '%='
%left '+' '-'
%left '*' '/' '%'
%left '&&' '||'
%left ';'

%token
    int   { TInt  $$  }
    bool  { TBool $$  }
    var   { TIdent $$ }
    
    '+'   { TAdd      }
    '-'   { TSub      }
    '*'   { TMul      }
    '/'   { TDiv      }
    '%'   { TMod      }

    '='   { TSet      }
    '+='  { TAddEq    }
    '-='  { TSubEq    }
    '*='  { TMulEq    }
    '/='  { TDivEq    }
    '%='  { TModEq    }
    
    '=='  { TEq       }
    '!='  { TNeq      }
    '<'   { TLt       }
    '>'   { TGt       }
    '<='  { TLeq      }
    '>='  { TGeq      }
    
    '&&'  { TAnd      }
    '||'  { TOr       }
    '!'   { TNot      }

    let   { TLet      }
    print { TPrint    }
    for   { TFor      }
    skip  { TSkip     }
    if    { TIf       }
    else  { TElse     }
    while { TWhile    }
    
    ';'   { TSemi     }
    '('   { TLParen   }
    ')'   { TRParen   }
    '{'   { TLCurly   }
    '}'   { TRCurly   }

%%

Stmt  : skip ';'                                 { Skip                           }
      | let var '=' IExpr ';'                    { Let   $2 $4                    }
      | '{' Stmts '}'                            { Blk   $2                       }
      | print '(' IExpr ')' ';'                  { Print $3                       }
      | var '=' IExpr ';'                        { Set   $1 $3                    }
      | var '+=' IExpr ';'                       { Set   $1 (Aop Add (Var $1) $3) }
      | var '-=' IExpr ';'                       { Set   $1 (Aop Sub (Var $1) $3) }
      | var '*=' IExpr ';'                       { Set   $1 (Aop Mul (Var $1) $3) }
      | var '/=' IExpr ';'                       { Set   $1 (Aop Div (Var $1) $3) }
      | var '%=' IExpr ';'                       { Set   $1 (Aop Mod (Var $1) $3) }
      | while '(' BExpr ')' Stmt                 { While $3 $5                    }
      | if '(' BExpr ')' Stmt                    { If    $3 $5 Skip               }
      | if '(' BExpr ')' Stmt else Stmt          { If    $3 $5 $7                 }
      | for '(' var ';' IExpr ';' IExpr ')' Stmt { Blk   [Let $3 $5, While (Cop Lt (Var $3) $7) (Blk [$9, Set $3 (Aop Add (Var $3) (Int 1))])]}
      

Stmts : {- empty -}                              { []        }
      | Stmt Stmts                               { ($1 : $2) }


IExpr : var                                      { Var $1        }
      | int                                      { Int $1        }
      | IExpr '+' IExpr                          { Aop Add $1 $3 }
      | IExpr '-' IExpr                          { Aop Sub $1 $3 }
      | IExpr '*' IExpr                          { Aop Mul $1 $3 }
      | IExpr '/' IExpr                          { Aop Div $1 $3 }
      | IExpr '%' IExpr                          { Aop Mod $1 $3 }
      | '(' IExpr ')'                            { $2            }


BExpr : bool                                     { Bool $1       }
      | IExpr '=='  IExpr                        { Cop Eq  $1 $3 }
      | IExpr '!='  IExpr                        { Cop Neq $1 $3 }
      | IExpr '<'   IExpr                        { Cop Lt  $1 $3 }
      | IExpr '>'   IExpr                        { Cop Gt  $1 $3 }
      | IExpr '<='  IExpr                        { Cop Leq $1 $3 }
      | IExpr '>='  IExpr                        { Cop Geq $1 $3 }
      | BExpr '&&'  BExpr                        { Bop And $1 $3 }
      | BExpr '||'  BExpr                        { Bop Or  $1 $3 }
      |       '!'   BExpr                        { Not $2        }
      | '(' BExpr ')'                            { $2            }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}