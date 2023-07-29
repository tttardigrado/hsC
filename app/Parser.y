{
module Parser where
import Lexer
import Syntax
}

%name parser
%tokentype { Token }
%error { parseError }

%nonassoc ':' '?'
%nonassoc '=' '+=' '-=' '*=' '/=' '%='
%left '&&' '||'
%nonassoc '>' '<' '<=' '>=' '==' '!=' '!' 
%left '+' '-'
%left '*' '/' '%'

%token
    int      { TInt  $$  }
    bool     { TBool $$  }
    var      { TIdent $$ }

    tint     { TTInt     }
    tbool    { TTBool    }
    tvoid    { TTVoid    }
    
    '+'      { TAdd      }
    '-'      { TSub      }
    '*'      { TMul      }
    '/'      { TDiv      }
    '%'      { TMod      }

    '='      { TSet      }
    '+='     { TAddEq    }
    '-='     { TSubEq    }
    '*='     { TMulEq    }
    '/='     { TDivEq    }
    '%='     { TModEq    }
    
    '=='     { TEq       }
    '!='     { TNeq      }
    '<'      { TLt       }
    '>'      { TGt       }
    '<='     { TLeq      }
    '>='     { TGeq      }
    
    '&&'     { TAnd      }
    '||'     { TOr       }
    '!'      { TNot      }

    let      { TLet      }
    print    { TPrint    }
    for      { TFor      }
    if       { TIf       }
    else     { TElse     }
    while    { TWhile    }
    break    { TBreak    }
    continue { TCont     }

    ';'      { TSemi     }
    '('      { TLParen   }
    ')'      { TRParen   }
    '{'      { TLCurly   }
    '}'      { TRCurly   }
    ':'      { TDColon   }
    '?'      { TQMark    }

%%

Stmt  : break ';'                              { Break                          }
      | continue ';'                           { Continue                       } 
      | Expr ';'                               { ExpStm $1                      }
      | '{' Stmts '}'                          { Blk   $2                       } 
      | let var ':' Type '=' Expr ';'          { Let   $2 $4 $6                 }
      | var '='  Expr ';'                      { Set   $1 $3                    }
      | var '+=' Expr ';'                      { Set   $1 (BOp Add (Var $1) $3) }
      | var '-=' Expr ';'                      { Set   $1 (BOp Sub (Var $1) $3) }
      | var '*=' Expr ';'                      { Set   $1 (BOp Mul (Var $1) $3) }
      | var '/=' Expr ';'                      { Set   $1 (BOp Div (Var $1) $3) }
      | var '%=' Expr ';'                      { Set   $1 (BOp Mod (Var $1) $3) }
      | while '(' Expr ')' Stmt                { While $3 $5                    }
      | if '(' Expr ')' Stmt                   { If    $3 $5 (Blk [])           }
      | if '(' Expr ')' Stmt else Stmt         { If    $3 $5 $7                 }
      | for '(' var ';' Expr ';' Expr ')' Stmt { For   $3 $5 $7 $9              }

Type  : tint                                   { IntT  }
      | tbool                                  { BoolT }
      | tvoid                                  { VoidT }

Stmts : {- empty -}                            { []        }
      | Stmt Stmts                             { ($1 : $2) }

Expr  : var                                    { Var  $1       }
      | int                                    { Int  $1       }
      | bool                                   { Bool $1       }
      | Expr '+'  Expr                         { BOp Add $1 $3 }
      | Expr '-'  Expr                         { BOp Sub $1 $3 }
      | Expr '*'  Expr                         { BOp Mul $1 $3 }
      | Expr '/'  Expr                         { BOp Div $1 $3 }
      | Expr '%'  Expr                         { BOp Mod $1 $3 }
      | Expr '==' Expr                         { BOp Eq  $1 $3 }
      | Expr '!=' Expr                         { BOp Neq $1 $3 }
      | Expr '<'  Expr                         { BOp Lt  $1 $3 }
      | Expr '>'  Expr                         { BOp Gt  $1 $3 }
      | Expr '<=' Expr                         { BOp Leq $1 $3 }
      | Expr '>=' Expr                         { BOp Geq $1 $3 }
      | Expr '&&' Expr                         { BOp And $1 $3 }
      | Expr '||' Expr                         { BOp Or  $1 $3 }
      |       '!' Expr                         { UOp Not $2    }
      |       '-' Expr                         { UOp Neg $2    }
      | print '(' Expr ')'                     { UOp Print $3  }
      | Expr  '?' Expr ':' Expr                { EIf $1 $3 $5  }
      | '(' Expr ')'                           { $2            }

{

parseError :: [Token] -> a
parseError t = error $ "Parse error " ++ show t

}