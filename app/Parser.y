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
      | '{' Stmts '}'                          { Blk   $2                       } 
      | let var ':' Type '=' Expr ';'          { Let   $2 $4 $6                 }
      | print '(' Expr ')' ';'                 { Print $3                       }
      | var '='  Expr ';'                      { Set   $1 $3                    }
      | var '+=' Expr ';'                      { Set   $1 (Aop Add (Var $1) $3) }
      | var '-=' Expr ';'                      { Set   $1 (Aop Sub (Var $1) $3) }
      | var '*=' Expr ';'                      { Set   $1 (Aop Mul (Var $1) $3) }
      | var '/=' Expr ';'                      { Set   $1 (Aop Div (Var $1) $3) }
      | var '%=' Expr ';'                      { Set   $1 (Aop Mod (Var $1) $3) }
      | while '(' Expr ')' Stmt                { While $3 $5                    }
      | if '(' Expr ')' Stmt                   { If    $3 $5 (Blk [])           }
      | if '(' Expr ')' Stmt else Stmt         { If    $3 $5 $7                 }
      | for '(' var ';' Expr ';' Expr ')' Stmt { For   $3 $5 $7 $9              }

Type  : tint                                   { TyInt     }
      | tbool                                  { TyBool    }

Stmts : {- empty -}                            { []        }
      | Stmt Stmts                             { ($1 : $2) }

Expr  : var                                    { Var  $1       }
      | int                                    { Int  $1       }
      | bool                                   { Bool $1       }
      | Expr '+' Expr                          { Aop Add $1 $3 }
      | Expr '-' Expr                          { Aop Sub $1 $3 }
      | Expr '*' Expr                          { Aop Mul $1 $3 }
      | Expr '/' Expr                          { Aop Div $1 $3 }
      | Expr '%' Expr                          { Aop Mod $1 $3 }
      | Expr '=='  Expr                        { Cop Eq  $1 $3 }
      | Expr '!='  Expr                        { Cop Neq $1 $3 }
      | Expr '<'   Expr                        { Cop Lt  $1 $3 }
      | Expr '>'   Expr                        { Cop Gt  $1 $3 }
      | Expr '<='  Expr                        { Cop Leq $1 $3 }
      | Expr '>='  Expr                        { Cop Geq $1 $3 }
      | Expr '&&'  Expr                        { Bop And $1 $3 }
      | Expr '||'  Expr                        { Bop Or  $1 $3 }
      |       '!'  Expr                        { Not $2        }
      | Expr '?' Expr ':' Expr                 { EIf $1 $3 $5  }
      | '(' Expr ')'                           { $2            }

{

parseError :: [Token] -> a
parseError t = error $ "Parse error " ++ show t

}