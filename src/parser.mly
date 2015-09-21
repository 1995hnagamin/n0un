%{
open Syntax

%}
%token LPAREN RPAREN LBRAKET RBRAKET
%token RARROW DOT COMMA SEMISEMI
%token ZERO SUCC

%token <int> INTV
%token <Syntax.id> ID
%token <(int * int)> PROJECTOR

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel : Program { $1 }

Program :
  Stmt { $1 }

Stmt :
  Expr SEMISEMI { $1 }

Expr :
  RExpr { $1 }

RExpr :
  DExpr RARROW RExpr { (PRec ($1, $3)) }
| DExpr { $1 }

DExpr :
  AExpr DOT DExpr { Comp($1, [$3]) }
| AExpr { $1 }

AExpr :
  CExpr LPAREN RPAREN { App ($1, []) }
| CExpr LPAREN Composee RPAREN { App ($1, $3) }
| CExpr { $1 }

CExpr :
  IExpr LBRAKET RBRAKET { (Comp ($1, [])) }
| IExpr LBRAKET Composee RBRAKET { (Comp ($1, $3)) }
| IExpr { $1 }

Composee :
  Expr { [$1] }
| Expr COMMA Composee { $1::$3 }

IExpr :
  INTV { Int $1 }
| ZERO { Zero }
| SUCC { Succ }
| Projection { $1 }
| LPAREN Expr RPAREN { $2 }

Projection :
  PROJECTOR { Proj $1 }
