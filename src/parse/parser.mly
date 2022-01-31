%token RARROW
%token LARROW
%token COMMA DOT
%token <int> INT
%token <string> IFER
%token <bool> BOOLLIT
%token IF THEN ELSE
%token FUN
%token DO IN
%token WITH HANDLE
%token HANDLER OPSEP
%token PLUS MINUS TIMES BANG
%token GT LT EQ
%token LPAREN RPAREN
%token LBRC RBRC
%token EOL

%start <Syntax.Exp.t> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| e = boolean
    { e }
| i1 = ifer LPAREN elist = exprlist OPSEP i2 = ifer DOT e = expr RPAREN
    { Exp.EffInv (i1, elist, i2, e) }
| DO i = ifer LARROW e1 = expr IN e2 = expr
    { Exp.DoIn (i, e1, e2) }
| WITH e1 = expr HANDLE e2 = expr
    { Exp.With (e1, e2) }
| HANDLER LBRC elist = effectlist RBRC
    { Exp.Handler (elist) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Exp.If (e1, e2, e3) }
| FUN i = ifer RARROW e = expr
    { Exp.Fun (i, e) }

effect:
| i1 = ifer LPAREN plist = paramlist OPSEP i2 = ifer RPAREN RARROW e = expr
     {(i1, plist, i2, e) } 

effectlist:
| eff = effect
    { [eff] } 
| eff = effect COMMA elist = effectlist
    { eff  :: elist }

exprlist:
| e = expr
    { [e] }
| e = expr COMMA elist = exprlist
    { e :: elist }

paramlist:
| i = ifer
    { [i] }
| i =  ifer COMMA ilist = paramlist 
    { i :: ilist }

boolean:
| e = arith
    { e }
| e1 = arith GT e2 = arith
    { Exp.Prim2 (e1, Exp.OpGt, e2) }
| e1 = arith LT e2 = arith
    { Exp.Prim2 (e1, Exp.OpLt, e2) }
| e1 = arith EQ e2 = arith
    { Exp.Prim2 (e1, Exp.OpEq, e2) }

arith:
| e = factor
    { e }
| e1 = arith PLUS e2 = factor
    { Exp.Prim2 (e1, Exp.OpAdd,e2) }
| e1 = arith MINUS e2 = factor
    { Exp.Prim2 (e1, Exp.OpSub, e2) }

factor:
| e = app
    { e }
| e1 = factor TIMES e2 = app
    { Exp.Prim2 (e1, Exp.OpMult, e2) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { Exp.Prim2 (e1, OpAp, e2) }
| MINUS e = simple
    { Exp.Prim1 (OpNeg, e) }
| BANG e = simple
    { Exp.Prim1 (OpNot, e) }


simple:
| e = ifer
    { Exp.Var (e) }
| i = INT
    { Exp.NumLit i }
| b = BOOLLIT
    { Exp.BoolLit b }
| LPAREN e = expr RPAREN
    { e }
| LPAREN RPAREN
    { Exp.Unit }

ifer:
| i = IFER
    { i }
