open Syntax

type program =
  ProgBind of (id * Eval.expval * ty)
| ProgPrint of (Eval.expval * ty)

let rec exec f env tyenv = function
  [] -> (env, tyenv)
| (LetDecl (id, expr))::prog ->
    let expr  = Eval.eval env expr
    and ty_e  = Typing.eval_ty tyenv expr in
    let env   = Environment.extend id expr env
    and tyenv = Environment.extend id ty_e tyenv in
    f (ProgBind(id, expr, ty_e));
    exec f env tyenv prog
| (PrintStmt expr)::prog ->
    let ty_e  = Typing.eval_ty tyenv expr
    and value = Eval.eval env expr in
    f (ProgPrint(value, ty_e));
    exec f env tyenv prog
