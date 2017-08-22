open Syntax
open Util

let (standard_init_env, standard_init_tyenv) =
  let standard_ids = [
    "zero", Zero, typfun 0;
    "succ", Succ, typfun 1;
  ] in
  List.fold_left
    (fun (env, tyenv) (id, exp, ty) ->
      (Environment.extend id (Eval.eval env exp) env,
       Environment.extend id ty tyenv))
    (Environment.empty, Environment.empty)
    standard_ids

