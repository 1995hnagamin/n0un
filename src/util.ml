open Syntax

let is_same_as f = function
  [] -> true
| x::xs ->
    let a = f x in
    List.for_all (fun x -> f x = a) xs

let is_different f xs = not (is_same_as f xs)

let (<<) g f x = g(f(x))

let comp g f = Comp(g, [f])

let app f x = App(f, [Int x])

let apps f xs = App(f, List.map (fun x -> Int x) xs)

let proj x y = Proj(x, Arity.exact y)

let proj_variadic x =
  Proj(x, Arity.at_least x)

let typfun n = TyPFun (Arity.exact n)

let typfun_variadic n = TyPFun (Arity.at_least n)
