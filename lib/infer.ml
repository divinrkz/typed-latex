

(*
   Environment is a map that holds type information of variables.
   TODO: Implementing global environment and local environment *** 
   *)
module NameMap = Map.make(String)
type environment = primitiveType NameMap.t

type substitutions = (id * primitiveType) list

let type_variable = ref (Char.code 'a')
(*
   Generate unknown type placeholder and returns T(string)
   of the generated alphabet*)
let gen_new_type () = 
  let c1 = !type_variable in 
  incr type_variable; T(Char.escaped (Char.chr c1))



(*
  Annotate expressions.
  e: an expression that has to be annotated.
  env: An environment map that holds type information of user defined variables.


  returns: returns an annotated expression of type aexpr that hold type informnation given
  provided [e]
   *)
let annotate_expr (e: expr ) (env: environment): aexpr = 
  match e with 
  | NumLit(n) -> ANumLit(n, TNum)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | Val(x) -> if NameMap.mem x env 
              then AVal(x, NameMap.find x env)
              else raise (failwith "Variable is not defined")
  | Binop(e1, op, e2) -> 
      let et1 = annotate_expr e1 env
      and et2 = annotate_expr e1 env
      and new_type = gen_new_type () in
        ABinop(et1, op, et2, new_type)
  | Fun (id, e) ->
    let ae = annotate_expr e env in 
    let t = NameMap.find id env in 
    AFun(id, ae, TFun(t, gen_new_type ()))
  | App(fn, arg) -> 
    let afn = annotate_expr fn env in 
    let aarg = annotate_expr arg env in 
    AApp(afn, aarg, gen_new_type ())

  and type_of (ae: aexpr): primitiveType = 
    match ae with 
    | ANumLit(_, t) | ABoolLit(_, t) -> t
    | AVal(_, t) -> t
    | ABinop(_, _, _, t) -> t
    | AFun (_, _, t) -> t
    | AApp (_, _, t) -> t





(* Runs Hindler Milner Type System subsequently:
  1. Annotating expressions with placeholder_types
  2. Generating constraints
  3. Unifications
  4. Substitution
  5. Obtain final annotated expression
   *)
let infer (env: environment) (e: expr): aexpr = 
  let annotated_expr = annotate_expr e env in 
  let constraints = collect_expr annotate_expr in
  let substitutions = unify constraints in 
    type_variable := (Char.code 'a');
    apply_expr substitutions annotated_expr