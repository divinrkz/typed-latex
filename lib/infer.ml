type id = string

type op = Add | Mul | Gt | Lt | And | Or

module CharMap = Map.Make(String)

type genericMap = int CharMap.t

type primitiveType =
  | TNum
  | TBool
  | T of string
  | TFun of primitiveType * primitiveType
;;

type expr =
  | NumLit of int
  | BoolLit of bool
  | Val of string
  | Binop of expr * op * expr
  | Fun of id * expr
  | App of expr * expr
;;

type aexpr =
  | ANumLit of int * primitiveType
  | ABoolLit of bool * primitiveType
  | AVal of string * primitiveType
  | ABinop of aexpr * op * aexpr * primitiveType
  | AFun of id * aexpr * primitiveType
  | AApp of aexpr * aexpr * primitiveType
;;

let string_of_op (op: op) =
  match op with
  | Add -> "+" | Mul -> "*" | Lt -> "<" | Gt -> ">"
  | Or -> "||" | And -> "&&"
;;

let string_of_type (t: primitiveType) =
  let rec aux (t: primitiveType) (chr: int) (map: genericMap) =
    match t with
    | TNum -> "int", chr, map
    | TBool -> "bool", chr, map
    | T(x) ->
      let gen_chr, new_chr, new_map = if CharMap.mem x map
        then Char.escaped (Char.chr (CharMap.find x map)), chr, map
        else
          let c = Char.escaped (Char.chr chr) in
          c, (chr + 1), CharMap.add x chr map
      in
      Printf.sprintf "'%s" gen_chr, new_chr, new_map
    | TFun(t1, t2) -> let (st1, c1, m1) = aux t1 chr map in
      let (st2, c2, m2) = aux t2 c1 m1 in
      (Printf.sprintf "(%s -> %s)" st1 st2), c2, m2 in
  let s, _, _ = aux t 97 CharMap.empty in s
;;

let rec string_of_aexpr (ae: aexpr): string =
  match ae with
  | ANumLit(x, t)  -> Printf.sprintf "(%s: %s)" (string_of_int x) (string_of_type t)
  | ABoolLit(b, t) -> Printf.sprintf "(%s: %s)" (string_of_bool b) (string_of_type t)
  | AVal(x, t) -> Printf.sprintf "(%s: %s)" x (string_of_type t)
  | ABinop(e1, op, e2, t) ->
    let s1 = string_of_aexpr e1 in let s2 = string_of_aexpr e2 in
    let sop = string_of_op op in let st = string_of_type t in
    Printf.sprintf "(%s %s %s: %s)" s1 sop s2 st
  | AFun(id, ae, t) ->
    let s1 = string_of_aexpr ae in
    let st = string_of_type t in
    Printf.sprintf "(fun %s -> %s): %s" id s1 st
  | AApp(e1, e2, t) ->
    let s1 = string_of_aexpr e1 and
    s2 = string_of_aexpr e2 and st = string_of_type t in
    Printf.sprintf "(%s %s): %s" s1 s2 st
;;

let rec string_of_expr (e: expr): string =
  match e with
  | NumLit(x) -> string_of_int x
  | BoolLit(b) -> string_of_bool b
  | Val(s) -> s
  | Binop(e1, op, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    let sop = string_of_op op in
    Printf.sprintf "(%s %s %s)" s1 sop s2
  | Fun(id, e) ->
    let s1 = string_of_expr e in Printf.sprintf "(fun %s -> %s)" id s1
  | App(e1, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    Printf.sprintf "(%s %s)" s1 s2
;;

(*
   Environment is a map that holds type information of variables.
   TODO: Implementing global environment and local environment *** 
   *)
module NameMap = Map.make(String)
type environment = primitiveType NameMap.t

(* Unknown type,  resolved type. eg.[(T, TNum); (U, TBool)] *)
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



(*
A constraint is a tuple two primitivetypes, and a strict equality is being
imposed on the two types,
   returns a list of constraints 
   *)
let rec collect_expr (ae: aexpr): (primitiveType * primitiveType) list = 
  match ae with 
  | ANumLit(_) | ABoolLit(_) -> [] 
  (* No constraints for literals *)
  | AVal(_) -> []
  | ABinop(e1, op, e2, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 in 

      (* impose constraints based on binary operator *)
    let opc = match op with 
      | Add | Mul -> [(et1, TNum); (et2, TNum); (t, TNum)] 
         (* we return et1, et2 since these are generic operators *)
      | Gt | Lt -> [(et1, et2); (t, TBool)]
      | And | Or -> [(et1, TBool); (et2, TBool); (t, TBool)]
    
  in 
  (collect_expr ae1) @ (collect_expr ae2) @ opc
  | AFun(id, ae, t) -> (match t with 
    | TFun(idt, ret_type) -> (collect_expr ae) @ [(type_of ae, ret_type)] )  
    | _ -> raise (failwith "not a function")

  | AApp(fn, arg, t) -> (match (type_of fn with 
    | TFun (argt, ret_type) -> (collect_expr fn) @ (collect_expr arg) @ [(t, rel_type): (argt, type_of arg)] )
    | T(_) -> (collect_expr fn) @ (collect_expr arg) @ [(type_of fn, TFun(type_of arg, t))]
    | _ -> raise (failwith "invalid application")
    )



 let rec substitute (u: primitiveType) (x: id) (t: primitiveType): primitiveType = 
  match t with 
  | TNum | TBool -> t 
  | T(c) -> if c = x then u else t  
  | TFun(t1, t2) -> TFun(substitute u x t1, substitute u x t2)
 ;;

 
 let apply (subs: substitutions) (t: primitiveType): primitiveType  = 
 List.fold_right  (fun (x, u) t -> substitute u x t ) subs t


 let rec unify (constraints: (primitiveType * primitiveType) list) : substitutions = 
  match constraints with 
  | [] -> []
  | (x, y) :: xs -> 
    let t2 = unify xs in 
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2

  and unify_one (t1: primitiveType) (t2: primitiveType): substitutions = 
    match t1, t2 with 
    | TNum, TNum | TBool, TBool -> []
    | T(x), z | z, T(x) -> [(x, z)]
    | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)]
    | _ raise (failwith "mismatched types")


let apply_expr (subs: substitutions) (ae: aexpr): aexpr = 
  match ae with 
  | ABoolLit(b, t) -> ABoolLit(b, apply subs t)
  | ANumLit (n, t) -> ANumLit (n, apply subs t)
  | AVal(s, t) -> AVal(s, apply subs t)
  | ABinop(e1, op, e2, t) -> ABinop(apply_expr subs e1, op, apply_expr subs e2, apply subs t)
  | AFun(id, e, t) -> AFun(id, apply_expr subs e, apply subs t)
  | AApp(fn, arg, t) -> AApp(apply_expr subs fn, apply_expr subs arg, apply subs t)
;;


(* Runs Hindler Milner Type System:
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





    let rec get_ids (e: expr): id list =
      let rec dedup = function
       | [] -> []
       | x :: y :: xs when x = y -> y :: dedup xs
       | x :: xs -> x :: dedup xs in
      let ids = match e with
       | NumLit(_) | BoolLit(_) -> []
       | Val(x) -> [x]
       | Fun(x, y) -> [x] @ (get_ids y)
       | Binop(e1, _, e2) -> (get_ids e1) @ (get_ids e2)
       | App(fn, arg) -> (get_ids fn) @ (get_ids arg) in
     dedup ids
    ;;
    
    let infer_types (e: Ast.expr): Ast.aexpr =
      let vals = get_ids e in
      let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in
      Infer.infer env e
    ;;
       