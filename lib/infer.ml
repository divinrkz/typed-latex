
(* Initial static environment for function and operators *)

(* // type constraints. 
t1 = t2

env |- e : t -| c

c is set of equations on types *)

(* 
environment is set of bindings, 
and C is set of type equations.   *)

(* // Constants.
Rule: env |- i: int -| {}
Rule: env |- b: bool -| {} *)
(* 
So, therefore 
{} |- -3110: int -| {}
{} |- false: bool -| {} *)

(* env |- n : env(n) -| {} *)
(* If not bindings, fail if not value env(n) *)
(* {x: int} |- x: int -| {}
{} |/- x *)
(* 
Initial static environment
( + ): int -> int -> int
( * ): int -> int -> int
( <= ): int -> int -> bool  *)

(* env |- if e1 then e2 else e3 : 't -| C1, C2, C3, C

  if fresh t
  and
  env |- e1: t1 -| C1
  and 
  env |- e2: t2 -| C2
  and
  env |- e3: t3 -| C3 
  and C = {t1 = bool, t' = t2, t' = t3}  *)

  (* Example 
  {} |- if false then 0 else 1: 'a -| C
    {} |- false: bool -| {}
    {} |- 0: int -| {}
    {} |- 1: int -| {}
    C = {bool = bool, 'a = int, 'a = int} *)

(* Function:
RULES
  env |- fun x -> e: 't1 -> t2 -| C
    if fresh 't1
      and env, x: 't1 |- e: t2 -| C

Example;
{} |- fun x -> if x then 1 else 0: 'a -> 'b -| C
  {x: 'a} |- if x then 1 else 0: 'b -| C
    {x: 'a} |- x: 'a -| {}
    {x: 'a} |- 1: int -| {}
    {x: 'a} |- 0: int -| {}
    C = {'a = bool, 'b = int, b' = int } *)
(* 

Function Application
RULES 
  env |- e1 e1: 't -| C1, C2, C
   if fresh 't 
    and env |- e1: t1 -| C1
   and env |- e2: t2 -| C2
   and C = {t1 = t2 -> 't}  *)


  (* 1. Unification of type constraints *)
  (* Unification algortishm  *)
  (* if reductions fails then types are inconsistent.
     Unifications is also so optimal that it guarantess the most general unifier. for particular set of equations.

   *)

   (* https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html *)

  