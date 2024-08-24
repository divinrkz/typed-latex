(* will "interpret" a subset of mathematical statements *)

(*
   Probably need to include: assumptions, facts, declarations/definitions
   Basic english pattern matcher?
   - Since, Given, Starting with, Since we have
   - We start with, We have, Assume, Suppose
   - AND to combine relations logically
   - Or, we can just take LHS as assumptions.

   Function declarations: f(...vars) = RHS
   - Ensure function declaration only in leftmost in chain of expressions (otherwise, doesn't make sense)
   - function args should all be variables/symbol commands
   - give warnings about free variables in RHS

   Relations:
   - Given a chain of equalities/inequalities/subsets/supersets,
     - ensure consistency (e.g. chain only contains =, < and <=; or only =, \\subset, \\subseteq; etc)
     - with a < b < c, is true iff a < b and b < c, etc
   - in general, a relation can be assigned a truth value, and is either assumed or trying to be proved

   a = b = c is shorthand for a = b and b = c => a = c

   Logic:
   - implications


   for a direct proof:
       start => ... => final

   for contrapositive:
       !final => ... => !start

   for contradiction:
       assume !final => ... => contradiction

   for induction:
       base case AND induction hypothesis AND induction step => done
*)

(*
Type checking:
- can be done independent of above?
- check that math equations are well formatted, and types can be unified

*)

(*
First iteration:
- Completely ignore textual context
- looking at each mathmode statement, take LHS to be an assupmtion (unless it is a function declaration)
- take last RHS to be the thing to be proved
- give each variable a type - either numeric, function, or set of _
*)

(*
Rules for interpreting:
  - lone relations (such as x < 5) are taken to be assumptions

*)
