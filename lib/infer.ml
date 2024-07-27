
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