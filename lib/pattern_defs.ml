open Patterns

let relation_types =
  [
    ( "Equality",
      "Unequality",
      "GreaterThan",
      "LessThan",
      "StrictGreaterThan",
      "StrictLessThan" );
  ]

(* let def1 =
   Sequence
     [
       Any [ Word "choose"; Word "let"; Word "consider"; Word "define" ];
       DefContainer (any_known_relation_pattern 1, 0);
       Optional
         (Repeat
            (Sequence
               [
                 Any [ Word "and"; Word "," ];
                 DefContainer (any_known_relation_pattern 1, 0);
               ]));
     ] *)

let def =
  Sequence
    [
      Optional (Word "also");
      Optional (Word ",");
      Optional (Sequence [ Word "we"; Word "could" ]);
      Any [ Word "suppose"; Word "say"; Word "imagine"; Word "let" ];
      DefContainer
        ( MathPattern (Function ("StrictGreaterThan", [ Expression 2; Expression 3 ], 1)),
          0 );
    ]
