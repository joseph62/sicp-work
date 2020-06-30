; The map procedure does not work asa primitive because it 
; does not know how to evaluate a procedure object in the
; metacircular evaluator. Defining the map procedure in the
; repl means that map uses the meta evaluators eval and apply.
