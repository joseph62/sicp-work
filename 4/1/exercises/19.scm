(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

; I would prefer the true simultaneous evaluation method of Eva.
; The incorrect answer method would be unacceptable and the 
; erroring solution seems somewhat confusing to a lisp user.

; One potential way to implement a simultaneous definition of values
; would be to scan out the define statements, identify dependencies
; that are not within nested procedure expressions and attempt to
; reorder the identified expressions in a way that allows the definitions
; to proceed without error.
