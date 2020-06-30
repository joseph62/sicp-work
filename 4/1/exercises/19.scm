(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

; I would prefer the true simultaneous evaluation method of Eva.
; The incorrect answer method would be unacceptable and the 
; erroring solution seems somewhat confusing to a lisp user.

; One way to arrive at the solution of allowing what looks
; like the correct behavior would be to check for a definition
; of a variable in outer scopes. If the variable is already defined
; then define it again with the same value otherwise 
; define it as '*unassigned*
