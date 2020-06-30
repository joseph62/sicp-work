(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
    (run-forever)
    'halted))

; Evaluating (try try) would violate the definition of halts?
; because if it evaluated try as halting then it would not halt,
; but if it evaluted try as not halting then it would halt.
