(define (add a b) (+ a b))

(define (expression-for-operator)
  (lambda (a b)
    ((if (even? a) add -) a b)))

; The way apply acts depends on the type of procedure it uses.
; In this case the procedure could be either primitive or compound.
; If it ends up being primitive, then all of the arguments must be calculated.
