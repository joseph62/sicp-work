(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

; (factorial 5) will work in normal order evaluation
; because the recursive case is only ever evaluated when the
; condition is not met. In applicative order evaluation
; the recursion hits maximum depth because the recursive
; case is always evaluated
