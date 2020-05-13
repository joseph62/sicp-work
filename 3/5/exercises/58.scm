(define (expand num den radix)
  (cons-stream 
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; A sequence that seems to be cyclical,
; but stays at 0 if it ever produces a 0 remainder

(expand 1 7 10)
; 1, 4, 2, 8, 5, 7

(expand 3 8 10)
; 3, 5, 7, 0, 0, 0
