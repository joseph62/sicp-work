(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

; P1: car cell: #f
; P2: car cell: #f
; P2: set-car! cell: #t
; P1: set-car! cell: #t
; P1: #f
; P2: #f

; Both P1 and P2 have acquired the mutex because test-and-set is not atomic.
