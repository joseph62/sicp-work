(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-eval input:
; count

;;; L-eval value:
; 0
; count initialized to 0 and the id operation has not yet been applied

;;; L-eval input:
; w

;;; L-eval value:
; 10
; both id procedures are evaluated at this point to return a value of 10

;;; L-eval input:
; count

;;; L-eval value:
; 2
; count has been updated by both calls to the id procedure.
