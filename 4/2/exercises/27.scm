(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-eval input:
; count

;;; L-eval value:
; 0

;;; L-eval input:
; w

;;; L-eval value:
; 10

;;; L-eval input:
; count

;;; L-eval value:
; 2

