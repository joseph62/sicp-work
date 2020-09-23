(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
    (cons-stream assertion THE-ASSERTIONS))
  'ok)

; The issue with the above implementation of the add-assertion! procedure
; has to do with the way cons-stream functions. The second operand's evaluation
; in cons-stream is delayed. So in this case a call to add-assertion! would set 
; THE-ASSERTIONS to be an infinite stream of the provided assertion.
