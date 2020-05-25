(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

; This version will run continue calling pairs with the next iteration of
; s and t. This would be an issue for infinite streams like integers.
