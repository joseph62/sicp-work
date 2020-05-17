(define (average . numbers)
  (/ (apply + numbers)
     (length numbers)))

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

; This implementation is less efficient that the version
; that binds the stream to guesses instead of recursing the procedure.
; Recursing creates a new stream without the memoization.
; If the memoization optimization was removed the two implementations would be equivalent.
