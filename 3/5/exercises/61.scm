(define (invert-unit-series stream)
  (cons-stream 
    1
    (scale-stream
      (mul-streams (stream-cdr stream)
                   (invert-unit-series stream))
      -1)))

