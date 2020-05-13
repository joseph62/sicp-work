(define (div-series s1 s2)
  (mul-series s1
              (invert-unit-series s2)))
