; a)
(define (integrate-series coefficients)
  (stream-map (lambda (n a)
                (/ a (+ n 1)))
              (integers-starting-from 0)
              coefficients))

; b)

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
