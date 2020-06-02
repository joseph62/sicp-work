(define (average . numbers)
  (/ (sum numbers) (length numbers)))

(define (smooth stream)
  (stream-map average stream (stream-cdr stream)))

(define smoothed-sense-data (smooth sense-data))

(define zero-crossings
  (stream-map sign-change-detector 
              smoothed-sense-data
              (stream-cdr smoothed-sense-data)))
