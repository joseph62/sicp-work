(define (random-in-range random-numbers low high)
  (let ((range (- high low)))
    (stream-map (lambda (n)
                  (+ low (remainder n range)))
                random-numbers)))

(define (rectangle-area x1 y1 x2 y2)
  (let ((l (abs (- x2 x1)))
        (w (abs (- y2 y1))))
    (* l w)))

(define (estimate-integral p x1 y1 x2 y2)
  (scale-stream
    (monte-carlo
      (stream-map p
                  (random-in-range random-numbers
                                   x1
                                   x2)
                  (random-in-range random-numbers
                                   y1
                                   y2))
      0
      0)
    (rectangle-area x1 y1 x2 y2)))
