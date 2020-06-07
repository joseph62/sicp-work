(define (stream-zip . streams)
  (apply stream-map (cons (lambda elements elements) streams)))

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams 
                  (scale-stream iL (* -1 (/ R L)))
                  (scale-stream vC (/ 1 L))))
    (stream-zip vC iL)))

