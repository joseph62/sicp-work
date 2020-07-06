(define (f x)
  (define (even? n)
    (if (= n 0)
      true
      (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
      false
      (even? (- n 1))))
  (even? x))

(define (g x)
  (define u (+ x 1))
  (define v (+ u 2))
  (+ u v))

(define (g-transformed x)
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u (+ x 1))
    (set! v (+ u 2))
    (+ u v)))
