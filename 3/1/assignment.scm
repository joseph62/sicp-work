(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define random-init 0)
(define (random-update x)
  (remainder (+ (* 44535 x) 5342) 635254))
