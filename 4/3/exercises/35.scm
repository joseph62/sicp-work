(define (an-integer-between low high)
  (let ((n (an-integer-starting-from low)))
    (require (< n high))
    (amb n (an-integer-between low high))))
