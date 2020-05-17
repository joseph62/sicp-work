(define (abs-difference a b)
  (abs (- (abs a) (abs b))))

(define (stream-limit stream tolerance)
  (let ((s0 (stream-car stream))
        (s1 (stream-car (stream-cdr stream))))
    (if (< (abs-difference s0 s1) tolerance)
        s1
        (stream-limit (stream-cdr stream) tolerance))))




    
