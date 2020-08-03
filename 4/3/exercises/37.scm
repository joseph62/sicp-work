(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between i high))
         (k (an-integer-between j high)))
    (require (= (+ (* i i) (* j j)) (* k k))) 
    (list i j k)))

(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between i high)) 
         (hsq (* high high))
         (ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

; I believe the second implementation is
; more efficient because it only considers
; the combinations of i and j instead of every
; combination of i, j, and k.
