(define (next-n n stream)
  (if (= n 0)
      ()
      (cons (stream-car stream)
            (next-n (- n 1)
                    (stream-cdr stream)))))
      
(define (sum-of-cubes numbers)
  (sum (map (lambda (n) (expt n 3)) numbers)))

(define (consecutive-filter p? n stream)
    (if (apply p? (next-n n stream))
        (cons-stream (stream-car stream)
                     (consecutive-filter p? n (stream-cdr stream)))
        (consecutive-filter p? n (stream-cdr stream))))

(define ramanujan-numbers 
  (stream-map sum-of-cubes
              (consecutive-filter (lambda (p0 p1)
                                    (= (sum-of-cubes p0)
                                       (sum-of-cubes p1)))
                                  2
                                  (weighted-pairs sum-of-cubes
                                                  integers
                                                  integers))))

; 1,729 4104 20683 32832 64232 65728
