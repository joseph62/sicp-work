(define (sum-of-squares numbers)
  (sum (map square numbers)))

(define seananujan-numbers
  (stream-map sum-of-squares
              (consecutive-filter (lambda (p0 p1 p2)
                                    (= (sum-of-squares p0)
                                       (sum-of-squares p1)
                                       (sum-of-squares p2)))
                                  3
                                  (weighted-pairs sum-of-squares
                                                  integers
                                                  integers))))

