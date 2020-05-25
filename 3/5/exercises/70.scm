(define (weighted-merge weight s t)
  (cond ((stream-null? s) t)
        ((stream-null? t) s)
        ((< (weight (stream-car s)) (weight (stream-car t)))
         (cons-stream (stream-car s)
                      (weighted-merge weight (stream-cdr s) t)))
        (else
          (cons-stream (stream-car t)
                       (weighted-merge weight s (stream-cdr t))))))

(define (weighted-pairs weight s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (weighted-merge weight
                    (stream-map (lambda (x) (list (stream-car s) x))
                                (stream-cdr t))
                    (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))


; a)
(define weighted-int-pairs (weighted-pairs (lambda (p)
                                             (+ (car p)
                                                (cadr p))) 
                                           integers 
                                           integers))

; b)
(define (any bools)
  (cond ((null? bools) #f)
        ((car bools) #t)
        (else (any (cdr bools)))))

(define (not-divisible-by-any numbers)
  (lambda (i)
    (not (any (map (lambda (n)
                     (= (remainder i n) 0))
                   numbers)))))

(define weighted-not-divisible-by-2-3-5 
  (weighted-pairs
    (lambda (p)
      (+ (* 2 (car p))
         (* 3 (cadr p))
         (* 5 (car p) (cadr p))))
    (stream-filter (not-divisible-by-any (list 2 3 5)) 
                   integers)
    (stream-filter (not-divisible-by-any (list 2 3 5)) 
                   integers)))
