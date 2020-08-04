(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (dwellers-not-adjacent a b)
  (lambda (dwelling)
    (not (= (abs (- (a dwelling) (b dwelling))) 1))))

(define (dweller-not-on-floor dweller f)
  (lambda (dwelling)
    (not (= (dweller dwelling) f))))

(define (interleave x y)
  (cond ((stream-null? x) y)
        ((stream-null? y) x)
        (else 
          (cons-stream (stream-car x)
                       (interleave y (stream-cdr x))))))
(define (combinations-of combiner xs ys)
  (if (stream-null? xs)
      ()
      (interleave
        (stream-map (lambda (y)
                      (combiner (stream-car xs) y))
                    ys)
        (combinations-of combiner (stream-cdr xs) ys))))

(define (baker dwelling) (car dwelling))
(define (cooper dwelling) (cadr dwelling))
(define (fletcher dwelling) (caddr dwelling))
(define (miller dwelling) (cadddr dwelling))
(define (smith dwelling) (car (cddddr dwelling)))

(define (multiple-dwelling)
  (let ((all-combinations-of-floors 
          (combinations-of 
            cons
            (stream 1 2 3 4 5)
            (combinations-of 
              cons
              (stream 1 2 3 4 5)
              (combinations-of 
                cons
                (stream 1 2 3 4 5)
                (combinations-of 
                  list
                  (stream 1 2 3 4 5)
                  (stream 1 2 3 4 5)))))))
    (stream-filter 
      distinct?
      (stream-filter
        (dwellers-not-adjacent fletcher smith)
        (stream-filter
          (dwellers-not-adjacent fletcher cooper)
          (stream-filter
            (lambda (dwelling)
              (> (miller dwelling) (cooper dwelling)))
            (stream-filter
              (dweller-not-on-floor fletcher 1)
              (stream-filter
                (dweller-not-on-floor fletcher 5)
                (stream-filter
                  (dweller-not-on-floor cooper 1)
                  (stream-filter
                    (dweller-not-on-floor baker 5)
                    all-combinations-of-floors))))))))))
