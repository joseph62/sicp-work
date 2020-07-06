; a)

(define fact-10
  ((lambda (n)
     ((lambda (fact)
        (fact fact n))
     (lambda (ft k)
       (if (= k 1)
         1
         (* k (ft ft (- k 1)))))))
   10))

(define fib-5
  ((lambda (n)
     ((lambda (fib)
        (fib fib n))
      (lambda (fib n)
        (cond ((= n 0) 1)
              ((= n 1) 1)
              (else (+ (fib fib (- n 1))
                       (fib fib (- n 2))))))))
     5))

; b)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
