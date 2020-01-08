; 17)

(define (last-pair items)
  (cond ((null? items) ()) 
         ((null? (cdr items)) (car items))
         (else (last-pair (cdr items)))))

; 18)

(define (reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
  (iter items ()))

; 19)

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (no-more? denominations) 
  (null? denominations))

(define (first-denomination denominations)
  (car denominations))

(define (except-first-denomination denominations)
  (cdr denominations))

(define (cc amount denominations)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? denominations)) 0)
        (else (let ((denomination (first-denominations denominations))
                    (other-denominations (except-first-denomination denominations)))
                (+ (cc amount 
                       (except-first-denomination denominations))
                   (cc (- amount 
                          (first-denomination denominations))
                       denominations))))))

; 20)

(define (same-parity first . rest)
  (let ((parity (if (even? first) even? odd?)))
    (define (iter numbers)
      (cond ((null? numbers) ())
            ((parity (car numbers)) (cons (car numbers) 
                                          (iter (cdr numbers))))
            (else (iter (cdr numbers)))))
    (cons first (iter rest))))
