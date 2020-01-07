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
