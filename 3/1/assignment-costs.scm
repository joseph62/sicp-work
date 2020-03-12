(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))

(W 20)
; 5

(W 10)
; -5

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

(D 20)
; 5

(D 10)
; 15

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin (set! product (* counter product))
               (set! counter (+ counter 1))
               ;BAD ORDER:
               ;(set! product (* counter product))
               ;(set! counter (+ counter 1))      
               iter)))
  (iter)))

