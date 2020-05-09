(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let* ((protected (make-serializer))
         (protected-withdraw (protected withdraw))
         (protected-deposit (protected deposit)))

    (define (dispatch operation)
      (cond ((eq? operation 'withdraw) protected-withdraw)
            ((eq? operation 'deposit) protected-deposit)
            ((eq? operation 'balance) balance)
            (else
              (error "Operation unknown -- ACCOUNT" operation))))
    dispatch))

; The modification to the protected withdraw and deposit may
; negatively impact the concurrency protections if the same protected
; operation could be invoked multiple times. If that is not the case 
; then there should not be any difference in the concurrency protections.
