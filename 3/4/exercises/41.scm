(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch operation)
      (cond ((eq? operation 'withdraw) (protected withdraw))
            ((eq? operation 'deposit) (protected deposit))
            ((eq? operation 'balance) 
             (protected (lambda () balance)))
            (else
              (error "Operation unknown -- ACCOUNT" operation))))
    dispatch))

; Adding serialization would not further reduce anomalous behavior
; because operations that modify the bank account are already serailized.

; Take for instance  a compound operation that first reads the bank account
; and then makes a withdraw operation based on the value. 
; This withdraw may produce an "Insufficient funds" result if
; the balance value had changed in between the balance and withdraw.
; Serializing the balance procedure would not help in this case because
; a different withdraw procedure could still occure in between the balance and
; subsequent withdraw.
