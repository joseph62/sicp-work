(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'serializer) protected)
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; Maintaining a serializer on the deposit and withdraw mechanisms
; as well as exposing the serializer for compound operations will
; cause an issue when any compound issue tries to use deposit or withdraw.
; For example calling the exchange-searialized procedure would add exchange
; to the serializer for the two accounts and then exchange would try to call
; withdraw and deposit during the course of execution. These calls would hang 
; indefinitely however because the serializer already has the exchange procedure
; running so it cannot allow withdraw and deposit to proceed, but exchange
; cannot proceed without withdraw and deposit.
