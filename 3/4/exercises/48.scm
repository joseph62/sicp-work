; Acquiring shared resources in the same order will assure that
; two procedures trying to acquire multiple resources cannot 
; both acquire partial resources that the other needs. 
; If for example two exchange procedures always try to acquire the
; lowest account first, one procedure will get the account and the 
; other will need to wait for it to be available every time.

(define (make-counter current)
  (lambda ()
    (let ((last current))
      (set! current (+ current 1))
      last)))

(define account-counter (make-counter 1))

(define (make-account-and-serializer balance)
  (let ((account-number (account-counter)))
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
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'serializer) protected)
              ((eq? m 'balance) balance)
              ((eq? m 'account-number) account-number)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

(define (account-number account)
  (account 'account-number))

(define (serialized-exchange a1 a2)
  (let ((s1 (serializer a1))
        (s2 (serializer a2)))
    (if (< (account-number a1)
           (account-number a2))
        ((s2 (s1 exchange)) a1 a2)
        ((s1 (s2 exchange)) a1 a2))))




