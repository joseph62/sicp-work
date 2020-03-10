(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

(withdraw 25)
; 75
(withdraw 25)
; 50
(withdraw 60)
; Insufficient funds
(withdraw 15)
; 35

(define new-withdraw
  (let ((balance 100))
    (lambda  (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda  (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
; 50

(W2 70)
; 30

(W2 40)
; Insufficient funds

(W1 40)
; 10

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? 'withdraw) withdraw)
          ((eq? 'deposit) deposit)
          ((else (error "Unkown request -- MAKE-ACCOUNT"
                        m)))))
  dispatch)

(define account (make-account 100))
((acc 'withdraw) 50)
; 50
((acc 'withdraw) 60)
; Insufficient funds
((acc 'deposit) 20)
; 70
((acc 'withdraw) 60)
; 10

