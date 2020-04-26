; 9)
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

; Global E:
; factorial
; fact-iter

(factorial 6)
; E1 -> Global E (factorial)
; n: 6

; E2 -> Global E (fact-iter)
; product: 1
; counter: 1
; max-count: 6

; E3 -> Global E (>)
; left: 1
; right: 6

; E4 -> Global E (*)
; left: 1
; right: 1

; E5 -> Global E (+)
; left: 1
; right: 1

; E6 -> Global E (fact-iter)
; product: 1
; counter: 2
; max-count: 6

; E7 -> Global E (>)
; left: 2
; right: 6

; E8 -> Global E (*)
; left: 2
; right: 1

; E9 -> Global E (+)
; left: 2
; right: 1

; E10 -> Global E (fact-iter)
; product: 2
; counter: 3
; max-count: 6

; E11 -> Global E (>)
; left: 3
; right: 6

; E12 -> Global E (*)
; left: 2
; right: 3

; E13 -> Global E (+)
; left: 3
; right: 1

; E14 -> Global E (fact-iter)
; product: 6
; counter: 4
; max-count: 6

; E15 -> Global E (>)
; left: 4
; right: 6

; E16 -> Global E (*)
; left: 6
; right: 4

; E17 -> Global E (+)
; left: 4
; right: 1

; E18 -> Global E (fact-iter)
; product: 24
; counter: 5
; max-count: 6

; E19 -> Global E (>)
; left: 5
; right: 6

; E20 -> Global E (*)
; left: 24
; right: 5

; E21 -> Global E (+)
; left: 5
; right: 1

; E22 -> Global E (fact-iter)
; product: 120
; counter: 6
; max-count: 6

; E23 -> Global E (>)
; left: 6
; right: 6

; 720

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; Global E
; factorial

(factorial 6)
; E1 -> Global E (factorial)
; n: 6

; E2 -> Global E (=)
; left: 6
; right 1

; E3 -> Global E (-)
; left: 6
; right: 1

; E4 -> Global E (factorial)
; n: 5

; E5 -> Global E (=)
; left: 5
; right 1

; E6 -> Global E (-)
; left: 5
; right: 1

; E7 -> Global E (factorial)
; n: 4

; E8 -> Global E (=)
; left: 4
; right 1

; E9 -> Global E (-)
; left: 4
; right: 1

; E10 -> Global E (factorial)
; n: 3

; E11 -> Global E (=)
; left: 3
; right 1

; E12 -> Global E (-)
; left: 3
; right: 1

; E13 -> Global E (factorial)
; n: 2

; E14 -> Global E (=)
; left: 2
; right 1

; E15 -> Global E (-)
; left: 2
; right: 1

; E16 -> Global E (factorial)
; n: 1

; E14 -> Global E (=)
; left: 1
; right 1

; E15 -> Global E (*)
; left: 2
; right: 1

; E16 -> Global E (*)
; left: 3
; right: 2

; E17 -> Global E (*)
; left: 4
; right: 6

; E18 -> Global E (*)
; left: 5
; right: 24

; E19 -> Global E (*)
; left: 6
; right: 120

; 720

; 10)

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds!"))))
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))

; Global:
; make-withdraw
; W1 -> E2
; W2 -> E5

; E1 -> Global
; initial-amount: 100

; E2 -> E1
; balance: 100

; E3 -> E2
; amount: 50

; E2 -> E1
; balance: 50 

; E4 -> Global
; initial-amount: 100

; E5 -> E4
; balance: 100

; Even though W1 and W2 are created with the same initial balance, they point to
; two entirely separate envrionments.

; 11)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (deposit account amount)
  ((account 'deposit) amount))

(define (withdraw account amount)
  ((account 'withdraw) amount))

(define acc (make-account 50)) 
(deposit acc 40)
; 90
(withdraw acc 60)
; 30

; Global
; make-account
; deposit
; withdraw

; E1 -> Global (make-account 50)
; balance: 50
; withdraw
; deposit
; dispatch

; Global
; acc: dispatch -> E1

; E2 -> Global (deposit acc 40)

; E3 -> E1 (acc 'deposit)

; E4 -> E1 (deposit 40)

; E1
; balance: 90

; E5 -> Global (withdraw acc 60)

; E6 -> E1 (acc 'withdraw)

; E7 -> E1 (withdraw 60)

; E1
; balance: 30

; If another account was created using make-account, the resulting account
; would use an entirely new envrionment to store it's balance, withdraw, deposit, and dispatch
