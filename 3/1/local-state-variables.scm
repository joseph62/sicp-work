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
