(define (square x) (* x x))

(square 3) ; 9

(square (+ 2 3)) ; 25

(square (square 3)) ; 81

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (f a) (sum-of-squares (+ a 1) (* a 2)))

; Applicative vs Normal order

; Applicative order:
; (f 5)
; (sum-of-squares (+ 5 1) (* 5 2))
; (sum-of-squares 6 10)
; (+ (square 6) (square 10))
; (+ (* 6 6) (* 10 10))
; (+ 36 100)
; 136

; Normal order:
; (f 5)
; (sum-of-squares (+ 5 1) (* 5 2))
; (+ (square (+ 5 1)) (square (* 5 2)))
; (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
; (+ (* 6 6) (* 10 10))
; (+ 36 100)
; 136
