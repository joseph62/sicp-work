; 1)

10 
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(define a 3)
; a

(define b (+ a 1))
; b

(+ a b (* a b))
; 19

(= a b)
; #f

(if (and (> b a) (< b (* a b)))
  b
  a)
; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16

(+ 2 (if (> b a) b a))
; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16

; 2)
(/ (+ 5
      4
      (- 2 
         (- 3 
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; 3)
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-of-squares-largest x y z)
  (cond ((= x (min x y z)) (sum-of-squares y z))
        ((= y (min x y z)) (sum-of-squares x z))
        ((= z (min x y z)) (sum-of-squares x y)))

; 4)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; Add b to a if b is positive, else subtract b from a

; 5)
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))
(test 0 (p))
; Applicative Order: (p) is evaluated first and recurses without stopping
; Normal Order: The arguments to test are applied to the underlying procedure and (p) is never evaluated
