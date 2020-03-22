; 12)
(define (append x y)
  (if (null? x)
	  y
	  (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (cond ((null? x) (error "Operand cannot be null -- LAST-PAIR" x))
		((null? (cdr x)) x)
		(else (last-pair (cdr x)))))

(define x '(a b))
(define y '(c d))

(define z (append x y))

z
; (a b c d)

(cdr x)
; (b)

(define w (append! x y))

w
; (a b c d)

(cdr x)
; (b c d)

; z is a pointer to a new list of elements in x and y
; w is a pointer to x which has been modified to include the elements of y

; 13)
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define cycle (make-cycle '(a b c)))
; (last-pair cycle) -> Will go on forever because the very last reference in the list is 
; 						a reference to the head of the list and not null

; 14)
(define (mystery x)
  (define (loop x y)
	(if (null? x)
		y
		(let ((temp (cdr x)))
		  (set-cdr! x y)
		  (loop temp x))))
  (loop x '()))

; This mystery procedure reverses the given list using mutation.
; The loop procedure takes an x and a y.
; Returns the value of y if x is null.
; The saves the tail of the x list as temp.
; Mutates x so that the tail is the list of y.
; Finally, it calls loop with x being the old tail of the current x and y being
; the head of the current x with the tail of y

; 15)

; z1 is composed of a pair where the car is a pointer to the list bound to 
; the name x and the cdr is also a pointer to the list bound by x

; z2 is composed of a pair where the car is a list whose values are a and b
; and the cdr is also a list whose values are a and b. These two lists are
; not the same pair.

; 16)

(define (count-pairs x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs (car x))
		 (count-pairs (cdr x))
		 1)))

(count-pairs (list 1 2 3))
; 3

(define a (cons 1 1))
(define b (cons a a))
(define c (cons b b))
(count-pairs c)
; 7

(count-pairs (cons 1 b))
; 4

; (count-pairs (make-cycle '(1 2 3)))
; does not return

