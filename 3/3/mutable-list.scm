(define (make-person name age)
  (list name age))

(define (name person)
  (car person))

(define (age person)
  (cadr person))

(define (set-age! person age)
  (set-car! (cdr person) age)
  person)

(define (birthday person)
  (set-age! person (+ (age person) 1))
  person)

(define (get-new-pair) ; create an empty pair
  (cons () ()))

(define (conss x y)
  (let ((new (get-new-pair)))
	(set-car! new x)
	(set-cdr! new y)
	new))

(define x '(a b))

(define z1 (cons x x))

(define z2 (cons '(a b) '(a b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)

z1
; ((wow b) wow b)

(set-to-wow! z2)

z2
; ((wow b) a b)

; The predicate eq? tests wether the two operands are
; the same pointer.

(eq? (car z1) (cdr z1))
; #t

(eq? (car z2) (cdr z2))
; #f
