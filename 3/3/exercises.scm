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

; 17)
(define (contains? x xs)
  (cond ((null? xs) #f)
		((eq? x (car xs)) #t)
		(else (contains? x (cdr xs)))))

(define (adjoin x xs)
  (cons x xs))

(define (count-pairs x)
  (define processed ())
  (define (count-pairs-iter x)
	(cond ((not (pair? x)) 0)
		  ((contains? x processed) 0)
		  (else 
			(set! processed (adjoin x processed))
			(+ (count-pairs-iter (car x))
			   (count-pairs-iter (cdr x))
			   1))))
  (count-pairs-iter x))

(count-pairs (list 1 2 3))
; 3

(count-pairs (make-cycle (list 1 2 3)))
; 3

(count-pairs c)
; 3

; 18)
(define (has-cycle? items)
  (define (has-cycle-iter? items processed)
	(cond ((null? items) #f)
		  ((contains? items processed) #t)
		  (else (has-cycle-iter? (cdr items)
								 (adjoin items processed)))))
  (has-cycle-iter? items ()))

; 19)
(define (has-cycle? items)
  (define (has-cycle-iter? items first)
	(cond ((null? items) #f)
		  ((eq? items first) #t)
		  (else 
			(has-cycle-iter? (cdr items) first))))
  (and (not (null? items))
	   (has-cycle-iter? (cdr items) items)))

; 20)
(define x (cons 1 2))

; Global
; x: x.dispatch

; E1 -> Global (cons 1 2)
; x: 1
; y: 2
; set-x!
; set-y!
; dispatch

(define z (cons x x))

; Global
; x: x.dispatch
; z: z.dispatch

; E2 -> Global (cons x x)
; x: (Global x)
; y: (Global x)
; set-x!
; set-y!
; dispatch

(set-car! (cdr z) 17)

; E3 -> Global (cdr z)

; E4 -> E2 (z.dispatch 'cdr)

; E5 -> Global (set-car! x.dispatch 17)
; E6 -> E1 (x.dispatch 'set-car!)
; E7 -> E1 (set-x! 17)

; E1 -> Global
; x: 17
; y: 2

(car x)
; E8 -> Global (car x.dispatch)
; E9 -> E1 (x.dispatch 'car)

; 17

; 21)
(define (print-queue q)
  (display (front-ptr q)))

; 22)
(define (make-queue)
  (let ((front ())
		(rear ()))
	(define (set-front-ptr! v)
	  (set! front v)
	  dispatch)
	(define (set-rear-ptr! v)
	  (set! rear v)
	  dispatch)
	(define (dispatch m)
	  (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
			((eq? m 'set-rear-ptr!) set-rear-ptr!)
			((eq? m 'front-ptr) front)
			((eq? m 'rear-ptr) rear)
			(else (error "DISPATCH code not recognized" m))))
	dispatch))

(define (front-ptr q)
  (q 'front-ptr))

(define (rear-ptr q)
  (q 'rear-ptr))

(define (set-front-ptr! q v)
  ((q 'set-front-ptr!) v))

(define (set-rear-ptr! q v)
  ((q 'set-rear-ptr!) v))

; 23)
(define (make-deque)
  (cons () ()))

(define (front-ptr d)
  (car d))

(define (rear-ptr d)
  (cdr d))

(define (set-front-ptr! d v)
  (set-car! d v)
  d)

(define (set-rear-ptr! d v)
  (set-cdr! d v)
  d)

(define (make-node v)
  (list v () ()))

(define (value n)
  (car n))

(define (prev-ptr n)
  (cadr n))

(define (next-ptr n)
  (caddr n))

(define (has-next? n)
  (not (null? (next-ptr n))))

(define (has-prev? n)
  (not (null? (prev-ptr n))))

(define (set-prev-ptr! n1 n2)
  (set-car! (cdr n1) n2)
  n1)

(define (set-next-ptr! n1 n2)
  (set-car! (cddr n1) n2)
  n1)

(define (link-prev-node! n1 n2)
  (set-prev-ptr! n1 n2)
  (set-next-ptr! n2 n1))

(define (unlink-prev-node! n)
  (cond ((has-prev? n)
		 (set-next-ptr! (prev-ptr n) ())
		 (set-prev-ptr! n ())
		 n)
		(else (error "UNLINK-PREV node has no previous node" n))))

(define (link-next-node! n1 n2)
  (set-next-ptr! n1 n2)
  (set-prev-ptr! n2 n1))

(define (unlink-next-node! n)
  (cond ((has-next? n)
		 (set-prev-ptr! (next-ptr n) ())
		 (set-next-ptr! n ())
		 n)
		(else (error "UNLINK-PREV node has no previous node" n))))

(define (empty-deque? d)
  (and (null? (front-ptr d))
	   (null? (rear-ptr d))))

(define (front-deque d)
  (cond ((empty-deque? d)
		 (error "FRONT deque is empty" d))
		(else (value (front-ptr d)))))

(define (rear-deque d)
  (cond ((empty-deque? d)
		 (error "REAR deque is empty" d))
		(else (value (rear-ptr d)))))

(define (front-insert-deque! d v)
  (let ((node (make-node v)))
	(cond ((empty-deque? d)
		   (set-front-ptr! d node)
		   (set-rear-ptr! d node)
		   d)
		  (else 
			(link-prev-node! (front-ptr d) node)
			(set-front-ptr! d node)
			d))))

(define (rear-insert-deque! d v)
  (let ((node (make-node v)))
	(cond ((empty-deque? d)
		   (set-front-ptr! d node)
		   (set-rear-ptr! d node)
		   d)
		  (else 
			(link-next-node! (rear-ptr d) node)
			(set-rear-ptr! d node)
			d))))

(define (front-delete-deque! d)
  (cond ((empty-deque? d)
		 (error "FRONT-DELETE deque is empty" d))
		((has-next? (front-ptr d))
		 (let ((new-front (next-ptr (front-ptr d))))
		   (unlink-prev-node! new-front)
		   (set-front-ptr! d new-front)
		   d))
		(else
		  (set-front-ptr! d ())
		  (set-rear-ptr! d ())
		  d)))

(define (rear-delete-deque! d)
  (cond ((empty-deque? d)
		 (error "REAR-DELETE deque is empty" d))
		((has-prev? (rear-ptr d))
		 (let ((new-rear (prev-ptr (rear-ptr d))))
		   (unlink-next-node! new-rear)
		   (set-rear-ptr! d new-rear)
		   d))
		(else
		  (set-rear-ptr! d ())
		  (set-next-ptr! d ())
		  d)))
