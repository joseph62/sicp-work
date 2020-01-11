; 17)

(define (last-pair items)
  (cond ((null? items) ()) 
         ((null? (cdr items)) (car items))
         (else (last-pair (cdr items)))))

; 18)

(define (reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
  (iter items ()))

; 19)

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (no-more? denominations) 
  (null? denominations))

(define (first-denomination denominations)
  (car denominations))

(define (except-first-denomination denominations)
  (cdr denominations))

(define (cc amount denominations)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? denominations)) 0)
        (else (let ((denomination (first-denominations denominations))
                    (other-denominations (except-first-denomination denominations)))
                (+ (cc amount 
                       (except-first-denomination denominations))
                   (cc (- amount 
                          (first-denomination denominations))
                       denominations))))))

; 20)

(define (same-parity first . rest)
  (let ((parity (if (even? first) even? odd?)))
    (define (iter numbers)
      (cond ((null? numbers) ())
            ((parity (car numbers)) (cons (car numbers) 
                                          (iter (cdr numbers))))
            (else (iter (cdr numbers)))))
    (cons first (iter rest))))

; 21)

(define (square-list numbers)
  (if (null? numbers) 
    () 
    (cons (square (car numbers)) 
          (square-list (cdr numbers)))))

(define (square-list numbers)
  (map square numbers))

; 22)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) 
            (cons (square (car things))
                  answer))))
  (iter items ()))

; (square-list (list 1 2 3))
; (iter (1 2 3) ())
; (iter (2 3) (cons (square 1) ()))
; (iter (3) (cons 2 (1)))
; (iter () (cons 3 (2 1)))
; (3 2 1)
; As this procedure processes items it adds them to the start of the list.
; This results in a reversed list in the end.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items ()))

; (square-list (list 1 2 3))
; (iter (2 3) (cons () 1))
; (iter (3) (cons (() . 1) 2))
; (iter () (cons ((() . 1) . 2) 3))
; (((() . 1) . 2) . 3)
; This solution creates a pair of the previous iteration and the next iteration.

; Correct solution would be:
; (1 . ())
; (1 . (2 . ()))
; (1 . (2 . (3 . ())))
; Must be able to add a pair on to the end of the list

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer (list (car things))))))
  (iter items ()))

; OR

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (reverse (iter items ())))

; 23)

(define (for-each f items)
  (if (null? items)
    #t
    (or (and (f (car items)) #f)
        (for-each f (cdr items)))))


; 24)

(list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))
; |1| | -> | |nil| 
;		    \---> |2| | -> | |nil|
;                            \---> |3| | -> |4|nil|

; 25)
(define a (list 1 3 (list 5 7) 9))
(= 7 (cadr (caddr a)))
(define b (list (list 7)))
(= 7 (caar b))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(= 7 (cadr (cadr (cadr (cadr (cadr (cadr c)))))))

; 26)
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)
(cons x y)
; ((1 2 3) . (4 5 6))
(list x y)
;((1 2 3) (4 5 6))

; 27)

(define (deep-reverse items)
  (cond ((null? items)  ())
		((list? items) (map deep-reverse (reverse items)))
		((pair? items) (cons (deep-reverse (car items)) 
							 (deep-reverse (cdr items))))
		(else items)))


; 28)
(define (fringe tree)
  (cond ((null? tree) ())
		((not (pair? tree)) (list tree))
		(else (append (fringe (car tree)) 
					  (fringe (cdr tree))))))

; 29)

(define (make-mobile left right)
  (list left right))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (mobile? m)
  (and (pair? m)
	   (branch? (left-branch m))
	   (branch? (right-branch m))))

(define (make-branch length structure)
  (list length structure))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

(define (simple-branch? b)
  (and (pair? b)
	   (number? (branch-length b))
	   (number? (branch-structure b))))

(define (mobile-branch? b)
  (and (pair? b)
	   (number? (branch-length b))
	   (mobile? (branch-structure b))))


(define (branch? b) 
  (or (simple-branch? b)
	  (mobile-branch? b)))

(define (total-weight m)
  (cond ((null? m) 0)
		((number? m) m)
		((simple-branch? m) (branch-structure m))
		((mobile-branch? m) (total-weight (branch-structure m)))
		((mobile? m) (+ (total-weight (left-branch m))
						(total-weight (right-branch m))))))

	
(define (branch-torque b)
  (* (branch-length b) (total-weight b)))

(define (balance? m)
  (= (branch-torque (left-branch m)) 
	 (branch-torque (right-branch m))))


(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch m)
  (cdr m))

(define (branch-structure b)
  (cdr b))

(define mobile (make-mobile (make-branch 1 20)
							(make-branch 2 
										 (make-mobile (make-branch 3 30) 
													  (make-branch 4 40)))))

(define balanced-mobile (make-mobile (make-branch 1 mobile)
									 (make-branch 2 45)))
; 30)

(define (square-tree tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
			 (square-tree sub-tree)
			 (square sub-tree)))
	   tree))

(define (square-tree tree)
  (cond ((null? tree) ())
		((not (pair? tree)) (square tree))
		(else (cons (square-tree (car tree))
					(square-tree (cdr tree))))))

; 31)

(define (map-tree f tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
			 (map-tree f sub-tree)
			 (f sub-tree)))
	   tree))

(define (square-tree tree)
  (map-tree square tree))

; 32)

(define (subsets s)
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) 
                          (cons (car s) x))
                        rest)))))

; 33)

(load "sequences-as-interfaces.scm")

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
			  () 
			  sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1))
			  0
			  sequence))

; 34)

(define (horner-eval x coefficients)
  (accumulate (lambda (coefficient higher-terms) (+ (* x higher-terms)
													coefficient))
			  0
			  coefficients))

; 35)

(define (count-leaves t)
  (accumulate +
			  0
			  (map (lambda (x)
					 (if (pair? x)
						 (+ (count-leaves (list (car x)))
							(count-leaves (cdr x)))
						 1))
				   t)))


; 36)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  ()
	  (cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))

; 37)

(define (sum sequence)
  (accumulate + 0 sequence))

(define (dot-product v w)
  (sum (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
	(map (lambda (r) (matrix-*-vector n-cols r)) m)))

; 38)

(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest)
		result
		(iter (op result (car rest))
			  (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 '(1 2 3))
; 3/2

(fold-left / 1 '(1 2 3))
; 1/6

(fold-right list () '(1 2 3))
; (1 (2 (3 ())))

(fold-left list () '(1 2 3))
; (((() 1) 2) 3)

; fold-right op init sequence == fold-left op init sequence 
; if and only if op satisfies the constraint that
; (= (op a b) (op b a)) for all a and b
; examples: +, *

; 39)

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence)) 

