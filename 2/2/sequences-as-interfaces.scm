(define (gte a b)
  (or (> a b)
	  (= a b)))

(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons  (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? '(1 2 3 4 5 6 7))
; (1 3 5 7)

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (sum items)
  (accumulate + 0 items))

(define (enumerate-interval start end)
  (if (> start end)
	  ()
	  (cons start (make-range (+ start 1) 
							  end))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
			  ()
			  (filter odd?
					  (map square
						   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
			  ()
			  (filter even?
					  (map fib
						   (enumerate-interval 0 n)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (list-fib-squares n)
  (accumulate cons
			  ()
			  (map (compose square fib)
				   (enumerate-interval 0 n))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
			  1
			  (map square
				   (filter odd? sequence))))

(define (make-programmer salary) (make-record 'programmer salary))
(define (make-sales salary) (make-record 'sales salary))
(define (make-record role salary) (cons role salary))

(define (role record)
  (car record))

(define (salary record)
  (cdr record))

(define (programmer? record)
  (equal? (role record) 'programmer))

(define (highest-paid-programmer records)
  (accumulate max
			  0
			  (map salary
				   (filter programmer? records))))

(define records (list (make-programmer 200)
					  (make-sales 300)
					  (make-programmer 100)
					  (make-sales 500)
					  (make-programmer 600)))

(define (flatmap f seq)
  (accumulate append () (map f seq)))



(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
		(cadr pair) 
		(+ (car pair) 
		   (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (flatmap 
				 (lambda (i) 
				   (map (lambda (j) (list i j)) 
						(enumerate-interval 1 (- i 1)))) 
				 (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
	  (list ())
	  (flatmap (lambda (x) 
				 (map (lambda (p) (cons x p))
					  (permutations (remove x s))))
			   s)))

(define (remove item sequence)
  (filter (lambda (x) (not (equal? x item)))
		  sequence))


