(define (make-table)
  (list '*table*))

(define (records table)
  (cdr table))

(define (lookup key table)
  (let ((record (assoc key (records table))))
	(if record
		(cdr record)
		#f)))

(define (assoc key records)
  (cond ((null? records) #f)
		((equal? key (caar records)) (car records))
		(else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (records table))))
	(if record
		(set-cdr! record value)
		(set-cdr! table
				  (cons (cons key value) (cdr table)))))
  'ok)

; 2d table
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (records table))))
	(if subtable
		(let ((record (assoc key-2 subtable)))
		  (if record
			  (cdr record)
			  #f))
		#f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (records table))))
	(if subtable
		(let ((record (assoc key-2 subtable)))
		  (if record
			  (set-cdr! record value)
			  (set-cdr! subtable
						(cons (cons key-2 value)
							  (cdr subtable)))))
		(set-cdr! table
				  (cons (list key-1
							  (cons key-2 value))
						(records table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
	(define (records table)
	  (cdr table))
	(define (lookup key-1 key-2)
	  (let ((subtable (assoc key-1 (records local-table))))
		(if subtable
			(let ((record (assoc key-2 subtable)))
			  (if record
				  (cdr record)
				  #f))
			#f)))
	(define (insert! key-1 key-2 value)
	  (let ((subtable (assoc key-1 (records local-table))))
		(if subtable
			(let ((record (assoc key-2 subtable)))
			  (if record
				  (set-cdr! record value)
				  (set-cdr! subtable
							(cons (cons key-2 value)
								  (cdr subtable)))))
			(set-cdr! local-table
					  (cons (list key-1
								  (cons key-2 value))
							(records local-table)))))
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup) lookup)
			((eq? m 'insert!) insert!)
			(else (error "Unkown operation -- TABLE" m))))
	dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))
