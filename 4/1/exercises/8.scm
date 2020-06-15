(define (make-procedure-definition name arguments body)
  (cons 'define (cons (cons name arguments) body)))

(define (tagged-list? items value)
  (eq? (car items) value))

(define (named-let-variable exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

(define (named-let? exp)
  (and (tagged-list? exp 'let)
	   (symbol? (named-let-variable exp))))

(define (make-lambda variables body)
  (cons 'lambda (cons variables body)))


(define (let->combination exp)
  (if (named-let? exp)
	(let ((bindings (named-let-bindings exp))
		  (body (named-let-body exp))
		  (name (named-let-variable exp)))
	  (let ((variables (binding-variables bindings))
			(expressions (binding-exps bindings)))
		(list 'begin 
			  (make-procedure-definition name variables body)
			  (make-application name expressions)))) 
	(let ((bindings (let-bindings exp))
		  (body (let-body exp)))
	  (let ((variables (binding-variables bindings))
			(expressions (binding-exps bindings)))
		(make-application (make-lambda variables body)
						  expressions)))))

(let ((fib-iter (lambda (a b count)
				  (if (= count 0)
					b
					(fib-iter (+ a b) a (- count 1))))))
			   (fib-iter 1 0 5))
