(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (first-binding bindings) (car bindings))

(define (rest-bindings bindings) (cdr bindings))

(define (binding-variable binding) (car binding))

(define (binding-exp binding) (cadr binding))

(define (binding-variables bindings) (map binding-variable bindings))
(define (binding-exps bindings) (map binding-exp bindings))

(define (make-application procedure parameters)
  (cons procedure parameters))

(define (let->application exp)
  (let ((bindings (let-bindings exp))
		(body (let-body exp)))
	(let ((variables (binding-variables bindings))
		  (expressions (binding-exps bindings)))
	  (make-application (make-lambda variables body)
						expressions))))
