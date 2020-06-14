(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (first-binding bindings) (car bindings))

(define (rest-bindings bindings) (cdr bindings))

(define (binding-variable binding) (car binding))

(define (binding-exp binding) (cadr binding))

(define (binding-variables bindings) (map binding-variable bindings))
(define (binding-exps bindings) (map binding-exp bindings))

(define (make-application procedure exps) (cons procedure exps))

(define (let->combination exp)
  (let ((bindings (let-bindings exp))
		(body (let-body exp)))
	(let ((variables (binding-variables bindings))
		  (expressions (binding-values bindings)))
	  (make-application (make-lambda variables body)
						expressions))))

(define (eval-let exp env)
  (eval (let->combination exp) env))

