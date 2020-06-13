(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		(else ((get 'eval (car exp)) (cdr exp) env))))

