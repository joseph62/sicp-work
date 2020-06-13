(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		(else ((get 'eval (list (car exp) 'env)) (cdr exp) env))))

