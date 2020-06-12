(define (lov-left-to-right exps env)
  (if (no-operands? exps)
	()
	(let ((value (eval (first-operand exps) env))
		  (rest-values (lov-left-to-right (rest-operands exps) env)))
	  (cons value
			rest-values))))

(define (lov-right-to-left exps env)
  (if (no-operands? exps)
	()
	(let ((rest-values (lov-left-to-right (rest-operands exps) env))
		  (value (eval (first-operand exps) env)))
	  (cons value
			rest-values))))
