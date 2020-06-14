; Directly Defined
(define (install-eval-and)
  (define first-exp (get 'first-exp '(exps)))
  (define rest-exp (get 'rest-exp '(exps)))
  (define false? (get 'false? '(exp)))
  (define (eval-and exps env)
	(cond ((last-exp? exps) (eval (first-exp exps) env))
		  ((false? (eval (first-exp exps) env)) 'false)
		  (else (eval-and (rest-exps exps) env))))
  (put 'eval '(and env) eval-and)
  'ok)

(define (install-eval-or)
  (define first-exp (get 'first-exp '(exps)))
  (define rest-exp (get 'rest-exp '(exps)))
  (define true? (get 'true? '(exp)))
  (define (eval-or exps env)
	(cond ((last-exp? exps) (eval (first-exp exps) env))
		  ((true? (eval (first-exp exps) env)) 'true)
		  (else (eval-or (rest-exps exps) env))))
  (put 'eval '(or env) eval-or)
  'ok)

; Derived Expressions
(define (install-derived-eval-or)
  (define (or->if exps)
	(if (null? exps)
	  'false
	  (let ((first (first-exp exps))
			(rest (rest-exps exps)))
		(make-if first
				 'true
				 (or-if rest-exps)))))
  (define (eval-or exps env)
	(eval (or->if exps) env))

  (put 'eval '(or env) eval-or)
  'ok)

(define (install-derived-eval-and)
  (define make-if (get 'make-if '(exp exp exp)))
  (define (and->if exps)
	(if (null? exps)
	  'true
	  (let ((first (first-exp exps))
			(rest (rest-exps exps)))
		(make-if first
				 (and-if rest-exps)
				 'false))))

  (define (eval-and exps env)
	(eval (and->if exps) env))

  (put 'eval '(and env) eval-and)
  'ok)
