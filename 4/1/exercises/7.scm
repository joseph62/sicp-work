; let* can be expanded to nested let expressions in the following way
(let* ((x 3)
	   (y (+ x 2))
	   (z (+ x y 5)))
  (* x z))

(let ((x 3))
  (let ((y (+ x 2)))
	(let ((z (+ x y 5)))
	  (* x z))))

; If we already have the evaluation process for let defined then it is sufficient to
; add only the evaluation rule:
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

; The let*->nested-lets procedure will convert our let* expression into
; a let expression that can be evaluated by our eval-let procedure

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let*->nested-lets exp)
  (let ((bindings (let-bindings exp))
		(body (let-body exp)))
	(fold-right (lambda (binding body)
				  (make-let (list binding) body))
				body
				bindings)))
