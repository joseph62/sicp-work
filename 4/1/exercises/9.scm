(define (make-lambda parameters body)
  (list 'lambda parameters body))

(define (make-one-armed-if predicate consequent)
  (list 'if predicate consequent))

(define (make-begin exps)
  (cons 'begin exps))

(define (while? exp)
  (tagged-list? exp 'while))

(define (while-predicate exp) (cadr exp))

(define (while-body exp) (cddr exp))

(define (while->procedure exp)
  (let ((f (make-lambda 
			 '(f) 
			 (make-one-armed-if 
			   (while-predicate exp)
			   (make-begin 
				 (append (while-body exp) 
						 '((f f)))))))) 
	(list f f)))

(define (eval-while exp env)
  (eval (while->procedure exp) env))

(define count 0)
(define while-example '(while (< count 10)
							  (display count)
							  (newline)
							  (set! count (+ count 1))))

; 0
; 1
; 2
; ...
; 9

