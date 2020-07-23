
(define example-f '(define (f a (b lazy) c (d lazy-memo))
                     (lambda ()
                       (+ a b c d))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameter-names procedure)
             (list-of-compound-arguments
               (procedure-parameters procedure)
               arguments 
               env)
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(define (list-of-compound-arguments parameters arguments env)
  (map (lambda (parameter argument)
         (cond ((operand-eager? parameter) (eval argument env))
               ((operand-memo? parameter) (delay-with-memoization argument env))
               (else (delay-it argument env)))) 
       parameters
       arguments))

(define (operand-type operand)
  (if (pair? operand)
      (cadr operand)
      'eager))

(define (operand-name operand)
  (if (operand-eager? operand)
      operand
      (car operand)))

(define (operand-eager? operand)
  (eq? (operand-type operand) 'eager))

(define (operand-lazy? operand)
  (or (eq? (operand-type operand) 'lazy)
      (eq? (operand-type operand) 'lazy-memo)))

(define (operand-memo? operand)
  (eq? (operand-type operand) 'lazy-memo))

(define (delay-with-memoization exp env)
  (list 'memoized-thunk exp env))

(define (memoized-thunk? obj)
  (tagged-list? obj 'memoized-thunk))

(define (force-it obj)
  (cond ((memoized-thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
         (set-car! obj 'evaluated-thunk)
         (set-car! (cdr obj) result)
         (set-cdr! (cdr obj) '())
         result))
        ((evaluated-thunk? obj) (thunk-value obj))
        ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))

(define (procedure-parameter-names procedure)
  (map operand-name (procedure-parameters procedure)))
