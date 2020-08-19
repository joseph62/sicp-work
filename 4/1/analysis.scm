(load "running-evaluator.scm")
(load "exercises/22.scm")

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze-if (cond->if exp)))
        ; Exercise 4.22
        ((let? exp) (analyze-application (let->application exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((predicate (analyze (if-predicate exp)))
        (consequent (analyze (if-consequent exp)))
        (alternative (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (predicate env))
          (consequent env)
          (alternative env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (body (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars body env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (reduce sequentially (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((procedure (analyze (operator exp)))
        (arguments (map analyze (operands exp))))
    (lambda (env)
      (execute-application (procedure env)
                           (map (lambda (argument) (argument env))
                                arguments)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
          (error
            "Unknown procedure type -- EXECUTE-APPLICATION"
            args))))
