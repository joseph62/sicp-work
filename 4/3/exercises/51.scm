(define (assignment? exp)
  (or (standard-assignment? exp)
      (permanent-assignment? exp)))

(define (standard-assignment? exp)
  (tagged-list? exp 'set!))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (if (standard-assignment? exp)
                 (lambda (val fail2)
                   (let ((old-value
                           (lookup-variable-value var env)))
                     (set-variable-value! var val env)
                     (succeed 'ok
                              (lambda ()
                                (set-variable-value! var 
                                                     old-value
                                                     env)
                                (fail2)))))
                 succeed)
             fail))))
