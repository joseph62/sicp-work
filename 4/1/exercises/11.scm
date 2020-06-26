(define (make-frame variables values)
  (cons '*frame* (zip variables values)))

(define (frame-bindings frame)
  (cdr frame))

(define (frame-variables frame)
  (map car (cdr frame)))

(define (frame-values frame)
  (map cadr (cdr frame)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (list var val)
                        (cdr frame))))

(define (binding-variable binding) (car binding))
(define (binding-value binding) (cadr binding))
(define (set-binding-value! value binding)
  (set-car! (cdr binding) value))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (let ((binding (assoc var bindings)))
        (if binding
          (binding-value binding)
          (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-bindings frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (let ((binding (assoc var bindings)))
        (if binding
          (set-binding-value! val binding)
          (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-bindings frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (let ((binding (assoc var bindings)))
        (if binding
          (set-binding-value! val binding)
          (add-binding-to-frame! var val frame))))
    (scan (frame-bindings frame))))

