(define (frame-find-binding var frame)
  (assoc var (frame-bindings frame)))

(define (env-loop frame-proc end-proc env)
  (if (eq? env the-empty-environment)
    (end-proc)
    (let* ((frame (first-frame env))
           (result (frame-proc frame)))
      (if result
        result
        (env-loop frame-proc 
                  end-proc 
                  (enclosing-environment env))))))

(define (lookup-variable-value var env)
  (env-loop (lambda (frame)
              (let ((binding (frame-find-binding var frame)))
                (if binding
                  (binding-value binding)
                  false)))
            (lambda ()
              (error "Unbound variable" var))
            env))

(define (set-variable-value! var val env)
  (env-loop (lambda (frame)
              (let ((binding (frame-find-binding var frame)))
                (if binding
                  (set-binding-value! val binding)
                  false)))
            (lambda ()
              (error "Unbound variable" var))
            env))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (frame-find-binding var frame)))
    (if binding
      (set-binding-value! val binding)
      (add-binding-to-frame! var val frame))))

