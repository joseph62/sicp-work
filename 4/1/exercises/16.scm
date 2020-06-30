; a)
(define (lookup-variable-value var env)
  (env-loop (lambda (frame)
              (let ((binding (frame-find-binding var frame)))
                (cond ((not (binding)) false)
                      ((eq? (binding-value binding) '*unassigned*)
                       (error "Variable unassigned" 
                              (binding-variable binding)))
                      (else (binding-value binding)))))
            (lambda ()
              (error "Unbound variable" var))
            env))

; b)
(define (scan-out-defines body)
  (make-let
    (map (lambda (var)
           (list (definition-variable var) '*unassigned*))
         (filter definition? body))
    (map (lambda (exp)
           (if (definition? exp)
             (list 'set! 
                   (definition-variable exp)
                   (definition-value exp))
             exp))
         body)))
         

; c)
; I decided to add scan-out-defines to the make-procedure
; procedure so that the transformation need only happen once
; not on every access.
