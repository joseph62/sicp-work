; make-unbound! will attempt to remove the given binding for
; the first frame of the environment only.
; This will keep unbinding from being able to modify
; frames past the first and potentially remove something
; another part of the program was using.
(define (make-unbound! var env)
  (let* ((frame (first-frame env)))
    (unbind-frame-binding! var frame)))

(define (unbind-frame-binding! var frame)
  (set-cdr! frame
            (filter 
              (lambda (binding)
                (not (eq? var (binding-variable binding))))
              (frame-bindings frame))))
              

