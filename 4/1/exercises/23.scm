(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

; One expression:
; In the case of one expression the work done by this version
; analyze-sequence during execution is roughly the same as the
; text version. This will still do slightly more work to execute
; the sequence in the form of a null check before executing the
; expression.

; Two expressions:
; In the case of more than one expression in the sequence. The version
; provided above would do much more work during execution than that
; of the version provided in the text. For every expression in 
; the sequence there will be an additional check for the end.
; The text version, however, executes each statement in sequence
; without any checking between expressions.
