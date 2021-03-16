(define (assign register-name . operands)
  ; operands can be one of:
  ; ((reg other-register-name))
  ; ((const constant-value))
  ; ((label label-name))
  ; ((op operation-name) . inputs)
  )

(define (perform operation . inputs)
  )

(define (test operation . inputs)
  )

(define (branch label)
  )

(define (goto label)
  ; label can be a label or a register containing a label
  )

(define (save register-name)
  )

(define (restore register-name)
  )


(define (make-register register-name)
  ('reg register-name))

(define (make-const constant-value)
  ('const constant-value))

(define (make-label label-name)
  ('label label-name))

(define (make-operation operation-name)
  ('op operation-name))

