(define (make-machine register-names operations controller)
  '(machine smile))

(define (set-register-contents! machine-model register-name value)
  'done)

(define (get-register-contents machine-model register-name)
  'value)

(define (start machine-model)
  'done)

; Example of simulator interface usage

(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

