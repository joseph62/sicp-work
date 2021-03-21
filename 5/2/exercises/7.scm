; A)
(define expt-machine
  (make-machine
    '(n continue b sub-b)
    (list (list '= =)
          (list '- -)
          (list '* *))
    '(expt-begin
        (test (op =) (reg n) (constant 0))
        (branch (label expt-base))
        (save continue)
        (save b)
        (assign continue (label expt-sub-done))
        (assign n (op -) (reg n) (constant 1))
        (goto (label expt-begin))
      expt-sub-done
        (assign sub-b (reg b))
        (restore b)
        (restore continue)
        (assign b (op *) (reg b) (reg sub-b))
        (goto (reg continue))
      expt-base
        (assign b (constant 1))
        (goto (reg continue))
      expt-end)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 3)
(start expt-machine)

(get-register-contents expt-machine 'b)
; 8

; B)
(define expt-machine-2
  (make-machine
    '(b n counter product val continue)
    (list (list '= =)
          (list '- -)
          (list '* *))
    '(  (assign counter (reg n))
        (assign product (constant 1))
      expt-begin
        (test (op =) (reg counter) (constant 0))
        (branch (label expt-base))
        (assign counter (op -) (reg counter) (constant 1))
        (assign product (op *) (reg b) (reg product))
        (goto (label expt-begin))
      expt-base
        (assign val (reg product))
        (goto (reg continue))
      expt-end)))


(set-register-contents! expt-machine-2 'b 4)
(set-register-contents! expt-machine-2 'n 2)
(start expt-machine-2)

(get-register-contents expt-machine-2 'b)
; 16
