; Without subroutines
(controller
  gcd-1
    (test (op =) (reg b) (const 0))
    (branch (label after-gcd-1))
  remainder-1-start
    (test (op <) (reg t) (reg b))
    (branch (label remainder-1-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label remainder-1-start))
  remainder-1-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label gcd-1))
  after-gcd-1
  ; ....
  gcd-2
    (test (op =) (reg b) (const 0))
    (branch (label after-gcd-2))
  remainder-2-start
    (test (op <) (reg t) (reg b))
    (branch (label remainder-2-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label remainder-2-start))
  remainder-2-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label gcd-2))
  after-gcd-2)

; With subroutine
(controller
  gcd 
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label gcd))
  gcd-done
    (goto (reg continue))
  ; Calling:
    (assign continue (label after-gcd-1))
    (goto (label gcd))
  after-gcd-1
  ; Another call
    (assign continue (label after-gcd-2))
    (goto (label gcd))
  after-gcd-2)

