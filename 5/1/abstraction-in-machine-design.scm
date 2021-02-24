(define (remainder (n d))
  (if (< n d)
      n
      (remainder (- n d) d)))


(controller
  gcd-loop
    (assign a (op (read)))
    (assign b (op (read)))
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
  remainder-start
    (test (op <) (reg t) (reg b))
    (branch (label remainder-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label rem-loop))
  remainder-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done
    (perform (op print) (reg a))
    (goto (label gcd-loop)))
