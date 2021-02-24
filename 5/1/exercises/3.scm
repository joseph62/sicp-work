; Computing square roots using Newton's method
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; Machine with `good-enough?` and `improve` primitives

(controller 
  sqrt-start
    (assign x (op read))
    (assign guess (constant 1.0))
  test-guess
    (test (op good-enough?) (reg x) (reg guess))
    (branch (label sqrt-done))
    (assign guess (op improve) (reg x) (reg guess))
    (goto (label test-guess))
  sqrt-done
    (perform (op print) (reg guess)))

; Machine without `good-enough?` and `improve` primitives

(controller 
  sqrt-start
    (assign x (op read))
    (assign guess (constant 1.0))
  test-guess
  good-enough?-start
    (assign square-guess (op *) (reg guess) (reg guess))
    (assign guess-x-diff (op -) (reg square-guess) (reg x))
  abs-start
    (test (op <) (reg guess-x-diff) (constant 0))
    (branch (label abs-end))
    (assign t (op -) (constant 0) (reg guess-x-diff))
    (assign guess-x-diff (reg t))
  abs-end
    (test (op <) (reg guess-x-diff) (constant 0.0001))
    (branch (label sqrt-done))
  good-enough?-end
    (branch (label sqrt-done))
  improve-start
    (assign x-guess-div (op /) (reg x) (reg guess))
    (assign x-guess-div-guess-sum (op +) (reg guess) (reg x-guess-div))
    (assign guess (op /) (reg x-guess-div-guess-sum) (constant 2))
  improve-end
    (goto (label test-guess))
  sqrt-done
    (perform (op print) (reg guess)))
