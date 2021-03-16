; Example recursive processes
; the result of the computation depends on 
; subsequent factorial computations
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

; Example of an iterative process
; the answer to subsequent calls to gcd are the 
; same as the answer to previous calls.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(controller
  factorial-start
    (test (op =) (reg n) (constant 1))
    (branch (label factorial-base))
    (save n)
    (save continue)
    (assign n (op -) (reg n) (constant 1))
    (assign continue (label factorial-sub-call-finished))
    (goto (label factorial-start))
  factorial-sub-call-finished
    (restore continue)
    (restore n)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
  factorial-base
    (assign val (constant 1))
    (goto (reg continue))
  factorial-end)


; DOUBLE RECURSION

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(controller
  fib-start
    (test (op <) (reg n) (constant 2))
    (branch (label fib-base))
    (save continue)
    (save n)
    (assign continue (label fib-n-1-done))
    (assign n (op -) (reg n) (constant 1))
    (goto (label fib-start))
  fib-n-1-done
    (restore n)
    (assign n (op -) (reg n) (constant 2))
    (assign continue (label fib-n-2-done))
    (save val)
    (goto (label fib-start))
  fib-n-2-done
    (assign n (reg val))
    (restore val)
    (restore continue)
    (assign n (op +) (reg n) (reg val))
    (goto (reg continue))
  fib-base
    (assign val (reg n))
    (goto (reg continue))
  fib-done)
    
