; A)

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(controller
  ; registers b and n set by caller
  expt-begin
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
  expt-end)

; B)
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(controller
  ; registers b and n set by caller
    (assign counter (reg n))
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
  ; assign register val with the answer and return to caller
  expt-end)

  
    
