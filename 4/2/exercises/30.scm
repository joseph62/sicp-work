(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; a)
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          '(57 321 88))

; for-each works here because newline and display are
; primitive procedures. Those expressions will be evaluated
; in an eager manner.

; b)

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

; with original eval-sequence
; (p1 1)
; (1 2)

; (p2 1)
; 1

; with eval-sequence-force
; (p1 1)
; (1 2)

; (p2 1)
; (1 2)

; c)

; The for-each example evaluates as expected with the forced eval-sequence 
; because that version always evaluates sequences.

; d) I think it is better to force evaluate sequences as it allows for a consistent 
; behavior for sequences.
