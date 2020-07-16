; Testing the difference in performance given a 
; derived expression which would only need to be transformed
; once using the analysis process.
(define (factorial n)
  (cond ((= n 0) 1)
        (else (* n (factorial (- n 1))))))

