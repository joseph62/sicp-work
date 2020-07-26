; The lazier streams allow items in the stream to not be defined
; by the time they are extracted from the stream.

(eval '(begin
         (define add-to-x (cons 
                            (+ x 1)
                            (cons 
                              (+ x 2)
                              (cons
                                (+ x 3)
                                ()))))
         (define x 1))
         ; (car add-to-x)  ; 2 << forced from repl
         ; (define x 3)
         ; (car (add-to-x)) ; 2
         ; (car (cdr add-to-x))) ; 5
         ; With memoization
      the-global-environment)

; New values coming out of the stream can be modified if
; the conditions of the expression change from the definition
; of the stream to the actual execution of an expression coming
; off of that stream.


