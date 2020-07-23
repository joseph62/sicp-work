(define (fib n)
  (if (= n 0)
      1
      (* n (fib (- n 1)))))

(define (display-n n)
  (display "The ")
  (if (even? n) 
      (display "even ")
      (display "odd "))
  (display "number is ")
  (display n)
  (newline))

(display-n (fib 14))

; Without memoization the (fib 14) expression 
; would need to be calculated twice. When
; used with the display-n procedure.

; With memoization
(square (id 10))
; 100

; count
; 1
; Without memoization, I would expect this value to be 2 because the value
; for the square operator is used twice and therefore evaluated twice.
