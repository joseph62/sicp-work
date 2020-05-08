(define x 10)
(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

; Possible outcomes:
; 101
; 121
; 11
; 100

; The set operations can run at the same time 
; so it is possible for one to overwrite the other.

