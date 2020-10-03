; a)
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter stream-null? stream)))

; b)
; The behavior of the query language evaluator would remain the same.
