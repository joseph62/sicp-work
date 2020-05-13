(define (partial-sums stream)
  (define (accumulate stream current)
    (let ((next (+ current (stream-car stream))))
      (cons-stream next
                   (accumulate (stream-cdr stream) next))))
  (accumulate stream 0))
