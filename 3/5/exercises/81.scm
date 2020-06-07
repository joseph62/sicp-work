(define (rand-update n)
  (+ n 1))

(define (rand start ops)
  (let ((op (stream-car ops)))
    (cond ((eq? 'reset (car op))
           (rand (cadr op) (stream-cdr ops)))
          ((eq? 'generate (car op))
           (cons-stream
             start
             (rand (rand-update start)
                   (stream-cdr ops)))))))
