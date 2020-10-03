(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream))))))

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (flatten-stream (stream-cdr stream)))))

; The delay evaluation is critical for the stream to continue to be lazily evaluatied.
; If the implementation of flatten-stream did not have the delay evaluation, it would
; result in an entirely evaluated stream of streams.
