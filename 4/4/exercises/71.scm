(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) 
                      frame-stream)))))

; Louis' versions

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append (find-assertions query-pattern frame)
                     (apply-rules query-pattern frame)))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave
      (qeval (first-disjunct disjuncts) frame-stream)
      (disjoin (rest-disjuncts disjuncts) frame-stream))))

; These definitions result in undesirable behavior for
; queries which fall into infinite loops for things such
; as the reflexive body of the married rule. The delay operations
; allow for the stream to be returned without starting the apply-rules or 
; further disjuncts until necessary.
