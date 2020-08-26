(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))
; The result of running this in the amb evaluator would be all of the prime-sum-pairs in
; the combination of those two number lists. The pairs variable will be permanently updated every time
; a prime sum pair is found.
