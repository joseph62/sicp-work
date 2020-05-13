(define fibs
  (cons-stream 
    0
    (cons-stream 
      1
      (add-streams (stream-cdr fibs)
                   fibs))))

; With the memo-proc optimization on delay this procedure
; the nth number in this sequence only requires 1 more
; addition procedure than the n-1th number in the sequence

; Without the memoization, the nth number would require n 
; addition procedures to take place.
