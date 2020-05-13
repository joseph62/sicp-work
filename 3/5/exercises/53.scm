(define s (cons-stream 1 (add-streams s s)))

; 1 2 4 8 ...
; The s stream is the double stream
