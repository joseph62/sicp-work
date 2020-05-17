(define ln-2-summands 
  (stream-map (lambda (i)
                (if (even? i)
                    (/ -1.0 i)
                    (/ 1.0 i)))
              integers))


(define ln-2 (partial-sums ln-2-summands))
; in 100 iterations this gets ln 2 down to 2 significant figures

(define euler-ln-2 (euler-transform ln-2))
; in 100 iterations this gets ln 2 down to 6 significant figures 

(define acc-ln-2 (accelerated-sequence euler-transform ln-2))
; in 4 iterations this gets ln 2 down to 7 significant figures 
