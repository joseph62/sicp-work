(sum ?amount
     (and (job ?x (computer programmer))
          (salary ?x ?amount)))

(accumulation-function 'variable 'query-pattern)

; Ben's original approach won't work because the system can create duplicate frames
; that match similar, but not the same criteria (As in exercise 44 where Oliver Warbucks 
; matches four different ways). One approach that Ben could take to solve the issue
; of duplicates for his accumulation functionality would be to maintain a set of 
; already matched frames so that duplicate frames do not factor into the accumulation.
