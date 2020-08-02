; Using integers-starting-from to create infinite possibilities
; would not work when using an approach like the one from the book.
; Each number in the triple is provided by integers-starting-from
; which is not exhaustable. So the result would be triples that
; only change the third value.

(define (interleave-amb x y)
  (amb x y (interleave-amb y x)))

(define (triples-starting-from x y z)
  (amb (list x y z)
       (interleave-amb (interleave-amb (list x 
                                             y 
                                             (integers-starting-from (+ z 1)))
                                       (list x 
                                             (integers-starting-from (+ y 1))
                                             (integers-starting-from (+ z 1))))
                       (triples-starting-from (+ x 1) (+ y 1) (+ z 1)))))
