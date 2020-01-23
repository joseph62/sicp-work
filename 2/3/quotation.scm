(define a 2)
(define b 3)

(list a b)
; (2 3)

(list 'a b)
; (a 3)

(list 'a 'b)
; (a b)

(car '(a b c))
; a

(cdr '(a b c))
; (b c)

(define (memq item items)
  (cond ((null? items) ())
		((eq? item (car items)) items)
		(else (memq item (cdr items)))))

