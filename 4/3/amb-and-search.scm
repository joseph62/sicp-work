(define (amb . possibilities)
  (if (null? possibilities)
      ()
      (car possibilities))) 

(list (amb 1 2 3) (amb 'a 'b))
; Possible values:
; (1 a) (1 b) (2 a) (2 b) (3 a) (3 b)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
