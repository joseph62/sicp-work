(define nil ())
(define one-through-four (cons 1 (cons 2 (cons 3 (cons 4 ())))))
(define one-through-four-again (list 1 2 3 4))
(car one-through-four)
; 1
(cdr one-through-four)
; (2 3 4)
(car (cdr (one-through-four)))
; 2
(cons 10 one-through-four)
; (10 1 2 3 4)
(cons 5 one-through-four)
; (5 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (iter items total)
    (if (null? items)
      total
      (iter (cdr items) (+ total 1))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (scale-list items factor)
  (if (null? items)
    ()
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) 
       items))

(define (map proc items)
  (if (null? items)
    ()
    (cons (proc (car items))
          (map proc (cdr items)))))
