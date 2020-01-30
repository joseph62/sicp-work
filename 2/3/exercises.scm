; 53)

(list 'a 'b 'c)
; (a b c)

(list (list 'george))
; ((george))

(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; (y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; ()

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; 54)
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
		((not (eq? (car a) (car b))) #f)
		(else (equal? (cdr a) (cdr b)))))

; 55)
; 'ab functionally (quote ab)
; ''ab functionally (quote (quote ab))
; the ' is syntactic sugar for (quote ...)
; so ''a is '(quote a)

; 56 - 58 in symbolic-differenctiation.scm
; 58 b - Not attempted

; 59 - in representing-sets.scm

; 60)
(define (element-of-set? x set)
  (null? (filter (lambda (y) (equal? x y)) set)))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set s1 s2)
  (filter (lambda (x) (element-of-set? x s1) s2)))

(define (union-set s1 s2)
  (fold-right adjoin-set s2 s1))

; This implementation of a set is very good for adding
; elements and much less efficient for membership 
; dependent operations as the size of the set will grow 
; with every single addition rather than when the addition is unique
