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

; 56)
; In symbolic-differentiation.scm

; 57)
; In symbolic-differentiation.scm
