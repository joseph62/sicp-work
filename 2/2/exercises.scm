; 17)

(define (last-pair items)
  (cond ((null? items) ()) 
         ((null? (cdr items)) (car items))
         (else (last-pair (cdr items)))))

; 18)

(define (reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
  (iter items ()))

; 19)

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (no-more? denominations) 
  (null? denominations))

(define (first-denomination denominations)
  (car denominations))

(define (except-first-denomination denominations)
  (cdr denominations))

(define (cc amount denominations)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? denominations)) 0)
        (else (let ((denomination (first-denominations denominations))
                    (other-denominations (except-first-denomination denominations)))
                (+ (cc amount 
                       (except-first-denomination denominations))
                   (cc (- amount 
                          (first-denomination denominations))
                       denominations))))))

; 20)

(define (same-parity first . rest)
  (let ((parity (if (even? first) even? odd?)))
    (define (iter numbers)
      (cond ((null? numbers) ())
            ((parity (car numbers)) (cons (car numbers) 
                                          (iter (cdr numbers))))
            (else (iter (cdr numbers)))))
    (cons first (iter rest))))

; 21)

(define (square-list numbers)
  (if (null? numbers) 
    () 
    (cons (square (car numbers)) 
          (square-list (cdr numbers)))))

(define (square-list numbers)
  (map square numbers))

; 22)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) 
            (cons (square (car things))
                  answer))))
  (iter items ()))

; (square-list (list 1 2 3))
; (iter (1 2 3) ())
; (iter (2 3) (cons (square 1) ()))
; (iter (3) (cons 2 (1)))
; (iter () (cons 3 (2 1)))
; (3 2 1)
; As this procedure processes items it adds them to the start of the list.
; This results in a reversed list in the end.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items ()))

; (square-list (list 1 2 3))
; (iter (2 3) (cons () 1))
; (iter (3) (cons (() . 1) 2))
; (iter () (cons ((() . 1) . 2) 3))
; (((() . 1) . 2) . 3)
; This solution creates a pair of the previous iteration and the next iteration.

; Correct solution would be:
; (1 . ())
; (1 . (2 . ()))
; (1 . (2 . (3 . ())))
; Must be able to add a pair on to the end of the list

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer (list (car things))))))
  (iter items ()))

; OR

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (reverse (iter items ())))

; 23)

(define (for-each f items)
  (if (null? items)
    #t
    (or (and (f (car items)) #f)
        (for-each f (cdr items)))))



