; unless as a derived expression
(define (unless-condition exp)
  (cadr exp))

(define (unless-exceptional exp)
  (cadddr exp))

(define (unless-usual exp)
  (caddr exp))
  
(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless->if exp)
  (list 'if
        (unless-condition exp)
        (unless-exceptional exp)
        (unless-usual exp)))

; unless as a procedure use case
; using unless in conjunction with map.
(map unless 
     (map zero? '(0 1 0 0 1 1 1 0 0 1))
     '(1 2 3 4 5 6 7 8 9 10)
     '(10 9 8 7 6 5 4 3 2 1))

