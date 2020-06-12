; a)
; The issue with moving the application evaluation up in the cond is that
; the predicate for application is broad enough to evaluate to true for 
; just about every type of expression. For example if the definition check
; were to happen after the application check. The eval procedure would treat
; every definition as an application of the procedure 'define' on the rest of the expression

; b)

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))

