(lambda ()
  (define u 1)
  (define v 2)
  (+ u v))

; E1 lambda -> (define u 1)
; u: 1

; E1 lambda -> (define v 2)
; u: 1
; v: 2

; E1 lambda -> (+ u v)
; u: 1
; v: 2

(lambda ()
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u 1)
    (set! v 2)
    (+ u v)))

; E1 lambda -> let (...)

; E2 let -> E1 lambda -> bindings
; u: '*unassigned*
; v: '*unassigned*

; E2 let -> E1 lambda -> (set! u 1)
; u: 1
; v: '*unassigned*

; E2 let -> E1 lambda -> (set! v 2)
; u: 1
; v: 2

; E2 let -> E1 lambda -> (+ u v)
; u: 1
; v: 2

(define (scan-out-defines body)
  (append
    (map (lambda (var)
           (list 'define (definition-variable var) '*unassigned*))
         (filter definition? body))
    (map (lambda (exp)
           (if (definition? exp)
             (list 'set! 
                   (definition-variable exp)
                   (definition-value exp))
             exp))
         body)))
