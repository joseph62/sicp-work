; Factorial

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

; Datapath diagram:
; Registers: n, product, counter, p, c
; Constants: 1, 1, 1
; Decision Points: >
; Operators: *, +
; Buttons: 
;   1->product
;   1->counter
;   p->product
;   c->counter
;   *->p
;   +->c
; Paths:
;   counter->*
;   counter->>
;   product->*
;   1->+
;   counter->+

; Control diagram
; 1) start
; 2) 1->product
; 3) 1->counter
; 4) counter>n
; 5) no? jump to 7
; 6) yes? done -> product
; 7) *->p
; 8) +->c
; 9) p->product
; 10) c->counter
; 11) jump to 4
