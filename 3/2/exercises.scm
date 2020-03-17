; 9)
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

; Global E:
; factorial
; fact-iter

(factorial 6)
; E1 -> Global E (factorial)
; n: 6

; E2 -> Global E (fact-iter)
; product: 1
; counter: 1
; max-count: 6

; E3 -> Global E (>)
; left: 1
; right: 6

; E4 -> Global E (*)
; left: 1
; right: 1

; E5 -> Global E (+)
; left: 1
; right: 1

; E6 -> Global E (fact-iter)
; product: 1
; counter: 2
; max-count: 6

; E7 -> Global E (>)
; left: 2
; right: 6

; E8 -> Global E (*)
; left: 2
; right: 1

; E9 -> Global E (+)
; left: 2
; right: 1

; E10 -> Global E (fact-iter)
; product: 2
; counter: 3
; max-count: 6

; E11 -> Global E (>)
; left: 3
; right: 6

; E12 -> Global E (*)
; left: 2
; right: 3

; E13 -> Global E (+)
; left: 3
; right: 1

; E14 -> Global E (fact-iter)
; product: 6
; counter: 4
; max-count: 6

; E15 -> Global E (>)
; left: 4
; right: 6

; E16 -> Global E (*)
; left: 6
; right: 4

; E17 -> Global E (+)
; left: 4
; right: 1

; E18 -> Global E (fact-iter)
; product: 24
; counter: 5
; max-count: 6

; E19 -> Global E (>)
; left: 5
; right: 6

; E20 -> Global E (*)
; left: 24
; right: 5

; E21 -> Global E (+)
; left: 5
; right: 1

; E22 -> Global E (fact-iter)
; product: 120
; counter: 6
; max-count: 6

; E23 -> Global E (>)
; left: 6
; right: 6

; 720

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; Global E
; factorial

(factorial 6)
; E1 -> Global E (factorial)
; n: 6

; E2 -> Global E (=)
; left: 6
; right 1

; E3 -> Global E (-)
; left: 6
; right: 1

; E4 -> Global E (factorial)
; n: 5

; E5 -> Global E (=)
; left: 5
; right 1

; E6 -> Global E (-)
; left: 5
; right: 1

; E7 -> Global E (factorial)
; n: 4

; E8 -> Global E (=)
; left: 4
; right 1

; E9 -> Global E (-)
; left: 4
; right: 1

; E10 -> Global E (factorial)
; n: 3

; E11 -> Global E (=)
; left: 3
; right 1

; E12 -> Global E (-)
; left: 3
; right: 1

; E13 -> Global E (factorial)
; n: 2

; E14 -> Global E (=)
; left: 2
; right 1

; E15 -> Global E (-)
; left: 2
; right: 1

; E16 -> Global E (factorial)
; n: 1

; E14 -> Global E (=)
; left: 1
; right 1

; E15 -> Global E (*)
; left: 2
; right: 1

; E16 -> Global E (*)
; left: 3
; right: 2

; E17 -> Global E (*)
; left: 4
; right: 6

; E18 -> Global E (*)
; left: 5
; right: 24

; E19 -> Global E (*)
; left: 6
; right: 120

; 720
