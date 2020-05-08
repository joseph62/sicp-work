; Peter
(set! balance (+ balance 10))
; Paul
(set! balance (- balance 20))
; Mary
(set! balance (/ balance 2.0))

; a)
; (/ (+ (- 100 20) 10) 2) -> 45
; (/ (- (+ 100 10) 20) 2) -> 45
; (+ (/ (- 100 20) 2) 10) -> 50
; (+ (- (/ 100 2) 20) 10) -> 40
; (- (+ (/ 100 2) 10) 20) -> 40
; (- (/ (+ 100 10) 2) 20) -> 35

; b)
; new-value: (- 100 20) Paul
; new-value: (+ 100 10) Peter
; set balance 80 Paul
; set balance 110 Peter
; new-value (/ 110 2) Mary
; set balance 55 Mary
; result 55

; new-value: (/ 100 2) Mary
; new-value: (- 100 20) Paul
; new-value: (+ 100 10) Peter
; set balance 80 Paul
; set balance 50 Mary
; set balance 110 Peter
; result 110

