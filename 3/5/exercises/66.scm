(define (enumerate start stream)
  (cons-stream (list start (stream-car stream))
               (enumerate (+ start 1)
                          (stream-cdr stream))))

(define car-1 
  (stream-filter
    (lambda (enumerated-pair)
      (= 1 (caadr enumerated-pair)))
    (enumerate 1 int-pairs)))

; every pair after the first (index 1) appears every other iteration of the int-pairs stream 2^1
; (1 100) appears at the 198th position of the stream

(define car-2 
  (stream-filter
    (lambda (enumerated-pair)
      (= 2 (caadr enumerated-pair)))
    (enumerate 1 int-pairs)))
; every pair after the first (index 3) appears every 4th iteration of int-pairs 2^2

(define car-3 
  (stream-filter
    (lambda (enumerated-pair)
      (= 3 (caadr enumerated-pair)))
    (enumerate 1 int-pairs)))
; every pair after the first (index 7) appears every 8th iteration of int-pairs 2^3

(define car-4
  (stream-filter
    (lambda (enumerated-pair)
      (= 4 (caadr enumerated-pair)))
    (enumerate 1 int-pairs)))
; every pair after the first (index 15) appears every 16th iteration of int-pairs 2^4


(define car-5
  (stream-filter
    (lambda (enumerated-pair)
      (= 5 (caadr enumerated-pair)))
    (enumerate 1 int-pairs)))
; every pair after the first (index 31) appears every 32nd iteration of int-pairs 2^5

; every pair with a car of 99 would start at the index of (index of 98 start + 2^98)
; and a pair would show up every 2^99 iterations of the int-pairs stream after that.

; every pair with a car of 100 would start at the index of (index of 99 start + 2^99)
; and a pair would show up every 2^100 iteration of the int-pairs stream after that.

(define (display-partial-stream n stream)
  (if (> n 0)
      (begin
        (display (stream-car stream))
        (newline)
        (display-partial-stream (- n 1)
                                (stream-cdr stream)))))

