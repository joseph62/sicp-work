(lambda ()
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a 1)
          (b 2))
      (set! u a)
      (set! v b))
    (+ u v)))


(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; In exercise description
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

; In text
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

; The exercise implementation would not work because the stream-map
; procedure would try applying f to y when y is set to 'unassigned
; which would likely cause an error.
; The in text implementation would not run in to this issue as y
; is set to an expected value by the time it is used in the dy set.
