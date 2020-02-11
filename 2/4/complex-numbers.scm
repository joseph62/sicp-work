(define (square n)
  (* n n))

(define (make-from-real-imag real imag)
  (list (sqrt (+ (square real)
                 (square imag)))
        (atan imag real)))

(define (make-from-mag-ang mag ang)
  (list mag ang))

(define (real-part number)
  (* (magnitude number)
     (cos (angle number))))

(define (imag-part number)
  (* (magnitude number)
     (sin (angle number))))

(define (magnitude number)
  (car number))

(define (angle number)
  (cadr number))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
