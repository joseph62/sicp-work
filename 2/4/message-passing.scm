(define (make-from-real-imag x y)
  (lambda (op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "Unkown op -- MAKE-FROM-REAL-IMAG" op)))))

(define (real-part complex)
  (complex 'real-part))

(define (imag-part complex)
  (complex 'imag-part))

(define (magnitude complex)
  (complex 'magnitude))

(define (angle complex)
  (complex 'angle))

(define (apply-generic op arg) (arg op))
