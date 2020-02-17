(define (attach-tag type-tag content)
  (cons type-tag content))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents number)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (square n)
  (* n n))

(define (make-from-real-imag-rectangular real imag)
  (attach-tag 'rectangular (list real imag)))

(define (make-from-real-imag real imag)
  (make-from-real-imag-rectangular real imag))

(define (make-from-mag-ang-polar mag ang)
  (attach-tag 'polar (list mag ang)))

(define (make-from-mag-ang mag ang)
  (make-from-mag-ang-polar mag ang))

(define (real-part-rectangular number)
  (car number))

(define (real-part-polar number)
  (* (magnitude-polar number)
     (cos (angle-polar number))))

(define (real-part number)
  (cond ((rectangular? number) 
         (real-part-rectangular (contents number)))
        ((polar? number)
         (real-part-polar (contents number)))
        (else (error "Unkown type -- REAL-PART" number))))

(define (imag-part-rectangular number)
  (cadr number))

(define (imag-part-polar number)
  (* (magnitude-polar number)
     (sin (angle-polar number))))

(define (imag-part number)
  (cond ((rectangular? number)
         (imag-part-rectangular (contents number)))
        ((polar? number)
         (imag-part-polar (contents number)))
        (else (error "Unknown type -- IMAG-PART"))))
(define (magnitude-polar number)
  (car number))

(define (magnitude-rectangular number)
  (sqrt (+ (square (real-part-rectangular number))
           (square (imag-part-rectangular number)))))

(define (magnitude number)
  (cond ((rectangular? number)
         (magnitude-rectangular (contents number)))
        ((polar? number)
         (magnitude-polar (contents number)))
        (else (error "Unknown type -- MAGNITUDE"))))

(define (angle-polar number)
  (cadr number))

(define (angle-rectangular number)
  (atan (imag-part-rectangular number)
        (real-part-rectangular number)))

(define (angle number)
  (cond ((rectangular? number)
         (angle-rectangular (contents number)))
        ((polar? number)
         (angle-polar (contents number)))
        (else (error "Unknown type -- ANGLE"))))



(define (imag-part number)
  (if (rectangular? (contents number))
    (caddr (contents number))
    (* (magnitude (contents number))
       (sin (angle (contents number))))))

(define (magnitude number)
  (if (polar? (contents number))
    (cadr (contents number))
    (sqrt (+ (square (real-part (contents number)))
             (square (imag-part (contents number)))))))

(define (angle number)
  (if (polar? (contents number))
    (caddr (contents number))
    (atan (imag-part (contents number))
          (real-part (contents number)))))

(define (rectangular? number)
  (equal? (type-tag number) 'rectangular))

(define (polar? number)
  (equal? (type-tag number) 'polar))

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
