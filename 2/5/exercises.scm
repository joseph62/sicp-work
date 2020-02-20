; 75)
(define (make-from-mag-ang r a)
  (lambda (op)
    (cond ((eq? op 'real-part) 
           (* r (cos a)))
          ((eq? op 'imag-part) 
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unkown op -- MAKE-FROM-REAL-IMAG" op)))))

; 76)
; New Type:
;   Explicit Dispatch: Each operation must be updated to account for the new type
;   Data Directed: Each type must define the way it is to be used by the system
;   Message Passing: Each type must define the way it is to be used by the system
; New Operation:
;   Explicit Dispatch: The new operation must be made and account for the existing types
;   Data Directed: Each type must implement the new operation
;   Message Passing: Each type must implement the new operation 
; 
; Explicit Dispatch seems to be the best for systems that 
; get new operations more than new types.
; Data Directed and Message Passing are better for systems that
; get new types more than new operations. Of the two, Data directed
; is more useful for systems that require arbitrary argument lists for procedures.

; 77)
; (magnitude z)
; (apply-generic 'magnitude z)
; ((get 'magnitude '(complex)) z)
; (magnitude rect)
; (apply-generic 'magnitude rect)
; ((get 'magnitude '(rectangular)) rect)
; (magnitude ('real 'imag))
; (sqrt (+ (square 'real)) (+ (square 'imag)))

; 78)
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (install-scheme-number-package)
  (define (tag x) x)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) x))
  'done)

(define (install-arthmetic-package)
  ; 79)
  (define (real-part r)
    ((get 'real-part '(complex)) r))
  (define (imag-part r)
    ((get 'imag-part '(complex)) r))

  (put 'equ? '(complex complex)
       (lambda (r1 r2)
         (and (= (real-part r1) (real-part r2))
              (= (imag-part r1) (imag-part r2)))))
  (define (numer r)
    ((get 'numer '(rational)) r))
  (define (denom r)
    ((get 'denom '(rational)) r))
  (put 'equ? '(rational rational)
       (lambda (r1 r2)
         (and (= (numer r1) (numer r2))
              (= (denom r1) (denom r2)))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ; 80)
  (put '=zero? '(complex)
       (lambda (r)
         (and (= (real-part r) 0)
              (= (imag-part r) 0))))
  (put '=zero? '(rational)
       (lambda (r)
         (and (= (numer r) 0)
              (= (denom r) 0))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done)

; 81)
; a) Calling the exp procedure on complex numbers would result
; in an infinite recursion. If the procedure is defined, the procedure
; would be executed properly.
; b) There is not a need to try a same type coercion in apply-generic.
; c) 
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
      (apply proc (map contents args))
      (if (= (length args) 2)
        (let* ((type1 (car type-tags))
               (type2 (cadr type-tags))
               (a1 (car args))
               (a2 (cadr args))
               (same-type (eq? type1 type2))
               (t1->t2 (get-coercion type1 type2))
               (t2->t1 (get-coercion type2 type1)))
          (cond ((and t1->t2 (not same-type))
                  (apply-generic op (t1->t2 a1) a2))
                ((and t2->t1 (not same-type))
                  (apply-generic op a1 (t2->t1 a2)))
                (else
                  (error "No method for these types"
                         (list op type-tags)))))
        (error "No method for these types"
               (list op type-tags))))))
