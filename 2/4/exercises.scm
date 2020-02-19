; 73)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; a)
; number? and same-variable? predicates are used for the three
; deriv base cases which must be present no matter how many
; deriv implementations are available.

; b)
(define (install-addition-deriv)
  (define (make-sum x y)
    (list '+ x y))
  (define (addend exp)
    (car exp))
  (define (augend exp)
    (cadr exp))
  (define (deriv-addition exp var)
    (make-sum (derive (addend exp) var)
              (derive (augend exp) var)))
  (put 'deriv '+ deriv-addition)
  'done)

(define (install-multiplication-deriv)
  (define (make-product x y)
    (list '* x y))
  (define (multiplier exp)
    (car exp))
  (define (multiplicand exp)
    (cadr exp))
  (define (deriv-multiplication exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  (put 'deriv '* deriv-multiplication)
  'done)

; c)
(define (install-exponent-deriv)
  (define (make-exponent base pow)
    (list '** base pow))
  (define (base exp)
    (car exp))
  (define (pow exp)
    (cadr exp))
  (define (deriv-exponent exp var)
    (make-product (power e)
                  (make-product (make-exponent (base e)
                                               (make-difference (power e) 1))
                                (derive (base e) var))))
  (put 'deriv '** deriv-exponent)
  'done)

; d)
; if the get function operands switch to <op> 'deriv the only change
; needed to to be made is the put

; 74)
; a)
(define (get-record division employee-name)
  ((get 'get-record division) employee-name))
; The division must implement a get-record procedure that accepts an
; employee name and returns the record for that employee

; b)
(define (get-salary division employee-name)
  ((get 'get-salary division) (get-record division employee-name)))
; The division must implement a version of get-salary and get-record

; c)
(define (find-employee-record divisions employee-name)
  (define (find-employee-record-iter divisions)
    (if (null? divisions)
      ()
      (let ((record (get-record (car divisions) employee-name)))
        (if record
          record
          (find-employee-record-iter (cdr divisions))))))
  (find-employee-record-iter divisions))

; d)
; When a new division is added to the company, the only change that needs to be
; made is to implement the get-record and get-salary procedures for that division

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
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
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

