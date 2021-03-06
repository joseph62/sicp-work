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

