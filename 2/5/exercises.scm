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

; 82)
; apply-generic would need to check all combinations of types
; to ensure that super type and mixed type operations would be found.

; 83)
(put 'raise 
     'scheme-number
     (lambda (n) (make-rational n n)))

(put 'raise 
     'rational
     (lambda (n) (make-real (/ (numer n)
                               (denom n)))))

(put 'raise
     'real
     (lambda (n) (make-complex (real n) 0)))

; 84)

(define (is-super? start potential-super)
  (or (eq? (type-tag start)
           (type-tag potential-super))
      (let ((raise (get 'raise (type-tag start))))
        (if raise
          (is-super? (raise (content start)) potential-super)
          #f))))

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
               (t1-super? (is-super? a2 a1))
               (t2-super? (is-super? a1 a2))
               (same-type (eq? type1 type2))
               (raise-t1 (get 'raise type1))
               (raise-t2 (get 'raise type2)))
          (cond ((and t2-super? t1->t2 (not same-type))
                  (apply-generic op (raise-t1 a1) a2))
                ((and t1-super? t2->t1 (not same-type))
                  (apply-generic op a1 (raise-t2 a2)))
                (else
                  (error "No method for these types"
                         (list op type-tags)))))
        (error "No method for these types"
               (list op type-tags))))))

; 85)

(put 'project 
     'complex
     (lambda (c) 
       (make-real (real-part c))))

(put 'project
     'real
     (lambda (r) 
       (let ((n (round r)))
         (make-rational n n))))

(put 'project
     'rational
     (lambda (r)
       (make-scheme-number (numer r))))

(define (project data)
  ((get 'project (type-tag data)) data))

(define (projectable? data)
  (get 'project (type-tag data)))

(define (drop data)
  (if (projectable? data)
    (let* ((projected (project data))
          (reraised (raise projected)))
      (if (equ? reraised data)
        (drop projected)
        data))
    data))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
      (drop (apply proc (map contents args)))
      (if (= (length args) 2)
        (let* ((type1 (car type-tags))
               (type2 (cadr type-tags))
               (a1 (car args))
               (a2 (cadr args))
               (t1-super? (is-super? a2 a1))
               (t2-super? (is-super? a1 a2))
               (same-type (eq? type1 type2))
               (raise-t1 (get 'raise type1))
               (raise-t2 (get 'raise type2)))
          (cond ((and t2-super? t1->t2 (not same-type))
                  (apply-generic op (raise-t1 a1) a2))
                ((and t1-super? t2->t1 (not same-type))
                  (apply-generic op a1 (raise-t2 a2)))
                (else
                  (error "No method for these types"
                         (list op type-tags)))))
        (error "No method for these types"
               (list op type-tags))))))

; 86)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-image x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'mangitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (real-part z) 
    (* (magnitude z) (cosine (angle z))))
  (define (imag-part z) 
    (* (magnitude z) (sine (angle z))))
  (define (make-from-real-image x y) 
    (cons (sqrt (+ (square x) (square x)))
          (atan y x)))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'mangitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
                         (apply-generic 'add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
                         (apply-generic 'sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
                       (apply-generic 'add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (apply-genric 'div (magnitude z1) (magnitude z2))
                       (apply-genric 'sub (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag (make-from-mag-ang r a))))
  'done)
