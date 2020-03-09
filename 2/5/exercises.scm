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

; 87)
(define (install-poly-generic-arithmetic)
  (define (=zero? poly)
    (empty-termlist? (term-list poly))
  (put '=zero? '(polynomial) =zero?)

; 88)
  (define (negate n)
    (sub 0 n))

  (define (sub-poly p1 p2)
    (if (same-variable? p1 p2)
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))

  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       (negate t1) (sub-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       (negate t2) (sub-term L1 (rest-terms L2))))
                    (else 
                      (adjoin-term
                        (make-term (order t1)
                                   (sub (coeff t1) (coeff t2))))
                      (sub-terms (rest-terms L1)
                                 (rest-terms L2))))))))

  'done)

; 89)
  (define (the-empty-termlist) ())
  (define (adjoin-term term term-list)
    (cond ((= (order term) (next-highest-order term-list))
           (cons (coeff term) term-list))
          ((< (order term) (next-highest-order term-list)) term-list)
          ((> (order term) (next-highest-order term-list))
           (adjoin-term term (cons 0 term-list)))))
  (define (next-highest-order term-list)
    (length term-list))
  (define (highest-order term-list)
    (- (length term-list) 1))
  (define (empty-termlist? term-list)
    null?)
  (define (first-term term-list)
    (let ((first-coeff (car term-list)))
      (if (=zero? first-coeff)
        (first-term (rest-terms term-list))
        (make-term (highest-order term-list)
                   (car term-list)))))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (make-term order coefficient)
    (list order coefficient))
  (define (order term)
    (car term))
  (define (coefficient term)
    (cadr term))

; 90)

(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term term term-list))

(define (install-sparse-termlist) 
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (adjoin-term term term-list)
    (cons term term-list))
  (define (the-empty-termlist) ())

  (define (tag datum) (attach-tag 'sparse-termlist datum))

  (put 'first-term '(sparse-termlist) first-term)
  (put 'rest-terms '(sparse-termlist)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'adjoin-term '(term sparse-termlist)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist 'sparse-termlist 
       (lambda () (tag (the-empty-termlist))))
  'done)

(define (install-dense-termlist)
  (define (the-empty-termlist) ())
  (define (adjoin-term term term-list)
    (cond ((= (order term) (next-highest-order term-list))
           (cons (coeff term) term-list))
          ((< (order term) (next-highest-order term-list)) term-list)
          ((> (order term) (next-highest-order term-list))
           (adjoin-term term (cons 0 term-list)))))
  (define (next-highest-order term-list)
    (length term-list))
  (define (highest-order term-list)
    (- (length term-list) 1))
  (define (empty-termlist? term-list)
    null?)
  (define (first-term term-list)
    (let ((first-coeff (car term-list)))
      (if (=zero? first-coeff)
        (first-term (rest-terms term-list))
        (make-term (highest-order term-list)
                   (car term-list)))))
  (define (rest-terms term-list)
    (cdr term-list))

  (define (tag datum) (attach-tag 'dense-termlist datum))

  (put 'the-empty-termlist 'dense-termlist 
       (lambda () (tag (the-empty-termlist))))
  (put 'first-term '(dense-termlist)
       (lambda (term-list) (first-term term-list)))
  (put 'rest-terms '(dense-termlist)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'adjoin-term '(term dense-termlist)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  'done)

  'done)

; 91)
(define (div-poly p1 p2)
  (if (same-variable? p1 p2)
    (div-terms (term-list p1)
               (term-list p2)))
    (error "Polys not in same var -- ADD-POLY"
           (list p1 p2)))

(define (div-terms l1 l2)
  (if (empty-termlist? l1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term l1))
          (t2 (first-term l2)))
      (if (> (order t2) (order t1))
        (list (the-empty-termlist) l1)
        (let ((new-c (div (coeff t1) (coeff t2)))
              (new-o (- (order t1) (order t2))))
          (let ((new-term (make-term new-c new-o))
                (rest-of-result
                  (div-terms (sub l1 new-term)
                             (mul-term-by-all-terms new-term l2))))
            (map (lambda (term-list)
                   (adjoin-term new-term term-list))
                 rest-of-result)))))))

; 92) Yikes, pass for now.

; 93)
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  'done)

; 94)
(define (gcd-terms l1 l2)
  (if (empty-termlist? l2)
    l1
    (gcd-terms l2 (remainder-terms l1 l2))))

(define (remainder-terms l1 l2)
  (cadr (div-terms l1 l2)))


(define (sub-poly p1 p2)
  (if (same-variable? p1 p2)
    (make-poly (variable p1)
               (gcd-terms (term-list p1)
                          (term-list p2)))
    (error "Polys not in same var -- SUB-POLY"
           (list p1 p2))))

; 95) ...
; 96)
(define (gcd-terms l1 l2)
  (if (empty-termlist? l2)
    l1
    (gcd-terms l2 (pseudoremainder-terms l1 l2))))

(define (pseudoremainder-terms l1 l2)
  (let ((o1 (order (next-term l2))) 
        (o2 (order (next-term l1))) 
        (c  (coeff (next-term l1))))
    (cadr (div-terms l1
                     (mul (pow c (add 1 (sub o1 o2)))
                          l2)))))

(define (gcd-terms l1 l2)
  (define (iter l1 l2)
    (if (empty-termlist? l2)
      l1
      (gcd-terms l2 (remainder-terms l1 l2))))
  (let ((result (iter l1 l2)))
    (div result
         (apply gcd (map (coeff result))))))

; 97) ...
