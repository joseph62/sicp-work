
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder a1 a2 s)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? s))
           (set-value! a2
                       (- (get-value s) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? s))
           (set-value! a1
                       (- (get-value s) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! s me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-a-value) (process-forget-value))
          (else
            (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect s me)
  me)

(define (multiplier m1 m2 p)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! p 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! p
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? p))
           (set-value! m2
                       (/ (get-value p) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? p))
           (set-value! m1
                       (/ (get-value p) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! p me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-a-value)
           (process-forget-value))
          (else
            (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect p me)
  me)

(define (constant value c)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect c me)
  (set-value! c value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- PROBE" request))))
  (connect connnector me)
  me)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints ()))
    (define (set-my-value! new-value setter)
      (cond ((not (has-value? me))
             (set! value new-value)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value new-value))
             (error "Contradiction" (list value new-value)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
            (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value!)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (map procedure
       (filter (lambda (item)
                 (not (eq? exception)))
               list))
  'done)

(define (has-value? c)
  (c 'has-value?))

(define (get-value c)
  (c 'value))

(define (set-value! c new-value informant)
  ((c 'set-value!) new-value informant))

(define (forget-value! c retractor)
  ((c 'forget) retractor))

(define (connect c constraint)
  ((c 'connect) constraint))

