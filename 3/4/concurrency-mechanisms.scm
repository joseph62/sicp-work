(define (make-serializer)
  (let ((locked #f))
    (lambda (procedure)
      (define (call-when-not-locked)
        (if (locked)
            (call-when-not-locked)
            (begin (set! locked #t)
                   (procedure)
                   (set! locked #f))))
      call-when-not-locked)))
                 
                 
(define (parallel-execute processes)
  (map apply processes))

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

