(load "../../3/queues.scm")

(define (acquire mutex)
  (mutex 'acquire))

(define (release mutex)
  (mutex 'release))

(define (make-mutexes n)
  (define (make-mutexes-iter q n)
    (if (= n 0)
        q
        (begin (insert-queue q (make-mutex))
               (make-mutexes-iter q (- n 1)))))
  (make-mutexes-iter (make-queue) n))

; a)
; In terms of mutexes
(define (make-semaphore maximum)
  (let ((count 0)
        (mutex (make-mutex)))
  (define (the-semaphore m)
    (cond ((eq? m 'acquire) 
           (mutex 'acquire)
           (if (< count maximum)
               (begin (set! count (+ count 1))
                      (mutex 'release))
               (begin (mutex 'release)
                      (the-semaphore m)))))
          ((eq? m 'release) 
           (mutex 'acquire)
           (set! count (- count 1))
           (mutex 'release))
          (else (error "Unknown operation -- SEMAPHORE" m)))
  the-semaphore))

; b)
(define (make-semaphore maximum)
  (let ((count (list 0))
        (mutex (make-mutex)))
  (define (the-semaphore m)
    (cond ((eq? m 'acquire) 
           (if (test-and-set! count maximum)
               (the-semaphore 'acquire)))
          ((eq? m 'release)
           (decrement! count))
          (else (error "Unknown operation -- SEMAPHORE" m))))
  the-semaphore))

(define (decrement! cell)
  (if (> (car cell) 0)
      (set-car! cell (- (car cell) 1))))

(define (test-and-set! cell maximum)
  (if (= (car cell) maximum)
      #t
      (begin (set-car! cell (+ (car cell) 1))
             #f)))
