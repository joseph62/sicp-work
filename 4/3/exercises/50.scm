(define (amb? exp) (or (tagged-list? exp 'amb)
                       (tagged-list? exp 'ramb)))

(define (random-amb? exp) (tagged-list? exp 'ramb))
(define (standard-amb? exp) (tagged-list? exp 'amb))


(define (random-to-front items)
  (let* ((index (random (length items)))
         (picked (list-ref items index))
         (rest (remove-list-ref items index)))
    (cons picked rest)))

(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1)
                         end))))

(define (remove-list-ref items remove-index)
  (map (lambda (index item)
         item)
       (filter (lambda (index item)
                 (not (= remove-index index)))
               (range 0 (length items))
               items)))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices) 
            (fail)
            (let ((choices (if (random-amb? exp)
                               (random-to-front choices)
                               choices)))
              ((car choices) env
                             succeed
                             (lambda ()
                               (try-next (cdr choices)))))))
      (try-next cprocs))))

; Using the ramb special form Alyssa's solution to sentence creation
; will be able to generate all sorts of sentences instead of getting
; stuck in particular cycles permanently.
