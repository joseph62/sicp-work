(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-primary exp)
  (cadr exp))

(define (if-fail-alternative exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (let ((primary-procedure (analyze (if-fail-primary exp)))
        (alternative-procedure (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (primary-procedure env
                         succeed
                         (lambda ()
                           (alternative-procedure env
                                                  succeed
                                                  fail))))))
