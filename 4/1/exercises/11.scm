(define (make-frame variables values)
  (cons '*frame* (zip variables values)))

(define (frame-variables frame)
  (map car (cdr frame)))

(define (frame-values frame)
  (map cadr (cdr frame)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (list var val)
                        (cdr frame))))
