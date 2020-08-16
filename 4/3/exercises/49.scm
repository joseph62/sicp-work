(define (parse-word words)
  (list (car words) (apply amb (cdr words))))
