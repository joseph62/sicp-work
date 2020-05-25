(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x)
                    (list (stream-car s) (stream-car t) x))
                  (stream-cdr u))
      (interleave
        (stream-map (lambda (x y)
                      (list (stream-car s) x y))
                    (stream-cdr t)
                    (stream-cdr u))
        (triples (stream-cdr s)
                 (stream-cdr t)
                 (stream-cdr u))))))

(define int-triples (triples integers integers integers))

(define (square x) (* x x))

(define (pythagorean-triples)
  (stream-filter (lambda (ijk) (= (square (caddr ijk))
                                  (+ (square (car ijk))
                                     (square (cadr ijk)))))
                 int-triples))

