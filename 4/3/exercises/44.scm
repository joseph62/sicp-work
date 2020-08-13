(define (make-position row col)
  (list row col))

(define (row pair)
  (car pair))

(define (col pair)
  (cadr pair))

(define (positions-slope p1 p2)
  (/ (abs (- (col p1) (col p2)))
     (abs (- (row p1) (row p2)))))

(define (unique-position? queen placed-queens)
  (and (unique-horiz? queen placed-queens)
       (unique-vert? queen placed-queens)
       (unique-diag? queen placed-queens)))

(define (unique-horiz? queen placed-queens)
  (if (member (row queen) (map row placed-queens))
    false
    true))

(define (unique-vert? queen placed-queens)
  (if (member (col queen) (map col placed-queens))
    false
    true))

(define (unique-diag? queen placed-queens)
  (if (member 1 (map (lambda (placed-queen)
                       (positions-slope queen placed-queen))
                     placed-queens))
    false
    true))

(define (queens board-size)
  (define (place-queen queens row)
    (if (= row board-size)
      queens
      (let ((queen 
              (make-position row
                             (an-integer-between 0 board-size))))
        (require (unique-position? queen queens))
        (place-queen (cons queen queens)
                     (+ row 1)))))
  (place-queen () 0))




