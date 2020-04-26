; 17)

(define (last-pair items)
  (cond ((null? items) ()) 
         ((null? (cdr items)) (car items))
         (else (last-pair (cdr items)))))

; 18)

(define (reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
  (iter items ()))

; 19)

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (no-more? denominations) 
  (null? denominations))

(define (first-denomination denominations)
  (car denominations))

(define (except-first-denomination denominations)
  (cdr denominations))

(define (cc amount denominations)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? denominations)) 0)
        (else (let ((denomination (first-denominations denominations))
                    (other-denominations (except-first-denomination denominations)))
                (+ (cc amount 
                       (except-first-denomination denominations))
                   (cc (- amount 
                          (first-denomination denominations))
                       denominations))))))

; 20)

(define (same-parity first . rest)
  (let ((parity (if (even? first) even? odd?)))
    (define (iter numbers)
      (cond ((null? numbers) ())
            ((parity (car numbers)) (cons (car numbers) 
                                          (iter (cdr numbers))))
            (else (iter (cdr numbers)))))
    (cons first (iter rest))))

; 21)

(define (square-list numbers)
  (if (null? numbers) 
    () 
    (cons (square (car numbers)) 
          (square-list (cdr numbers)))))

(define (square-list numbers)
  (map square numbers))

; 22)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) 
            (cons (square (car things))
                  answer))))
  (iter items ()))

; (square-list (list 1 2 3))
; (iter (1 2 3) ())
; (iter (2 3) (cons (square 1) ()))
; (iter (3) (cons 2 (1)))
; (iter () (cons 3 (2 1)))
; (3 2 1)
; As this procedure processes items it adds them to the start of the list.
; This results in a reversed list in the end.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items ()))

; (square-list (list 1 2 3))
; (iter (2 3) (cons () 1))
; (iter (3) (cons (() . 1) 2))
; (iter () (cons ((() . 1) . 2) 3))
; (((() . 1) . 2) . 3)
; This solution creates a pair of the previous iteration and the next iteration.

; Correct solution would be:
; (1 . ())
; (1 . (2 . ()))
; (1 . (2 . (3 . ())))
; Must be able to add a pair on to the end of the list

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer (list (car things))))))
  (iter items ()))

; OR

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (reverse (iter items ())))

; 23)

(define (for-each f items)
  (if (null? items)
    #t
    (or (and (f (car items)) #f)
        (for-each f (cdr items)))))


; 24)

(list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))
; |1| | -> | |nil| 
;           \---> |2| | -> | |nil|
;                            \---> |3| | -> |4|nil|

; 25)
(define a (list 1 3 (list 5 7) 9))
(= 7 (cadr (caddr a)))
(define b (list (list 7)))
(= 7 (caar b))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(= 7 (cadr (cadr (cadr (cadr (cadr (cadr c)))))))

; 26)
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)
(cons x y)
; ((1 2 3) . (4 5 6))
(list x y)
;((1 2 3) (4 5 6))

; 27)

(define (deep-reverse items)
  (cond ((null? items)  ())
        ((list? items) (map deep-reverse (reverse items)))
        ((pair? items) (cons (deep-reverse (car items)) 
                             (deep-reverse (cdr items))))
        (else items)))


; 28)
(define (fringe tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) 
                      (fringe (cdr tree))))))

; 29)

(define (make-mobile left right)
  (list left right))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (mobile? m)
  (and (pair? m)
       (branch? (left-branch m))
       (branch? (right-branch m))))

(define (make-branch length structure)
  (list length structure))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

(define (simple-branch? b)
  (and (pair? b)
       (number? (branch-length b))
       (number? (branch-structure b))))

(define (mobile-branch? b)
  (and (pair? b)
       (number? (branch-length b))
       (mobile? (branch-structure b))))


(define (branch? b) 
  (or (simple-branch? b)
      (mobile-branch? b)))

(define (total-weight m)
  (cond ((null? m) 0)
        ((number? m) m)
        ((simple-branch? m) (branch-structure m))
        ((mobile-branch? m) (total-weight (branch-structure m)))
        ((mobile? m) (+ (total-weight (left-branch m))
                        (total-weight (right-branch m))))))

    
(define (branch-torque b)
  (* (branch-length b) (total-weight b)))

(define (balance? m)
  (= (branch-torque (left-branch m)) 
     (branch-torque (right-branch m))))


(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch m)
  (cdr m))

(define (branch-structure b)
  (cdr b))

(define mobile (make-mobile (make-branch 1 20)
                            (make-branch 2 
                                         (make-mobile (make-branch 3 30) 
                                                      (make-branch 4 40)))))

(define balanced-mobile (make-mobile (make-branch 1 mobile)
                                     (make-branch 2 45)))
; 30)

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; 31)

(define (map-tree f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree)
  (map-tree square tree))

; 32)

(define (subsets s)
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) 
                          (cons (car s) x))
                        rest)))))

; 33)

(load "sequences-as-interfaces.scm")

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              () 
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1))
              0
              sequence))

; 34)

(define (horner-eval x coefficients)
  (accumulate (lambda (coefficient higher-terms) (+ (* x higher-terms)
                                                    coefficient))
              0
              coefficients))

; 35)

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (+ (count-leaves (list (car x)))
                            (count-leaves (cdr x)))
                         1))
                   t)))


; 36)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 37)

(define (sum sequence)
  (accumulate + 0 sequence))

(define (dot-product v w)
  (sum (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (r) (matrix-*-vector n-cols r)) m)))

; 38)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 '(1 2 3))
; 3/2

(fold-left / 1 '(1 2 3))
; 1/6

(fold-right list () '(1 2 3))
; (1 (2 (3 ())))

(fold-left list () '(1 2 3))
; (((() 1) 2) 3)

; fold-right op init sequence == fold-left op init sequence 
; if and only if op satisfies the constraint that
; (= (op a b) (op b a)) for all a and b
; examples: +, *

; 39)

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence)) 

; 40)


(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 i))) 
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; 41)

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (jk) 
                    (cons i jk))
                  (unique-pairs i)))
           (enumerate-interval 1 n)))

(define (triples-less-than-n-sum-to-s n s)
  (filter (lambda (ijk)
            (= (sum ijk) s))
          (unique-triples n)))

; 42)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (make-position row column)
  (cons row column))

(define (row position)
  (car position))

(define (column position)
  (cdr position))

(define empty-board ())

(define (first-true predicate? items)
  (cond ((null? items) ())
        ((predicate? (car items)) (car items))
        (else (first-true predicate? (cdr items)))))
 
(define (safe? k positions)
  (define (safe-column? check-column positions)
    (null? (filter (lambda (p) 
                     (= (column p) check-column))
                   rest))) 
  (define (safe-row? check-row positions)
    (null? (filter (lambda (p)
                     (= (row p) check-row))
                   rest)))
  (define (safe-diagonal? position others)
    (null? (filter (lambda (p)
                     (= (abs (- (row p) 
                                (row position)))
                        (abs (- (column p)
                                (column position)))))
                   others)))

  (let ((position (first-true (lambda (p) (= (column p) k)) 
                              positions))
        (rest (filter (lambda (p) (not (= (column p) k)))
                      positions)))
    (and (safe-column? (column position) rest)
         (safe-row? (row position) rest)
         (safe-diagonal? position rest))))
         


(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))


; (queens 4)

; 3,4 1,3 4,2 2,1
; xxqx
; qxxx
; xxxq
; xqxx

; 2,4 4,3 1,2 3,1
; xqxx
; xxxq
; qxxx
; xxqx

; 43)

;(flatmap 
;  (lambda (new-row)
;    (map (lambda (rest-of-queens)
;           (adjoin-position new-row k rest-of-queens))
;         (queen-cols (- k 1))))
;  (enumerate-interval 1 board-size))
; This version results in a recursive call to queen-cols for every new row
; position where the original only calls queen-cols once for every recursion
; level. This would take T^n time

; 44)

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

; 45)

(define (split outer-direction inner-direction)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (outer-direction painter (inner-direction smaller smaller)))))
  (lambda (painter n) (splitter painter n)))

; 46)
; See a-picture-language.scm

; 47)
; For list implementation see a-picture-language.scm
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

; 48)
; See a-picture-language.scm

; 49)
(define (painter-a frame)
  (let ((origin (make-vect 0.0 0.0))
        (edge1.0 (make-vect 1.0 0.0))
        (edge2 (make-vect 0.0 1.0))
        (far (make-vect 1.0 1.0)))
    ((segments->painter (list (make-segment origin edge1.0)
                              (make-segment origin edge2)
                              (make-segment edge1.0 far)
                              (make-segment edge2 far)))
     frame)))

(define (painter-b frame)
  (let ((origin (make-vect 0.0 0.0))
        (edge1.0 (make-vect 1.0 0.0))
        (edge2 (make-vect 0.0 1.0))
        (far (make-vect 1.0 1.0)))
    ((segments->painter (list (make-segment origin far)
                              (make-segment edge1 edge2)))
     frame)))


(define (painter-c frame)
  (let ((midpoint-o-e1 (make-vect 0.5 0.0))
        (midpoint-o-e2 (make-vect 0.0 0.5))
        (midpoint-e1-f (make-vect 0.5 1.0))
        (midpoint-e2-f (make-vect 1.0 0.5)))
    ((segments->painter (list (make-segment midpoint-o-e1 midpoint-o-e2)
                              (make-segment midpoint-o-e1 midpoint-e2-f)
                              (make-segment midpoint-o-e2 midpoint-e1-f)
                              (make-segment midpoint-e1-f midpoint-e2-f)))
     frame))))

(define (painter-d frame)
  (let ((upper-right-hand (make-vect 0.0 0.9))
        (lower-right-hand (make-vect 0.0 0.75))
        (upper-right-elbow (make-vect 0.2 0.7))
        (lower-right-elbow (make-vect 0.2 0.5))
        (right-shoulder (make-vect 0.4 0.75))
        (right-armpit (make-vect 0.4 0.675))
        (right-neck (make-vect 0.45 0.75))
        (mid-right-head (make-vect 0.425 0.9))
        (top-right-head (make-vect 0.45 1.0))
        (right-side (make-vect 0.4 0.575))
        (right-outer-foot (make-vect 0.35 0.0))
        (right-inner-foot (make-vect 0.45 0.0))
        (crotch (make-vect 0.5 0.375))
        (left-inner-foot (make-vect 0.55 0.0))
        (left-outer-foot (make-vect 0.65 0.0))
        (left-armpit (make-vect 0.575 0.5))
        (left-lower-hand (make-vect 1.0 0.2))
        (left-upper-hand (make-vect 1.0 0.35))
        (left-shoulder (make-vect 0.75 0.75))
        (left-nect (make-vect 0.65 0.75))
        (mid-left-head (make-vect 0.7 0.9))
        (top-left-head (make-vect 0.65 1.0)))
    ((segments->painter (list (make-segment upper-right-hand upper-right-elbow)
                              (make-segment upper-right-elbow right-shoulder)
                              (make-segment right-shoulder right-neck)
                              (make-segment right-neck mid-right-head)
                              (make-segment mid-right-head top-right-head)
                              (make-segment top-left-head mid-left-head)
                              (make-segment mid-left-head left-neck)
                              (make-segment left-neck left-shoulder)
                              (make-segment left-shoulder upper-left-hand)
                              (make-segment lower-left-hand left-armpit)
                              (make-segment left-armpit left-outer-foot)
                              (make-segment left-inner-foot crotch)
                              (make-segment crotch right-inner-foot)
                              (make-segment right-outer-foot right-side)
                              (make-segment right-side right-armpit)
                              (make-segment right-armpit lower-right-elbow)
                              (make-segment lower-right-elbow lower-right-hand)))
     frame)))

(define wave painter-d)

; 50)

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; 51)

(define (below p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
            (transform-painter p1
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0)))
          (paint-bottom
            (transform-painter p2
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below p1 p2)
  (rotate270 (beside (rotate90 p1)
                     (rotate90 p2))))
; 52)


(define (painter-d frame)
  (let ((upper-right-hand (make-vect 0.0 0.9))
        (lower-right-hand (make-vect 0.0 0.75))
        (upper-right-elbow (make-vect 0.2 0.7))
        (lower-right-elbow (make-vect 0.2 0.5))
        (right-shoulder (make-vect 0.4 0.75))
        (right-armpit (make-vect 0.4 0.675))
        (right-neck (make-vect 0.45 0.75))
        (mid-right-head (make-vect 0.425 0.9))
        (top-right-head (make-vect 0.45 1.0))
        (right-side (make-vect 0.4 0.575))
        (right-outer-foot (make-vect 0.35 0.0))
        (right-inner-foot (make-vect 0.45 0.0))
        (crotch (make-vect 0.5 0.375))
        (left-inner-foot (make-vect 0.55 0.0))
        (left-outer-foot (make-vect 0.65 0.0))
        (left-armpit (make-vect 0.575 0.5))
        (left-lower-hand (make-vect 1.0 0.2))
        (left-upper-hand (make-vect 1.0 0.35))
        (left-shoulder (make-vect 0.75 0.75))
        (left-nect (make-vect 0.65 0.75))
        (mid-left-head (make-vect 0.7 0.9))
        (top-left-head (make-vect 0.65 1.0))
        (top-star (make-vect 0.5 0.575))
        (top-right-star (make-vect 0.45 0.525))
        (bottom-right-star (make-vect 0.475 0.475))
        (bottom-left-star (make-vect 0.525 0.475))
        (top-left-star (make-vect 0.55 0.525)))

    ((segments->painter (list (make-segment upper-right-hand upper-right-elbow)
                              (make-segment upper-right-elbow right-shoulder)
                              (make-segment right-shoulder right-neck)
                              (make-segment right-neck mid-right-head)
                              (make-segment mid-right-head top-right-head)
                              (make-segment top-left-head mid-left-head)
                              (make-segment mid-left-head left-neck)
                              (make-segment left-neck left-shoulder)
                              (make-segment left-shoulder upper-left-hand)
                              (make-segment lower-left-hand left-armpit)
                              (make-segment left-armpit left-outer-foot)
                              (make-segment left-inner-foot crotch)
                              (make-segment crotch right-inner-foot)
                              (make-segment right-outer-foot right-side)
                              (make-segment right-side right-armpit)
                              (make-segment right-armpit lower-right-elbow)
                              (make-segment lower-right-elbow lower-right-hand)
                              (make-segment top-star bottom-right-star)
                              (make-segment bottom-right-star top-left-star)
                              (make-segment top-left-star top-right-star)
                              (make-segment top-right-star bottom-left-star)
                              (make-segment bottom-left-star top-star)))
     frame)))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up (flip-horiz up)))
            (bottom-right (below right (flip-vert right)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))


(define (square-limit painter n)
  (let ((combine4 (square-of-four identity rotate90
                                  rotate180 rotate270)))
    (combine4 (corner-split painter n))))
