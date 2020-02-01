; Unordered List Set

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set s1 s2)
  (filter (lambda (x) (element-of-set? x s2)) s1))

; 59)
(define (union-set s1 s2)
  (fold-right adjoin-set s2 s1))

; Ordered List Set

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
    ()
    (let ((x1 (car s1))
          (x2 (car s2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr s1)
                                     (cdr s2))))
            ((< x1 x2)
             (intersection-set (cdr s1) s2))
            ((> x1 x2)
             (intersection-set s1 (cdr s2)))))))

; 61)
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) 
                               (adjoin-set x (cdr set))))
        (else set)))

; 62)
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
          (let ((x1 (car s1))
                (x2 (car s2))
                (r1 (cdr s1))
                (r2 (cdr s2)))
            (cond ((= x1 x2) (cons x1 (union-set r1 r2)))
                  ((> x1 x2) (cons x2 (union-set s1 r2)))
                  ((< x1 x2) (cons x1 (union-set r1 s2))))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) 
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x () ()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
