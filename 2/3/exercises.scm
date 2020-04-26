; 53)

(list 'a 'b 'c)
; (a b c)

(list (list 'george))
; ((george))

(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; (y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; ()

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; 54)
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((not (eq? (car a) (car b))) #f)
        (else (equal? (cdr a) (cdr b)))))

; 55)
; 'ab functionally (quote ab)
; ''ab functionally (quote (quote ab))
; the ' is syntactic sugar for (quote ...)
; so ''a is '(quote a)

; 56 - 58 in symbolic-differenctiation.scm
; 58 b - Not attempted

; 59 - in representing-sets.scm

; 60)
(define (element-of-set? x set)
  (null? (filter (lambda (y) (equal? x y)) set)))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set s1 s2)
  (filter (lambda (x) (element-of-set? x s1) s2)))

(define (union-set s1 s2)
  (fold-right adjoin-set s2 s1))

; This implementation of a set is very good for adding
; elements and much less efficient for membership 
; dependent operations as the size of the set will grow 
; with every single addition rather than when the addition is unique

; 61, 62 - In representing-sets.scm

; 63)
(define (tree->list-1 tree)
  (if (null? tree)
    ()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                     (cons (entry tree)
                           (copy-to-list (right-branch tree)
                                         result-list)))))
  (copy-to-list tree '()))

; a) Both of these implementations return a list sorted in ascending order
; b) tree->list-1 and tree->list-2 use the same order of operations,
; however the second procedure keeps a running total during execution which
; should result in a lower space complexity for the operation

(define tree->list tree->list-2)

; 64)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree es n)
  (if (= n 0)
    (cons '() es)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree es left-size)))
        (let ((left-tree (car left-result))
              (non-left-es (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-es))
                (right-result (partial-tree (cdr non-left-es)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-es (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-es))))))))
; a)
; partial-tree takes a list of elements and the number of elements to include
; in the resulting tree. If the number to include is zero, return an empty 
; value and the list of elements as the remaining. Then calculate the size of
; what will become the left sub tree for the result by finding half of the 
; current number of elements to include minus one for the current node rounded down.
; Next, partial-tree calls itself with the list of elements and the number of elements
; to include as the half rounded down to get the left sub tree, the remaining elements
; which includes the current entry and the right sub tree elements, and the size of the
; right sub tree which is defined as the total number of elements to include minus the
; number of elements to include and the current node entry. Using the elements left over
; after constructing the left sub tree, take the next element as the entry for the current
; node and the rest plus the right sub tree size calculated to call parital-tree again
; to calculate the right sub tree. Finally, return a pair where the left element is
; a node of entry, left sub tree, and right sub tree equal to the values calculated and
; the right element is the remaining nodes after calculating the node.

; b)
; The number of operations will increase linearly with the number of elements provided
; because the operation must happen for every element provided

; 65)


(define (intersection-sorted-list s1 s2)
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

(define (intersection-set t1 t2)
  (list->tree 
    (intersection-sorted-list
      (tree->list t1)
      (tree->list t2))))

(define (union-sorted-list s1 s2)
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

(define (union-set t1 t2)
  (list->tree 
    (union-sorted-list
      (tree->list t1)
      (tree->list t2))))

; 66)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else (entry set-of-redcords))))

; 67)

(define sample-tree
  (make-code-tree (make-leaf 'a 4)
                  (make-code-tree
                    (make-leaf 'b 2)
                    (make-code-tree (make-leaf 'd 1)
                                    (make-leaf 'c 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
; 0 (1 1 0 0 1 0 1 0 1 1 1 0)
; a (1 1 0 0 1 0 1 0 1 1 1 0)
; a 1 (1 0 0 1 0 1 0 1 1 1 0)
; a 1 1 (0 0 1 0 1 0 1 1 1 0)
; a 1 1 0 (0 1 0 1 0 1 1 1 0)
; a     d (0 1 0 1 0 1 1 1 0)
; a     d 0 (1 0 1 0 1 1 1 0)
; a     d a (1 0 1 0 1 1 1 0)
; a     d a 1 (0 1 0 1 1 1 0)
; a     d a 1 0 (1 0 1 1 1 0)
; a     d a   b (1 0 1 1 1 0)
; a     d a   b 1 (0 1 1 1 0)
; a     d a   b 1 0 (1 1 1 0)
; a     d a   b   b (1 1 1 0)
; a     d a   b   b 1 (1 1 0)
; a     d a   b   b 1 1 (1 0)
; a     d a   b   b 1 1 1 (0)
; a     d a   b   b     c (0)
; a     d a   b   b     c  0
; a     d a   b   b     c  a

; 68)

(define (encode message tree)
  (if (null? message)
    ()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (iter bits tree)
    (cond ((leaf? tree) (reverse bits))
          ((member symbol (symbols (left tree))) 
           (iter (cons 0 bits) (left tree)))
          ((member symbol (symbols (right tree)))
           (iter (cons 1 bits) (right tree)))))
  (iter () tree))

; 69)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (lowest-leaf leaves)
  (define (iter current processed rest)
    (cond ((null? rest) (list current processed))
          ((< (weight current) (weight (car rest)))
           (iter current 
                 (cons (car rest) processed) 
                 (cdr rest)))
          (else (iter (car rest) 
                      (cons current processed)
                      (cdr rest)))))
  (iter (car leaves) () (cdr leaves)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
    (car leaf-set)
    (let* ((first-result (lowest-leaf leaf-set))
           (first-tree (car first-result))
           (partial-remaining (cadr first-result))
           (second-result (lowest-leaf partial-remaining))
           (second-tree (car second-result))
           (remaining (cadr second-result)))
      (successive-merge 
        (cons (make-code-tree first-tree 
                              second-tree)
              remaining)))))

; 70)

(define boom-tree (generate-huffman-tree '((a 2)   (boom 1) 
                                           (get 2) (job 2) 
                                           (na 16) (sha 3)
                                           (yip 9) (wah 1))))

(define encoded-song (encode '(get a job 
                               sha na na na na na na na na
                               get a job 
                               sha na na na na na na na na
                               wah yip yip yip yip yip yip yip yip yip
                               sha boom)
                             boom-tree))

; length: 84
; fixed encoding length 2**3 (3 bits)
; 3 bits * 36 symbols = 108

; 71)

; 2**4: 1 2 4 8 16 32
;   31
;  /  \
;16    15
;     /  \
;    8    7
;        / \
;       4   3
;          / \
;         2   1
; # bits for least frequent: 4
; # bits for most frequent: 1

; 2**9: 1 2 4 8 16 32 64 128 256 512 1024
;  1024
;   / \
; 512  511
;      / \
;    256  255
;         / \
;       128  127
;            / \
;          64   63
;               / \
;             32   31
;                 /  \
;               16    15
;                    /  \
;                   8    7
;                       / \
;                      4   3
;                         / \
;                        2   1
; # bits for least frequent: 9
; # bits for most frequent: 1

; 72)
; for the case of a lobsided tree the time spent encoding
; a symbol can be either n (if membership test is constant),
; n*logn (if membership test is with a balanced tree set),
; or n**2 (if membership test is linear)
