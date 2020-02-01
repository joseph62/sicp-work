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
