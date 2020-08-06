; Betty: K 2, B 3
; Ethel: E 1, J 2
; Joan:  J 3, E 5
; Kitty: K 2, M 4
; Mary:  M 4, B 1

(define (rank-combinations)
  (combinations-of
    cons
    (stream '(k 2) '(b 3))
    (combinations-of
      cons
      (stream '(e 1) '(j 2))
      (combinations-of
        cons
        (stream '(j 3) '(e 5))
        (combinations-of
          list
          (stream '(k 2) '(m 4))
          (stream '(m 4) '(b 1)))))))

(define (deduplicate-rankings rankings)
  (fold (lambda (rank rankings)
          (if (member rank rankings)
              rankings
              (cons rank rankings)))
        ()
        rankings))

(define (xor-rankings a b)
  (lambda (rankings)
    (or (and (member a rankings)
             (not (member b rankings)))
        (and (member b rankings)
             (not (member a rankings))))))

(define (ranking-rank ranking)
  (cadr ranking))

(define (ranking-person ranking)
  (car ranking))

(define (find-possible-ranks)
  (stream-filter
    (lambda (ranks)
      (distinct? (map ranking-rank ranks)))
    (stream-filter
      (lambda (ranks)
        (distinct? (map ranking-person ranks)))
      (stream-filter 
        (xor-rankings '(k 2) '(b 3))
        (stream-filter
          (xor-rankings '(e 1) '(j 2))
          (stream-filter
            (xor-rankings '(j 3) '(e 5))
            (stream-filter
              (xor-rankings '(k 2) '(m 4))
              (stream-filter
                (xor-rankings '(m 4) '(b 1))
                (stream-map deduplicate-rankings (rank-combinations))))))))))

; The correct answer is:
; Kitty
; Joan
; Betty
; Mary
; Ethel
