(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

; Query

(append-to-form (a b) (c d) ?z))

; Result

(append-to-form (a b) (c d) (a b c d))

; What list when appended to (a b) results in (a b c d)?

(append-to-form (a b) ?y (a b c d))

; Result

(append-to-form (a b) (c d) (a b c d))

; All pairs of lists that append to form (a b c d)

(append-to-form ?x ?y (a b c d))

; Result
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))
(append-to-form (a b c d) () (a b c d))

