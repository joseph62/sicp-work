(lives-near ?person (Hacker Alyssa P))

; Generates only each unique result while

(lives-near ?person-1 ?person-2)

; Generates duplicate results where person-1 and person-2 are flipped.
; The first query only returns a single result because a person was specified
; for one of the operands and the flipped pairs would not satisfy the query.
; The query for all pairs of people that live near each other returns duplicates
; because both the pair and the flipped pair satisfy the query.

; If there were some way to make additional query specifications based on the output
; or perhaps some way to enforce ordering in the same way the more specific Alyssa query does,
; then we would be able to limit the results of the more broad query to only unique results.

