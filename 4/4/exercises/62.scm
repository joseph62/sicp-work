(rule (last-pair (?u . ?v) ?last-pair)
      (last-pair ?v ?last-pair))

(rule (last-pair (?last-element ()) (?last-element ())))

(last-pair (3) ?x)

(last-pair (3) (3))

(last-pair (1 2 3) ?x)

(last-pair (1 2 3) (3))

(last-pair (2 ?x) (3))

(last-pair (2 3) (3))

(last-pair ?x (3))

(last-pair (3) (3)) 
; There are many more answers to this query that the rules would not find
; becuase the last-pair query only knows where it ended up. We know that
; the last-pair of the list is three so the correct response would be to return
; every list ending in three. 

