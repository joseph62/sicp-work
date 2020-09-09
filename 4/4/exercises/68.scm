
(rule (append-to-form () ?y ?y)) ; appending the empty list to y yields y

(rule (append-to-form (?u . ?v) ?y (?u . ?z)) ; appending a list where u is the head and v is the rest to y
      (append-to-form ?v ?y ?z))              ; yields a list where u is the head and z is the rest where z
                                              ; where z is the result of appending v to y.

; The reverse of the one element list is the one element list
(rule (reverse (?a ()) (?a ())))

; The reverse of list y is the reverse of the rest of y, 
; b on to the one element list a
(rule (reverse (?a . ?b) ?z)
      (append-to-form (reverse ?b ?y) (?a) ?z))
