; Original
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

; Louis version
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)

; The system will
; sucessfully unify and create a frame where
;    ?staff-person = (Bitdiddle Ben)
;    ?boss = ?who
; and end up with
(or (supervisor (Bitdiddle Ben) ?who)
    (and (outranked-by ?middle-manager ?who)
         (supervisor (Bitdiddle Ben) ?middle-manager)))
; which results in 
(or (supervisor (Bitdiddle Ben) ?who)
    (and (or (supervisor ?middle-manager ?who)
             (and (outranked-by ?middle-manager ?who)
                  (supervisor ?middle-manager ?who))) 
         (supervisor (Bitdiddle Ben) ?middle-manager)))
; The outranked-by ?middle-manager ?who will continue to repeat...
