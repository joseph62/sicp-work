
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

; Wheels are supervisors that are supervised by a supervisor
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

; All people that live near Ben Bitdiddle

(lives-near ?person (Ben Bitdiddle))

; All computer programmers that live near Ben Bitdiddle

(and (lives-near ?person (Ben Bitdiddle))
     (job ?person (computer programmer)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
