(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(wheel ?who)

; Results
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

; Oliver Warbucks shows up four times because he matches the criteria for different ways:

(and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
     (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
     (supervisor (Fect Cy D) (Bitdiddle Ben)))

(and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
     (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(and (supervisor (Eben Scrooge) (Warbucks Oliver))
     (supervisor (Cratchet Robert) (Eben Scrooge)))
