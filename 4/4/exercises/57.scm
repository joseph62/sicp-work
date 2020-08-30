(rule (can-replace ?person-1 ?person-2)
      (and (or (and (job ?person-1 ?job)
                    (job ?person-2 ?job))
               (and (job ?person-1 ?person-1-job)
                    (job ?person-2 ?person-2-job)
                    (can-do-job ?person-1-job ?person-2-job)))
           (not (same ?person-1 ?person-2))))

; a) All people who can replace Cy D Fect

(can-replace ?person (Fect Cy D))

; b) All people who can replace someone who is being paid more than they are

(and (can-replace ?person-1 ?person-2)
     (salary ?person-1 ?person-1-salary)
     (salary ?person-2 ?person-2-salary)
     (lisp-value < ?person-1-salary ?person-2-salary))
