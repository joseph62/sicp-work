(define meetings-entries
  '((meeting accounting (Monday 9am))
    (meeting administration (Monday 10am))
    (meeting computer (Wednesday 3pm))
    (meeting whole-company (Wednesday 4pm))
    (meeting administration (Friday 1pm))))

; a) All meetings occuring on Friday

(meeting ?division (Friday ?time))

; b)

(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?title))
           (or (meeting ?division ?day-and-time)
               (meeting whole-company ?day-and-time))))

; c) All meetings for Alyssa on Wednesday

(meeting-time (Hacker Alyssa P) (Wednesday ?time)) 
