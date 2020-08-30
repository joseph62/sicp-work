(rule (big-shot ?person)
      (and (supervisor ?person ?supervisor)
           (job ?person (?person-division . ?person-title))
           (job ?supervisor (?supervisor-division . ?supervisor-title))
           (not (same ?person-division ?supervisior-division))))
           
