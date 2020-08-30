; a) The names of all people who are supervised by Ben Bitdiddle, together with their addresses

(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?address))

; b) All people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary

(and (salary (Bitdiddle Ben) ?ben-salary)
     (salary ?name ?salary)
     (list-value < ?salary ?ben-salary))

; c) All people who are supervised by someone who is not in the computer division,
;  together with the supervisor's name and job

(and (supervisor ?supervisee ?supervisor)
     (not (job ?supervisor (computer . ?title)))
     (job ?supervisor ?full-title))

