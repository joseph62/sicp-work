; Query all computer programmers with their address

(and (job ?person (computer programmer))
     (address ?person ?where))

; Results

(and (job (Hacker Alyssa P) (computer programmer))
     (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))

(and (job (Fect Cy D) (computer programmer))
     (address (Fect Cy D) (Cambridge (Ames Street) 3)))

; Query all persons supervised by Ben Bitdiddle or Alyssa P Hacker

(or (supervisor ?person (Bitdiddle Ben))
    (supervisor ?person (Hacker Alyssa P)))

; Results

(or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))

(or (supervisor (Fect Cy D) (Bitdiddle Ben))
    (supervisor (Fect Cy D) (Hacker Alyssa P)))

(or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    (supervisor (Tweakit Lem E) (Hacker Alyssa P)))

(or (supervisor (Reasoner Louis) (Bitdiddle Ben))
    (supervisor (Reasoner Louis) (Hacker Alyssa P)))

; Query all persons supervised by Ben Bitdiddle that is not a computer programmer

(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

; Query all persons whose salary is less than 30k

(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))
