; All computer programmers query

(job ?x (computer programmer))

; Results

(job (Hacker Alyssa P) (computer programmer))
(job (Fect Cy D) (computer programmer))

; All employee address query

(address ?x ?y)

; All people who supervise themselves

(supervisor ?x ?x)

; All people in the computer division

(job ?name (computer . ?type))

(computer ?type)

; Will match 
(computer programmer)

; Won't match
(computer programmer trainee)

(computer . ?type)

; Will match both
(computer programmer)
(computer programmer trainee)
