(factorial 3)
; stack:
;   continue:
;   n:
;   val:

; registers:
;   continue: caller
;   n: 3
;   val:

; ----

; stack:
;   continue: 
;     - caller
;   n:
;     - 3
;   val:

; registers:
;   continue: factorial-sub-call-finished
;   n: 2
;   val:

; ----

; stack:
;   continue: 
;     - caller
;   n:
;     - 3
;   val:

; registers:
;   continue: factorial-sub-call-finished
;   n: 2
;   val:

; ----

; stack:
;   continue:
;     - factorial-sub-call-finished
;     - caller
;   n:
;     - 2
;     - 3
;   val:

; registers:
;   continue: factorial-sub-call-finished
;   n: 1
;   val: 1

; ----

; stack:
;   continue:
;     - caller
;   n:
;     - 3
;   val:

; registers:
;   continue: factorial-sub-call-finished
;   n: 2
;   val: 2

; ----

; stack:
;   continue:
;   n:
;   val:

; registers:
;   continue: caller
;   n: 3
;   val: 6

; = 6

(fib 2)

; stack:
;   continue:
;   n:
;   val:

; registers:
;   continue: caller
;   n: 2
;   val: 

; ----

; stack:
;   continue:
;       - caller
;   n:
;       - 2
;   val:

; registers:
;   continue: fib-n-1-done
;   n: 1
;   val: 1 

; ----

; stack:
;   continue:
;   n:
;   val:

; registers:
;   continue: caller
;   n: 2
;   val: 1 

; ----

; stack:
;   continue:
;       - caller
;   n:
;   val:
;       - 1

; registers:
;   continue: fib-n-2-done
;   n: 0
;   val: 0

; ----

; stack:
;   continue:
;   n:
;   val:

; registers:
;   continue: caller
;   n: 0
;   val: 1

; = 1
