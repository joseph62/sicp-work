; The register save and restore that can be removed from the 
; fib example in the book is the one that happens for the second
; recursive call to fib. The fib controller does not actually need the value
; of n to complete the computation, just the value of (fib (- n 1)) and (fib (- n 2))
