(define (RC R C dt)
  (lambda (i v0)
    (stream-add
      (scale-stream i R)
      (integral (scale-stream i (/ 1 C)) 
                v0 
                dt))))



