(controller
  (assign product (constant 1))
  (assign counter (constant 1))
  factorial-start 
    (test (op >) (reg counter) (reg n))
    (branch (label factorial-end))
    (assign p (op *) (reg counter) (reg product))
    (assign product (reg p))
    (assign c (op +) (reg counter) (constant 1))
    (assign counter (reg c))
    (goto (label factorial-start))
  factorial-end)
