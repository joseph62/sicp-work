(define family-tree
  '((son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)))

(rule (grandson ?grandfather ?grandson) ; Find grandfathers even if father and/or grandson entries use wife
      (and (son-of-father ?grandfather ?father)
           (son-of-father ?father ?grandson)))

(rule (son-of-father ?father ?son) ; Find sons of fathers even if son entries use wife
      (or (and (wife ?father ?wife)
               (son ?wife ?son))
          (son ?father ?son)))

