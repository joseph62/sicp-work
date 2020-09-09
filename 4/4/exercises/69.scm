(rule ((grandson) ?grandfather ?grandson)
      (grandson ?grandfather ?grandson))

(rule ((great . ?rel) ?great-grandfather ?grandson)
      (and (son ?great-grandfather ?next-grandfather)
           ((?rel) ?next-grandfather ?grandson)))
