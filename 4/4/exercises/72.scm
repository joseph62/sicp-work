; stream-flatmap and disjoin interleave their streams so that 
; all streams associated with those operations have a chance
; to provide output. This way, if there is an infinite stream,
; the output is not only results of one stream.
