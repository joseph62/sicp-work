; Euclid's Algorithm for finding the greatest common denominator

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Data path diagram
;  _  a<-b   _      / \
; |a|<--X---|b|---->|=|
;  -         -      \ /
;  |____ ___| /\     |
;      | |    |      |
;     \/ \/   |      |
;     _____   |      /\
;    / rem \  |     /0 \
;    -------  |     ----
;       |     |
;       |     X b<-t 
;  t<-r X     |
;       |     |
;    ------   |
;   |  t   |---
;    ------

; Controller diagram
;
;      | start
;      \/
;      /\
;     /= \  yes
;  -->\  /------> done
;  |   \/
;  |   | no
;  |   \/
;  |  ----
;  | |t<-r|
;  |  ----
;  |   | 
;  |   \/
;  |  ----
;  | |a<-b|
;  |  ----
;  |   | 
;  |   \/
;  |  ----
;  |-|b<-t|
;     ----
