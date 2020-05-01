(include "constraint-propogation.scm")

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)

(set-value! F 212 'user)

(forget-value! C 'user)

(set-value! F 212 'user)
