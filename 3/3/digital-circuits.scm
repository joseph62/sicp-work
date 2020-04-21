(load "./queues.scm")

(define (make-agenda)

  (define (make-item time action)
	(list time action))

  (define (item-time item) (car item))
  (define (item-action item) (cadr item))

  (let ((time 0)
		(agenda-items (make-queue)))
	(define (empty-agenda?)
	  (empty-queue? agenda-items))

	(define (first-agenda-item)
	  (item-action (front-queue agenda-items)))

	(define (remove-first-agenda-item!)
	  (set! time (+ time (item-time (front-queue agenda-items))))
	  (delete-queue! agenda-items))

	(define (add-to-agenda! time action)
	  (insert-queue! agenda-items (make-item time action)))

	(define (current-time)
	  time)

	(define (dispatch m)
	  (cond ((eq? m 'empty-agenda?) empty-agenda?)
			((eq? m 'first-agenda-item) first-agenda-item)
			((eq? m 'remove-first-agenda-item!) remove-first-agenda-item!)
			((eq? m 'add-to-agenda!) add-to-agenda!)
			((eq? m 'current-time) current-time)
			(else (error "Unkown operation -- AGENDA" m))))
	dispatch))

(define (empty-agenda? agenda)
  ((agenda 'empty-agenda?)))

(define (first-agenda-item agenda)
  ((agenda 'first-agenda-item)))

(define (remove-first-agenda-item! agenda)
  ((agenda 'remove-first-agenda-item!)))

(define (add-to-agenda! time action agenda)
  ((agenda 'add-to-agenda!) time action))

(define (current-time agenda)
  ((agenda 'current-time)))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-wire)
  (list 0 (list)))

(define (get-signal wire)
  (car wire))

(define (set-signal! wire new-signal)
  (set-car! wire new-signal)
  (map (lambda (action) (action)) (cadr wire))
  'ok)

(define (add-action! wire thunk)
  (set-car! (cdr wire) 
			(cons thunk 
				  (cadr wire))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
				  action
				  the-agenda))

(define (propogate)
  (if (empty-agenda? the-agenda)
	  'done
	  (let ((first-item (first-agenda-item the-agenda)))
		(first-item)
		(remove-first-agenda-item! the-agenda)
		(propogate))))

(define (probe name wire)
  (add-action! wire
			  (lambda ()
				(newline)
				(display name)
				(display " ")
				(display (current-time the-agenda))
				(display "  New-value = ")
				(display (get-signal wire)))))

(define (inverter input output)
  (define (invert-input)
	(let ((new-value (logical-not (get-signal input))))
	  (after-delay inverter-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
		((= s 1) 0)
		(else (error "Invalid  signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
	(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
	  (after-delay and-gate-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
		((and (= s1 0) (= s2 1)) 0)
		((and (= s1 1) (= s2 0)) 0)
		((and (= s1 0) (= s2 0)) 0)
		(else (error "Invalid signals" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
	(let ((new-value (logical-or (get-signal a1) (get-signal a2))))
	  (after-delay or-gate-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
		((and (= s1 0) (= s2 1)) 1)
		((and (= s1 1) (= s2 0)) 1)
		((and (= s1 0) (= s2 0)) 0)
		(else (error "Invalid signals" s1 s2))))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

; - OR -

(define (half-adder a b s c)
  (let ((d (make-wire))
		(e (make-wire)))
	(or-gate a b d)
	(and-gate a b c)
	(inverter c e)
	(and-gate d e s)
	'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
		(c1 (make-wire))
		(c2 (make-wire)))
	(half-adder b c-in s c1)
	(half-adder a s sum c2)
	(or-gate c1 c2 c-out)
	'ok))

(define (make-wire)
  (let ((signal 0)
		(actions ()))

	(define (call-all-actions)
	  (map apply actions)
	  'ok)

	(define (get-signal)
	  signal)

	(define (set-signal! s)
	  (set! signal s)
	  (call-all-actions)
	  'ok)

	(define (add-action! action)
	  (set! actions (cons action actions))
	  (call-all-actions)
	  'ok)

	(define (wire-dispatch op)
	  (cond ((eq? op 'get-signal) get-signal)
			((eq? op 'set-signal!) set-signal!)
			((eq? op 'add-action!) add-action!)
			(else (error "Unrecognized op code -- WIRE" op))))
	wire-dispatch))

(define (get-signal wire)
  ((wire 'get-signal)))

(define (set-signal! wire signal)
  ((wire 'set-signal!) signal))

(define (add-action! wire action)
  ((wire 'add-action!) action))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propogate)

(set-signal! input-2 1)

(propogate)
