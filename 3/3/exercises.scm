; 12)
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (cond ((null? x) (error "Operand cannot be null -- LAST-PAIR" x))
        ((null? (cdr x)) x)
        (else (last-pair (cdr x)))))

(define x '(a b))
(define y '(c d))

(define z (append x y))

z
; (a b c d)

(cdr x)
; (b)

(define w (append! x y))

w
; (a b c d)

(cdr x)
; (b c d)

; z is a pointer to a new list of elements in x and y
; w is a pointer to x which has been modified to include the elements of y

; 13)
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define cycle (make-cycle '(a b c)))
; (last-pair cycle) -> Will go on forever because the very last reference in the list is 
;                       a reference to the head of the list and not null

; 14)
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; This mystery procedure reverses the given list using mutation.
; The loop procedure takes an x and a y.
; Returns the value of y if x is null.
; The saves the tail of the x list as temp.
; Mutates x so that the tail is the list of y.
; Finally, it calls loop with x being the old tail of the current x and y being
; the head of the current x with the tail of y

; 15)

; z1 is composed of a pair where the car is a pointer to the list bound to 
; the name x and the cdr is also a pointer to the list bound by x

; z2 is composed of a pair where the car is a list whose values are a and b
; and the cdr is also a list whose values are a and b. These two lists are
; not the same pair.

; 16)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list 1 2 3))
; 3

(define a (cons 1 1))
(define b (cons a a))
(define c (cons b b))
(count-pairs c)
; 7

(count-pairs (cons 1 b))
; 4

; (count-pairs (make-cycle '(1 2 3)))
; does not return

; 17)
(define (contains? x xs)
  (cond ((null? xs) #f)
        ((eq? x (car xs)) #t)
        (else (contains? x (cdr xs)))))

(define (adjoin x xs)
  (cons x xs))

(define (count-pairs x)
  (define processed ())
  (define (count-pairs-iter x)
    (cond ((not (pair? x)) 0)
          ((contains? x processed) 0)
          (else 
            (set! processed (adjoin x processed))
            (+ (count-pairs-iter (car x))
               (count-pairs-iter (cdr x))
               1))))
  (count-pairs-iter x))

(count-pairs (list 1 2 3))
; 3

(count-pairs (make-cycle (list 1 2 3)))
; 3

(count-pairs c)
; 3

; 18)
(define (has-cycle? items)
  (define (has-cycle-iter? items processed)
    (cond ((null? items) #f)
          ((contains? items processed) #t)
          (else (has-cycle-iter? (cdr items)
                                 (adjoin items processed)))))
  (has-cycle-iter? items ()))

; 19)
(define (has-cycle? items)
  (define (has-cycle-iter? items first)
    (cond ((null? items) #f)
          ((eq? items first) #t)
          (else 
            (has-cycle-iter? (cdr items) first))))
  (and (not (null? items))
       (has-cycle-iter? (cdr items) items)))

; 20)
(define x (cons 1 2))

; Global
; x: x.dispatch

; E1 -> Global (cons 1 2)
; x: 1
; y: 2
; set-x!
; set-y!
; dispatch

(define z (cons x x))

; Global
; x: x.dispatch
; z: z.dispatch

; E2 -> Global (cons x x)
; x: (Global x)
; y: (Global x)
; set-x!
; set-y!
; dispatch

(set-car! (cdr z) 17)

; E3 -> Global (cdr z)

; E4 -> E2 (z.dispatch 'cdr)

; E5 -> Global (set-car! x.dispatch 17)
; E6 -> E1 (x.dispatch 'set-car!)
; E7 -> E1 (set-x! 17)

; E1 -> Global
; x: 17
; y: 2

(car x)
; E8 -> Global (car x.dispatch)
; E9 -> E1 (x.dispatch 'car)

; 17

; 21)
(define (print-queue q)
  (display (front-ptr q)))

; 22)
(define (make-queue)
  (let ((front ())
        (rear ()))
    (define (set-front-ptr! v)
      (set! front v)
      dispatch)
    (define (set-rear-ptr! v)
      (set! rear v)
      dispatch)
    (define (dispatch m)
      (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'front-ptr) front)
            ((eq? m 'rear-ptr) rear)
            (else (error "DISPATCH code not recognized" m))))
    dispatch))

(define (front-ptr q)
  (q 'front-ptr))

(define (rear-ptr q)
  (q 'rear-ptr))

(define (set-front-ptr! q v)
  ((q 'set-front-ptr!) v))

(define (set-rear-ptr! q v)
  ((q 'set-rear-ptr!) v))

; 23)
(define (make-deque)
  (cons () ()))

(define (front-ptr d)
  (car d))

(define (rear-ptr d)
  (cdr d))

(define (set-front-ptr! d v)
  (set-car! d v)
  d)

(define (set-rear-ptr! d v)
  (set-cdr! d v)
  d)

(define (make-node v)
  (list v () ()))

(define (value n)
  (car n))

(define (prev-ptr n)
  (cadr n))

(define (next-ptr n)
  (caddr n))

(define (has-next? n)
  (not (null? (next-ptr n))))

(define (has-prev? n)
  (not (null? (prev-ptr n))))

(define (set-prev-ptr! n1 n2)
  (set-car! (cdr n1) n2)
  n1)

(define (set-next-ptr! n1 n2)
  (set-car! (cddr n1) n2)
  n1)

(define (link-prev-node! n1 n2)
  (set-prev-ptr! n1 n2)
  (set-next-ptr! n2 n1))

(define (unlink-prev-node! n)
  (cond ((has-prev? n)
         (set-next-ptr! (prev-ptr n) ())
         (set-prev-ptr! n ())
         n)
        (else (error "UNLINK-PREV node has no previous node" n))))

(define (link-next-node! n1 n2)
  (set-next-ptr! n1 n2)
  (set-prev-ptr! n2 n1))

(define (unlink-next-node! n)
  (cond ((has-next? n)
         (set-prev-ptr! (next-ptr n) ())
         (set-next-ptr! n ())
         n)
        (else (error "UNLINK-PREV node has no previous node" n))))

(define (empty-deque? d)
  (and (null? (front-ptr d))
       (null? (rear-ptr d))))

(define (front-deque d)
  (cond ((empty-deque? d)
         (error "FRONT deque is empty" d))
        (else (value (front-ptr d)))))

(define (rear-deque d)
  (cond ((empty-deque? d)
         (error "REAR deque is empty" d))
        (else (value (rear-ptr d)))))

(define (front-insert-deque! d v)
  (let ((node (make-node v)))
    (cond ((empty-deque? d)
           (set-front-ptr! d node)
           (set-rear-ptr! d node)
           d)
          (else 
            (link-prev-node! (front-ptr d) node)
            (set-front-ptr! d node)
            d))))

(define (rear-insert-deque! d v)
  (let ((node (make-node v)))
    (cond ((empty-deque? d)
           (set-front-ptr! d node)
           (set-rear-ptr! d node)
           d)
          (else 
            (link-next-node! (rear-ptr d) node)
            (set-rear-ptr! d node)
            d))))

(define (front-delete-deque! d)
  (cond ((empty-deque? d)
         (error "FRONT-DELETE deque is empty" d))
        ((has-next? (front-ptr d))
         (let ((new-front (next-ptr (front-ptr d))))
           (unlink-prev-node! new-front)
           (set-front-ptr! d new-front)
           d))
        (else
          (set-front-ptr! d ())
          (set-rear-ptr! d ())
          d)))

(define (rear-delete-deque! d)
  (cond ((empty-deque? d)
         (error "REAR-DELETE deque is empty" d))
        ((has-prev? (rear-ptr d))
         (let ((new-rear (prev-ptr (rear-ptr d))))
           (unlink-next-node! new-rear)
           (set-rear-ptr! d new-rear)
           d))
        (else
          (set-rear-ptr! d ())
          (set-next-ptr! d ())
          d)))

; 24)
(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (records table)
      (cdr table))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key)
      (let ((record (assoc key (records local-table))))
        (if record
          (cdr record)
          #f)))

    (define (insert! key value)
      (let ((record (assoc key (records local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! local-table (cons (cons key value)
                                      (records local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m logic-or'insert!) insert!)
            (else (error "Unkown command -- TABLE" m))))
    dispatch))

(define (lookup table key)
  ((table 'lookup) key))

(define (insert! table key value)
  ((table 'insert!) key value))

; 25)

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((eq? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup keys)
      (define (lookup-iter current keys)
        (let ((pair (assoc (car keys) current))
              (rest-keys (cdr keys)))
          (cond ((and (null? rest-keys) pair) (cdr pair))
                (pair (lookup-iter (cdr pair) rest-keys))
                (else #f))))
      (lookup-iter (cdr local-table) keys))

    (define (insert! keys value)
      (define (insert-iter! current keys value)
        (let ((next (assoc (car keys) (cdr current)))
              (rest-keys? (not (null? (cdr keys)))))
          (cond ((and next rest-keys?)
                 (insert-iter! next (cdr keys) value))
                ((and (not next) rest-keys?)
                 (let ((next (cons (car keys) ())))
                   (set-cdr! current (cons next
                                           (cdr current)))
                   (insert-iter! next (cdr keys) value)))
                ((and next (not rest-keys?))
                 (set-cdr! next value))
                (else 
                  (let ((next (cons (car keys) value)))
                    (set-cdr! current (cons next
                                            (cdr current))))))))
      (insert-iter! local-table keys value)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unkown operation -- TABLE" m))))
    dispatch))

; 26)
; The current make-table implementation would need to be modified
; in the following ways to accomodate a table tree:
; - The table must be created with an = < > operator for the keys
;   that are used (define (make-table = < >) ...
; - The assoc procedure would need to be modified to operate on
;   a tree structure using the =, <, and > for the table
; - The insert! operation would need to be modified to correctly
;   create the tree structure
; - A record object should be created that models the key value pair
;   to serve as the value for each node in the tree structure

; 27)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previous-computed-value (lookup table x)))
        (or previous-computed-value
            (let ((result (f x)))
              (insert! t x result)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

; Global:
; memo-fib (memoize ...)
; lookup
; insert!

; E1 -> Global memoize
; table (make-table)

; E2 -> Global make-table
; local-table
; lookup
; insert!
; assoc


; (memo-fib 3)
; E3 -> E1 (memo-fib 3) 

; E4 -> Global (lookup table)

; E5 -> E2 (lookup local-table)
; E6 -> E2 assoc 

; E7 -> Global (f x)

; 28) digital-circuits.scm

; 29)
(load "digital-circuits.scm")

(define (or-gate a1 a2 out)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (not-out (make-wire)))

    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 not-out)
    (inverter not-out out)
    'ok))

; Or gate delay = 3 * (Inverter delay) + (And gate delay)

; 30)
(define (ripple-adder as bs ss)
  (define (ripple-adder-iter current carry)
    (if (null? current)
        'ok
        (let ((a (caar current))
              (b (cadar current))
              (s (caddar current))
              (next-carry (make-wire)))
          (full-adder a b carry s next-carry)
          (ripple-adder-iter (cdr current) next-carry))))
  (ripple-adder-iter (zip as bs ss) (make-wire)))

; The wait time to compute the ripple adder of as and bs
; would be the sum of time of each full-adder because subsequent
; full adders rely on the carry of the previous full adder

; half adder:
;   S = 2 * and gate time + inverter gate time + or gate time
;   C = and gate time

; Full adder:
;   SUM = 2 * S of half adder
;   Cout = S of half adder + 2 * C of half adder + or gate delay time

; Ripple adder:
;   S1 = SUM of the full adder
;   Sn = (n - 1) * Cout of full adder + SUM of the full adder

; 31) Removing the action call in add-action! 
; means that the state of the circuit will only be correct
; when a signal is next set

; 32) The segments must be fifo because each action that enters the
; system must be executed in the order it was inserted into the list
; so even if two actions occur within the same segment, the first action
; must be executed before the second action.

; 33)
; (a + b)/2 = c
(load "constraint-propogation.scm")
(define (averager a b c)
 (let ((a-b-product (make-connector))
       (half (make-connector)))
   (constant 0.5 half)
   (multiplier a b a-b-product)
   (multiplier a-b-product half c)
   'ok))

; 34)
(define (squarer a b)
  (multiplier a a b))
; This implementation of the squarer will only work by setting a.
; Calculating a based on b will never happen because multiplier will
; check for values in two of the three connectors. So when a is set the
; formula will compute b, but if only b is set, the multiplier will not calculate a

; 35)
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (set-value! b (* a a) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-a-value)
           (process-forget-value))
          (else
            (error "Unknown request -- SQUARER" request))))
  me)

; 36)

(define a (make-connector))

; Global:
; squarer
; get-value
; has-value?
; set-value!
; forget-value!
; a: me, E1

; E1 -> Global: a (make-connector)
; value: #f
; informant: #f
; constraints: ()
; set-my-value!
; forget-my-value!
; connect
; me

(define b (make-connector))
; E2 -> Global: b (make-connector)
; value: #f
; informant: #f
; constraints: ()
; set-my-value!
; forget-my-value!
; connect
; me

; Global:
; b: me, E2

(set-value! a 10 'user)

; E3 -> Global: set-value! 
; c: a
; new-value: 10
; informant: 'user

; E4 -> E1 (me 'set-value!)
; request: 'set-value!

; E5 -> E1 (set-my-value! 10 'user)
; new-value: 10
; setter: 'user

; E6 -> Global (has-value? me)
; c: me

; E7 -> E1 (me 'has-value)
; request: 'has-value

; E8 -> Global (set! value 10)
; name: value
; value: 10

; E9 -> Global (set! informant 'user)
; name: informant
; value: 'user

; E10 -> Global (for-each-except 'user inform-about-value ())
; exception: 'user
; procedure: inform-about-value
; list: ()

; 37)

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (subtractor x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (divisor x y z)
    z))

(define (cv value)
  (let ((v (make-connector)))
    (constant value v)
    v))
