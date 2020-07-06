(define letrec-example 
  '(letrec ((even?
              (lambda (n)
                (if (= n 0)
                  true
                  (odd? (- n 1)))))
            (odd?
              (lambda (n)
                (if (= n 0)
                  false
                  (even? (- n 1))))))
     (even? 1)))

(define (make-binding variable value)
  (list variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cadr binding))

; a)

(define (associate-temporary-bindings bindings) 
; I have no idea how to come up with temporary variable 
; names on the fly so I'm making these ahead of time. 
; This solution is brittle in the cases were the number 
; of temporary variables required exceeds the number defined.
  (define temporary-names
    '(_a _b _c _d _e _f _g _h _i _j
         _k _l _m _n _o _p _q _r _s 
         _t _u _v _w _x _y _z))
  (map (lambda (binding temporary-name)
         (list (binding-variable binding)
               (make-binding temporary-name
                             (binding-value binding))))
       bindings
       temporary-names))

(define (temporary-binding-for var temporary-bindings)
  (let ((temporary-binding (assoc var temporary-bindings)))
    (if temporary-binding
      (cadr temporary-binding)
      (error "No temporary binding for variable" var))))

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec-bindings exp)
  (cadr exp))

(define (letrec-body exp)
  (cddr exp))

(define (binding->initialization binding)
  (make-binding (binding-variable binding) '*unassigned*))

(define (letrec->let exp)
  (let ((temporary-bindings
          (associate-temporary-bindings (letrec-bindings exp)))
        (declarations
          (map binding->initialization (letrec-bindings exp))))
    (make-let 
      declarations
      (cons 
        (make-let 
          (map cadr temporary-bindings) 
          (map (lambda (var)
                 (let ((binding 
                         (temporary-binding-for var temporary-bindings)))
                   (list 'set var (binding-variable binding))))
               (map binding-variable (letrec-bindings exp))))
        (letrec-body exp)))))


(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

; b)

; letrec environment

; E1 letrec
; fact: unassigned

; E2 temp -> E1
; _a: (lambda <references fact>)

; E1 letrec
; fact: (lambda ...)

; E3 (fact 10) -> E1
; n: 10
; fact: fact

; let environment

; E1 let
; fact: (lambda <references fact>)

; E2 (fact 10) -> global
; n: 10
; -- fact not defined in E2 or global
