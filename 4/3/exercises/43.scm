(define (daughters) (amb 'lorna
                         'gabrielle
                         'melissa
                         'rosalind
                         'mary-ann))

(define (make-father name yacht daughter)
  (list name yacht daughter))

(define (father-yacht father)
  (cadr yacht))

(define (father-daughter father)
  (caddr father))

(define (father-name father)
  (car father))

(define (father-of-daughter daughter fathers)
  (let ((father (filter (lambda (father) 
                          (eq? (father-daughter father) daughter))
                        fathers)))
    (if (null? father)
        ()
        (car father))))

(define (yacht-not-daughter? father)
  (not (eq? (father-yacht father)
            (father-daughter father))))

(define (get-yachts-and-daughters)
  (let ((moore (make-father 'moore (daughters) (daughters))))
    (require (eq? 'mary-ann (father-daughter moore)))
    (require (eq? 'lorna (father-yacht moore)))
    (let ((barnacle (make-father 'barnacle (daughters) (daughters))))
      (require (eq? 'gabrielle (father-yacht barnacle)))
      (require (eq? 'melissa (father-daughter barnacle)))
      (let ((downing (make-father 'downing (daughters) (daughters))))
        (require (eq? 'melissa (father-yacht downing)))
        (require (yacht-not-daughter? downing))
        (let ((hall (make-father 'hall (daughters) (daughters))))
          (require (eq? 'rosalind (father-yacht hall)))
          (require (yacht-not-daughter? hall))
          (let ((parker (make-father 'parker (daughters) (daughters))))
            (require (yacht-not-daughter? parker))
            (let* ((fathers (list downing hall barnacle parker moore))
                   (gabrielle-father (father-of-daughter 'gabrielle
                                                         fathers)))
              (require (not (null? gabrielle-father)))
              (require (eq? (father-daughter parker)
                            (father-yacht gabrielle-father)))
              (require (unique? (map father-daughter
                                     (list downing hall barnacle parker moore))))
              (require (unique? (map father-yacht
                                     (list downing hall barnacle parker moore))))
              fathers)))))))


