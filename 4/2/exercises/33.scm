(define (eval-quotation exp env)
  (let ((quotation (text-of-quotation exp)))
    (if (list-quotation? quotation)
        (eval 
          (quotation->list-of-quotations quotation)
          env)
        quotation)))

(define (text-of-quotation quotation)
  (cadr quotation))

(define (list-quotation? quotation)
  (pair? quotation))

(define (quotation->list-of-quotations quotations)
  (fold-right (lambda (l r)
                (list 'cons (list 'quote l) r))
              ()
              quotations))
