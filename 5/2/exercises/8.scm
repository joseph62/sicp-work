(define ambiguous-controller-text 
  '( 
     start
        (goto (label here))
     here
        (assign a (const 3))
        (goto (label there))
     here
        (assign a (const 4))
        (goto (label there))
     there
     )

; The extract-labels method recurses all the way to the end of the text
; and works backward adding labels on to the front of the labels structure

; The labels would look something like: '((start ()) (here (3)) (here (4)) (there ()))
; So the behavior of this controller would be to set the value of register a to 3.

; Extract-labels with check for duplicate labels

(define (contains-label? labels label)
  (if (lookup-label labels label)
      #t
      #f))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (contains-label? labels next-inst)
                                  (error "Duplicate label detected -- EXTRACT-LABELS" next-inst)
                                  (receive insts
                                           (cons (make-label-entry next-inst
                                                                   insts))))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))
