(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word words)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sentance (parse-sentance)))
    (require (null? *unparsed*))
    sentance))

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prepositional-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define parse-simple-noun-phrase parse-noun-phrase)

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

; (parse '(the student with the cat sleeps in the class))

(define result 
  '(sentence
     (noun-phrase
       (simple-noun-phrase (article the) (noun student))
       (prepositional-phrase
         (preposition with)
         (simple-noun-phrase
           (article the)
           (noun cat))))
     (verb-phrase
       (verb sleeps)
       (prep-phrase (prep in)
                    (simple-noun-phrase
                      (article the)
                      (noun class))))))

; (parse '(the professor lectures to the student with the cat))

(define first
  '(sentence
     (simple-noun-phrase (article the) (noun professor))
     (verb-phrase
       (verb-phrase
         (verb lectures)
         (prepositional-phrase
           (preposition to)
           (simple-noun-phrase
             (article the) (noun student))))
       (prepositional-phrase
         (preposition with)
         (simple-noun-phrase
           (article the) (noun cat))))))

(define second
  '(sentence
     (simple-noun-phrase (article the) (noun professor))
     (verb-phrase
       (verb lectures)
       (prepositional-phrase
         (preposition to)
         (noun-phrase
           (simple-noun-phrase
             (article the) (noun student))
           (prepositional-phrase
             (preposition with)
             (simple-noun-phrase
               (article the) (noun cat))))))))
