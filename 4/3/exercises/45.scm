(parse '(the professor lectures to the student in the class with the cat))

(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prepositional-phrase
      (preposition to)
      (noun-phrase
        (simple-noun-phrase
          (article the) (noun student))
        (prepositional-phrase
          (preposition in)
          (noun-phrase
            (simple-noun-phrase
              (article the) (noun class))
            (prepositional-phrase
              (preposition with)
              (simple-noun-phrase
                (article the) (noun cat)))))))))
; Students are in the classroom with the cat
; Professor is lecturing to them

(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prepositional-phrase
        (preposition to)
        (noun-phrase
          (simple-noun-phrase
            (article the) (noun student))
          (prepositional-phrase
            (preposition in)
            (simple-noun-phrase
              (article the) (noun class))))))
    (prepositional-phrase
      (preposition with)
      (simple-noun-phrase
        (article the) (noun cat)))))
; Students are in the classroom
; Professor is lecturing to them with the cat

(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prepositional-phrase
        (preposition to)
        (simple-noun-phrase
          (article the) (noun student))))
    (prepositional-phrase
      (preposition in)
      (noun-phrase 
        (simple-noun-phrase
          (article the) (noun class))
        (prepositional-phrase
          (preposition with)
          (simple-noun-phrase 
            (article the) (noun cat)))))))
; Professor lectures students
; Professor does this with the cat in the class

(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
        (verb lectures)
        (prepositional-phrase
          (preposition to)
          (simple-noun-phrase
            (article the) (noun student)))) 
      (prepositional-phrase
        (preposition in)
        (simple-noun-phrase
          (article the) (noun class))))
    (prepositional-phrase
      (preposition with)
      (simple-noun-phrase 
        (article the) (noun cat)))))

; Professor lectures students 
; Happens in the class
; Happens with the cat

; Not sure what the last one is...
