(define (pass) (final-page "You may pass."))

(define name (single-query "What is your name?"))
(define quest (single-query "What is your quest?"))

(if (string-ci=? name "Matthias")
    (let ([v (string->number (single-query "What is the terminal velocity of a sparrow?"))])
      (if (and v (= v 5))
          (pass)
          (final-page "Study sparrows more.")))
    (begin 
      (single-query "What is your favorite color?")
      (pass)))
