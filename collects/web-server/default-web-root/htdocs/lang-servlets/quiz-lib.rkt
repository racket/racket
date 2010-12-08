#lang racket/base
(require racket/serialize
         web-server/http
         net/url)
(provide (struct-out mc-question)
         make-cue-page
         quiz)

(define-serializable-struct mc-question (cue answers correct-answer))

;; answer-rows: mc-question -> (listof `(tr ...))
;; format the answers as rows for an html table
(define (answer-rows mc-q)
  (let loop ([i 0] [ans-strs (mc-question-answers mc-q)])
    (cond
      [(null? ans-strs) '()]
      [else
       (cons `(tr (td (input ([type "radio"]
                              [name "answs"]
                              [value ,(number->string i)])
                             ,(car ans-strs))))
             (loop (add1 i) (cdr ans-strs)))])))

;; make-cue-page: mc-question -> url hidden-field -> html-page
;; generate the page for the question
(define (make-cue-page mc-q)
  (lambda (ses-url k-hidden)
    (response/xexpr
     `(html (head (title "Question"))
            (body
             (form ([action ,(url->string ses-url)] [method "post"]
                                                    [enctype "application/x-www-form-urlencoded"])
                   ,(mc-question-cue mc-q)
                   (table () ,@(answer-rows mc-q))
                   (input ([type "submit"]))
                   ,k-hidden))))))

(define quiz
  (list
   (make-mc-question
    "How old is the earth?"
    (list
     "6004 years"
     "4.55 billion years"
     "200,000 years")
    1)
   (make-mc-question
    "Do bears like honey?"
    (list "yes" "no")
    0)
   (make-mc-question
    "How tall is the empire state building?"
    (list "1,206 feet"
          "511 meters"
          "1,454 feet"
          "107 floors")
    2)
   (make-mc-question
    "What is the ultimate answer?"
    (list "10" "true" "64" "42")
    3)
   (make-mc-question
    "Where do babies come from?"
    (list
     "The cabbage patch"
     "The stork"
     "A watermelon seed"
     "Wal-Mart")
    1)
   (make-mc-question
    "Who was the first president of the United States?"
    (list
     "Thomas Jefferson"
     "Theodore Roosevelt"
     "George Washington"
     "Douglas Fairbanks"
     "John Adams"
     )
    2)
   (make-mc-question
    "What makes the world go 'round?"
    (list
     "money"
     "love"
     "angular momentum"
     "sex")
    0)))
