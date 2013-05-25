#lang web-server
(require "quiz-lib.rkt")
(define interface-version 'stateless)
(provide start interface-version)

;; get-answer: mc-question -> number
;; get an answer for a multiple choice question
(define (get-answer mc-q)
  (string->number
   (bytes->string/utf-8
    (binding:form-value
     (bindings-assq #"answs" 
                    (request-bindings/raw 
                     (send/suspend/hidden (make-cue-page mc-q))))))))

;; get-answers: (listof mc-question) -> (listof number)
;; get answers for all of the quiz questions.
(define (get-answers mc-qs)
  (cond
    [(null? mc-qs) '()]
    [else
     (cons
      (get-answer (car mc-qs))
      (get-answers (cdr mc-qs)))]))

;; tally-results: (listof mc-question) (listof number) -> number
;; count the number of correct answers
(define (tally-results mc-qs answs)
  (cond
    [(null? mc-qs) 0]
    [(= (car answs)
        (mc-question-correct-answer (car mc-qs)))
     (add1 (tally-results (cdr mc-qs) (cdr answs)))]
    [else (tally-results (cdr mc-qs) (cdr answs))]))

(define (start initial-request)
  (response/xexpr
   `(html (head (title "Final Page"))
          (body
           (h1 "Quiz Results")
           (p ,(format "You got ~a correct out of ~a questions."
                       (tally-results quiz (get-answers quiz))
                       (length quiz)))
           (p "Thank you for taking the quiz")))))
