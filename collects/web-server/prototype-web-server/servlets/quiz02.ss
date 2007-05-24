(module quiz02 (lib "lang.ss" "web-server" "prototype-web-server")
  (require "quiz-lib.ss"
           (lib "servlet-helpers.ss" "web-server" "private"))
  (provide start)
  
  ;; get-answer: mc-question -> number
  ;; get an answer for a multiple choice question
  (define (get-answer mc-q)
    (let* ([req
           (send/suspend/hidden (make-cue-page mc-q))]
           [bdgs (request-bindings req)])
      (if (exists-binding? 'answs bdgs)
          (string->number
           (extract-binding/single
            'answs bdgs))
          -1)))
    
  ;; get-answers: (-> (listof mc-question)) -> (listof number)
  ;; get answers for all of the quiz questions.
  (define (get-answers get-mc-qs)
    (cond
      [(null? (get-mc-qs)) '()]
      [else
       (cons
        (get-answer (car (get-mc-qs)))
        (get-answers (lambda () (cdr (get-mc-qs)))))]))
  
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
    `(html (head (title "Final Page"))
           (body
            (h1 "Quiz Results")
            (p ,(format "You got ~a correct out of ~a questions."
                        (tally-results quiz (get-answers (lambda () quiz)))
                        (length quiz)))
            (p "Thank you for taking the quiz")))))