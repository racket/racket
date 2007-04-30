;; Written by Don Felgar, edited by Greg Pettyjohn
;;
;; Multiple-choice quiz PLT servlet sample.
;;
;;
;; Question sexp interface:
;; Questions = (listof intro-text (listof question))
;; intro-text = string explaining what test is about
;; question = (question-text choices correct-answer explanation)
;; question-text = string, the actual question
;; choices = (listof string), possible answers to the question
;; correct-answer = integer, index into choices
;;
(module quiz mzscheme
  ;; Configuration
  (define *data-file*
    (build-path (collection-path "web-server")
                "default-web-root" "servlets" "examples" "english-measure-questions.ss"))
  (define *questions-per-quiz* 5)
  
  (require (lib "servlet.ss" "web-server")
           (lib "list.ss")
           (lib "etc.ss"))
  
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  ;; Accessors into question sexp's
  (define question-text car)
  (define question-choices cadr)
  (define question-answer caddr)
  (define question-explanation cadddr)
  
  (define quiz (load *data-file*))
  (define quiz-intro (car quiz))
  (define all-questions (cadr quiz))
  
  ;; ask-question: question number number -> (listof (cons symbol string))
  ;; Page for asking quiz question.
  ;; result contains a binding for 'answer
  (define (ask-question question-sexp question-number n-questions)
    (request-bindings
     (send/suspend
      (lambda (k-url)
        (let ((answer-num -1))
          `(html
            (head
             (title "Quiz Servlet")
             (body
              (p ,(format "Question ~A of ~A" (add1 question-number)
                          n-questions))
              (p ,(question-text question-sexp))
              (form ((method "post") (action ,k-url))
                    ,@(map (lambda (choice)
                             (set! answer-num (add1 answer-num))
                             `(p
                               ,(choice-descriptor answer-num) ". "
                               (input ((type "radio")
                                       (name "answer")
                                       (value ,(number->string
                                                answer-num))))
                               ,choice))
                           (cadr question-sexp))
                    (input ((type "submit")
                            (value "Next"))))))))))))
  
  ;; ((listof question-sexp) size) -> (listof question-sexp)
  ;; Choose a subset (without duplicates) of given size from a given list
  ;; assume subset-size <= (length questions)
  (define (random-question-set questions subset-size)
    (let choose-questions ([questions questions]
                           [chosen '()])
      (if (= (length chosen) subset-size)
          chosen
          (let ([qstn (list-ref questions (random (length questions)))])
            (choose-questions
             (filter
              (lambda (q)
                (not (eq? qstn q)))
              questions)
             (cons qstn chosen))))))
  
  ;; choice-descriptor: number -> character
  ;; Map 0 to "A", 1 to "B", etc
  (define (choice-descriptor number)
    (string (integer->char (+ (char->integer #\A) number))))
  
  ;; begin-quiz: -> request
  ;; request bindings are not currently used
  (define (begin-quiz)
    (send/suspend
     (lambda (k-url)
       `(html
         (head
          (title "Quiz Servlet"))
         (body
          (p ,quiz-intro)
          (form ((method "post") (action ,k-url))
                (input ((type "submit")
                        (value "Begin Quiz")))))))))
  
  ;; Compare list of questions to answers.
  ;; ((listof question-sexp) (listof integer|false)) -> (listof integer)
  (define (score-quiz questions answers)
    (foldr
     (lambda (question answer csw)
       (let ([correct-answer (question-answer question)])
         (apply
          (lambda (correct skipped wrong)
            (cond
              [(not answer)
               (list correct (add1 skipped) wrong)]
              [(= answer correct-answer)
               (list (add1 correct) skipped wrong)]
              [else
               (list correct skipped (add1 wrong))]))
          csw)))
     (list 0 0 0) questions answers))
  
  ;; end-quiz: (listof question) (listof (or/c number false)) -> request
  ;; request bindings are not currently used.
  (define (end-quiz questions answers)
    (send/forward
     (lambda (k-url)
       (let* ((score (score-quiz questions answers))
              (correct (car score))
              (skipped (cadr score))
              (wrong (caddr score))
              (xml
               `(html
                 (head
                  (title "Quiz Servlet"))
                 (body
                  (p ,(format "Your score: ~A/~A"
                              correct
                              (+ correct wrong skipped)))
                  (p ,(format "Correct: ~A" correct))
                  (p ,(format "Skipped: ~A" skipped))
                  (p ,(format "Wrong: ~A" wrong))
                  (table ((border "5"))
                         (tr (td "Question") (td "Correct Answer")
                             (td "Your Answer") (td "Explanation"))
                         ,@(map
                            (lambda (q a)
                              `(tr
                                (td ,(question-text q))
                                (td ,(format "~A. ~A"
                                             (choice-descriptor
                                              (question-answer q))
                                             (list-ref (question-choices q)
                                                       (question-answer q))))
                                (td ,(if a
                                         (format "~A. ~A" (choice-descriptor a)
                                                 (list-ref
                                                  (question-choices q) a))
                                         "Skipped"))
                                (td ,(question-explanation q))))
                            questions answers))
                  (form ((method "get")
                         (action ,k-url))
                        (input ((type "submit")
                                (value "New Quiz"))))))))
         xml))))
  
  ;; Return the first value for key in bindings, if it at least one
  ;; exists, otherwise #f.
  (define (binding-value key bindings)
    (and (exists-binding? key bindings)
         (extract-binding/single key bindings)))
  
  ;; run-quiz: -> void
  ;; run quizes until the student gets tired
  (define (run-quiz)
    (let ([*questions-per-quiz*
           (if (> *questions-per-quiz* (length all-questions))
               (begin
                 (display (format "~A ~A ~A ~A\n"
                                  "Configuration error.  *questions-per-quiz*:"
                                  *questions-per-quiz*
                                  "for a question list of size"
                                  (length all-questions)))
                 (length all-questions))
               *questions-per-quiz*)])
      
      (let ([questions (random-question-set all-questions
                                            *questions-per-quiz*)])
        (begin-quiz)
        (let ([answers
               (build-list (length questions)
                           (lambda (question-number)
                             (let ([answer
                                    (binding-value 'answer
                                                   (ask-question
                                                    (list-ref questions question-number)
                                                    question-number
                                                    *questions-per-quiz*))])
                               (and answer (string->number answer)))))])
          (end-quiz questions answers))))
    (run-quiz))
  
  ;; Entry point into servlet.
  (define (start initial-request)
    (run-quiz)))

