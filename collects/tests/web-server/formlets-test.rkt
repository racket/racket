#lang racket
(require schemeunit
         net/url
         web-server/http
         web-server/formlets
         web-server/formlets/lib)
(provide all-formlets-tests)

(define (run-formlet f i)
  (define-values (r proc post-i) (f i))
  (list r proc post-i))

(define all-formlets-tests
  (test-suite
   "Formlets"
   
   (test-suite 
    "lib"
    
    (test-case "pure (render)" (check-equal? (first (run-formlet (pure 0) 0)) empty))
    (test-case "pure (proc)" 
               (let ([x (random 1000)])
                 (check-equal? ((second (run-formlet (pure x) 0)) empty)
                               x)))
    (test-case "pure (post-i)"
               (let ([x (random 1000)])
                 (check-equal? (third (run-formlet (pure 0) x))
                               x)))
    
    (test-case "xml-forest (render)" (check-equal? (formlet-display (xml-forest (list))) empty))
    (test-case "xml-forest (render)" (check-equal? (formlet-display (xml-forest (list '(html (body "Hey"))))) 
                                                   (list '(html (body "Hey")))))
    (test-case "xml-forest (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (xml-forest (list)) 0)) empty) x)
                               x)))
    (test-case "xml-forest (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (xml-forest (list)) 0)) (list (make-binding:form #"name" #"value"))) x)
                               x)))
    (test-case "xml-forest (post-i)"
               (let ([x (random 1000)])
                 (check-equal? (third (run-formlet (xml-forest (list)) x))
                               x)))
    
    (test-case "xml (render)" (check-equal? (formlet-display (xml '(html (body "Hey")))) 
                                            (list '(html (body "Hey")))))
    (test-case "xml (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (xml '(html (body "Hey"))) 0)) empty) x)
                               x)))
    (test-case "xml (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (xml '(html (body "Hey"))) 0)) (list (make-binding:form #"name" #"value"))) x)
                               x)))
    (test-case "xml (post-i)"
               (let ([x (random 1000)])
                 (check-equal? (third (run-formlet (xml '(html (body "Hey"))) x))
                               x)))
    
    (test-case "text (render)" (check-equal? (formlet-display (text "Hey"))
                                             (list "Hey")))
    (test-case "text (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (text "Hey") 0)) empty) x)
                               x)))
    (test-case "text (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (text "Hey") 0)) (list (make-binding:form #"name" #"value"))) x)
                               x)))
    (test-case "text (post-i)"
               (let ([x (random 1000)])
                 (check-equal? (third (run-formlet (text "Hey") x))
                               x)))
    
    (test-case "tag-xexpr (render)" (check-equal? (formlet-display (tag-xexpr 'p (list (list 'id "p")) (text "Hey")))
                                                  '((p ([id "p"]) "Hey"))))
    (test-case "tag-xexpr (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (tag-xexpr 'p (list (list 'id "p")) (text "Hey")) 0)) empty) x)
                               x)))
    (test-case "tag-xexpr (proc)" 
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (tag-xexpr 'p (list (list 'id "p")) (text "Hey")) 0))
                                 (list (make-binding:form #"name" #"value"))) x)
                               x)))
    (test-case "tag-xexpr (post-i)"
               (let ([x (random 1000)])
                 (check-equal? (third (run-formlet (tag-xexpr 'p (list (list 'id "p")) (text "Hey")) x))
                               x)))
    
    (test-case "cross (render)" 
               (check-equal? (formlet-display (cross (text "One") (text "Two")))
                             (list "One" "Two")))
    (test-case "cross (proc)"
               (let ([x (random 1000)])
                 (check-equal? (((second (run-formlet (cross (text "One") (text "Two")) 0)) empty) x)
                               x)))
    (test-case "cross (proc)"
               (check-equal? ((second (run-formlet (cross (pure string->number)
                                                          (pure "100"))
                                                   0)) empty)
                             100))
    (test-case "cross (post-i)"
               (let ([x (random 1000)])
                 (check-equal? (third (run-formlet (cross (text "One") (text "Two")) x))
                               x)))
    
    (test-case "cross* (render)" 
               (check-equal? (formlet-display (cross* (text "One") (text "Two")))
                             (list "One" "Two")))
    (test-case "cross* (proc)"
               (check-equal? ((second (run-formlet (cross* (pure (lambda xs (apply string->number xs)))
                                                           (pure "100"))
                                                   0)) empty)
                             100))
    (test-case "cross* (post-i)"
               (let ([x (random 1000)])
                 (check-equal? (third (run-formlet (cross* (text "One") (text "Two")) x))
                               x))))
   
   (local [(define (->cons bf)
             (cons (binding-id bf)
                   (binding:form-value bf)))
           (define (test-process f bs)
             (formlet-process f
                              (make-request #"GET" (string->url "http://test.com")
                                            empty
                                            (delay bs)
                                            #f "127.0.0.1" 80 "127.0.0.1")))]
     (test-suite
      "Input"
      
      (test-equal? "make-input"
                   (->cons (test-process (make-input (lambda (n) n)) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "make-input"
                   (test-process (make-input (lambda (n) n)) empty)
                   #f)
      
      (test-equal? "make-input*"
                   (map ->cons (test-process (make-input* (lambda (n) n)) (list (make-binding:form #"input_0" #"value"))))
                   (list (cons #"input_0" #"value")))
      (test-equal? "make-input*"
                   (map ->cons (test-process (make-input* (lambda (n) n)) (list (make-binding:form #"input_0" #"value0")
                                                                                (make-binding:form #"input_0" #"value1"))))
                   (list (cons #"input_0" #"value0")
                         (cons #"input_0" #"value1")))
      (test-equal? "make-input*"
                   (test-process (make-input* (lambda (n) n)) empty)
                   empty)
      
      (test-equal? "text-input"
                   (->cons (test-process (text-input) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "password-input"
                   (->cons (test-process (password-input) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "checkbox"
                   (->cons (test-process (checkbox #"start" #t) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      
      (test-equal? "multiselect-input"
                   (test-process (multiselect-input (list 1 2 3))
                                 (list (make-binding:form #"input_0" #"0")))
                   (list 1))
      (test-equal? "multiselect-input"
                   (test-process (multiselect-input (list 1 2 3))
                                 (list (make-binding:form #"input_0" #"0")
                                       (make-binding:form #"input_0" #"2")))
                   (list 1 3))
      (test-equal? "multiselect-input"
                   (test-process (multiselect-input (list 1 2 3))
                                 empty)
                   empty)
      
      ; XXX check output
      
      (test-equal? "select-input"
                   (test-process (select-input (list 1 2 3))
                                 (list (make-binding:form #"input_0" #"0")))
                   1)
      (test-equal? "select-input"
                   (test-process (select-input (list 1 2 3))
                                 (list (make-binding:form #"input_0" #"0")
                                       (make-binding:form #"input_0" #"2")))
                   1)
      (test-exn "select-input"
                exn?
                (lambda ()
                  (test-process (select-input (list 1 2 3))
                                empty)))
      
      (test-equal? "required" 
                   (test-process (required (text-input)) (list (make-binding:form #"input_0" #"value")))
                   #"value")
      (test-exn "required"
                exn?
                (lambda ()
                  (test-process (required (text-input)) empty)))
      
      (test-equal? "default"
                   (test-process (default #"def" (text-input)) (list (make-binding:form #"input_0" #"value")))
                   #"value")
      (test-equal? "default"
                   (test-process (default #"def" (text-input)) empty)
                   #"def")
      
      (test-equal? "textarea-input"
                   (test-process (textarea-input) (list (make-binding:form #"input_0" #"value")))
                   "value")
      
      (test-equal? "to-string"
                   (test-process (to-string (required (text-input))) (list (make-binding:form #"input_0" #"value")))
                   "value")
      (test-equal? "to-symbol"
                   (test-process (to-symbol (to-string (required (text-input)))) (list (make-binding:form #"input_0" #"value")))
                   'value)
      (test-equal? "to-number"
                   (test-process (to-number (to-string (required (text-input)))) (list (make-binding:form #"input_0" #"100")))
                   100)
      (test-equal? "to-boolean"
                   (test-process (to-boolean (required (text-input))) (list (make-binding:form #"input_0" #"on")))
                   #t)
      (test-equal? "to-boolean"
                   (test-process (to-boolean (required (text-input))) (list (make-binding:form #"input_0" #"off")))
                   #f)
      
      ))
   
   
   (local [(define-struct date (month day) #:transparent)
           (define (date->xml d)
             (format "~a/~a"
                     (date-month d)
                     (date-day d)))      
           (define (submit t)
             `(input ([type "submit"]) ,t))
           (define date-formlet
             (formlet
              (div
               "Month:" ,{input-int . => . month}
               "Day:" ,{input-int . => . day})
              (make-date month day)))
           
           (define travel-formlet
             (formlet
              (div
               "Name:" ,{input-string . => . name}
               (div
                "Arrive:" ,{date-formlet . => . arrive}
                "Depart:" ,{date-formlet . => . depart})
               ,@(list "1" "2" "3")
               ,(submit "Submit"))
              (list name arrive depart)))
           
           (define display-itinernary
             (match-lambda
               [(list name arrive depart)
                `(html
                  (head (title "Itinerary"))
                  (body
                   "Itinerary for: " ,name
                   "Arriving:" ,(date->xml arrive)
                   "Departing:" ,(date->xml depart)))]))]
     (test-suite
      "Date"
      
      (test-case "date->xml"
                 (check-equal? (date->xml (make-date 1 2))
                               "1/2"))
      (test-case "date-formlet"
                 (check-equal? (formlet-display date-formlet)
                               '((div () "Month:" (input ((name "input_0") (type "text"))) "Day:" (input ((name "input_1") (type "text")))))))
      (test-case "travel-formlet"
                 (check-equal? (formlet-display travel-formlet)
                               '((div
                                  ()
                                  "Name:"
                                  (input ((name "input_0") (type "text")))
                                  (div
                                   ()
                                   "Arrive:"
                                   (div () "Month:" (input ((name "input_1") (type "text"))) "Day:" (input ((name "input_2") (type "text"))))
                                   "Depart:"
                                   (div () "Month:" (input ((name "input_3") (type "text"))) "Day:" (input ((name "input_4") (type "text")))))
                                  "1"
                                  "2"
                                  "3"
                                  (input ((type "submit")) "Submit")))))
      (test-case "travel-formlet (proc)"
                 (check-equal? (formlet-process travel-formlet
                                                (make-request #"GET" (string->url "http://test.com")
                                                              empty
                                                              (delay
                                                                (list (make-binding:form #"input_0" #"Jay")
                                                                      (make-binding:form #"input_1" #"10")
                                                                      (make-binding:form #"input_2" #"6")
                                                                      (make-binding:form #"input_3" #"10")
                                                                      (make-binding:form #"input_4" #"8")))
                                                              #f "127.0.0.1" 80 "127.0.0.1"))
                               (list "Jay" (make-date 10 6) (make-date 10 8))))))
   
   ; Multiple value formlets
   (local [(define (date->xml m d)
             (format "~a/~a" m d))      
           (define (submit t)
             `(input ([type "submit"]) ,t))
           (define date-formlet
             (formlet
              (div
               "Month:" ,{input-int . => . month}
               "Day:" ,{input-int . => . day})
              (values month day)))
           
           (define travel-formlet
             (formlet
              (div
               "Name:" ,{input-string . => . name}
               (div
                "Arrive:" ,{date-formlet . => . (values arrive-m arrive-d)}
                "Depart:" ,{date-formlet . => . (values depart-m depart-d)})
               ,@(list "1" "2" "3")
               ,(submit "Submit"))
              (values name arrive-m arrive-d depart-m depart-d)))
           
           (define-syntax-rule (check-equal?/values actual-expr expected-expr)
             (call-with-values (lambda () actual-expr)
                               (lambda actual
                                 (call-with-values (lambda () expected-expr)
                                                   (lambda expected
                                                     (check-equal? actual expected))))))]
     (test-suite
      "Date (values)"
      
      (test-case "date->xml"
                 (check-equal? (date->xml 1 2)
                               "1/2"))
      (test-case "date-formlet"
                 (check-equal? (formlet-display date-formlet)
                               '((div () "Month:" (input ((name "input_0") (type "text"))) "Day:" (input ((name "input_1") (type "text")))))))
      (test-case "travel-formlet"
                 (check-equal? (formlet-display travel-formlet)
                               '((div
                                  ()
                                  "Name:"
                                  (input ((name "input_0") (type "text")))
                                  (div
                                   ()
                                   "Arrive:"
                                   (div () "Month:" (input ((name "input_1") (type "text"))) "Day:" (input ((name "input_2") (type "text"))))
                                   "Depart:"
                                   (div () "Month:" (input ((name "input_3") (type "text"))) "Day:" (input ((name "input_4") (type "text")))))
                                  "1"
                                  "2"
                                  "3"
                                  (input ((type "submit")) "Submit")))))
      (test-case "travel-formlet (proc)"
                 (check-equal?/values (formlet-process travel-formlet
                                                       (make-request #"GET" (string->url "http://test.com")
                                                                     empty
                                                                     (delay
                                                                       (list (make-binding:form #"input_0" #"Jay")
                                                                             (make-binding:form #"input_1" #"10")
                                                                             (make-binding:form #"input_2" #"6")
                                                                             (make-binding:form #"input_3" #"10")
                                                                             (make-binding:form #"input_4" #"8")))
                                                                     #f "127.0.0.1" 80 "127.0.0.1"))
                                      (values "Jay" 10 6 10 8)))))
   
   ))

(require schemeunit/text-ui)
(run-tests all-formlets-tests)