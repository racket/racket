#lang racket
(require rackunit
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
                                            #f "127.0.0.1" 80 "127.0.0.1")))
           (define (test-display f)
             (formlet-display f))]
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
      
      ; text
      (test-equal? "text-input"
                   (->cons (test-process (text-input) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "text-input"
                   (test-display (text-input))
                   '((input ((name "input_0") (type "text")))))
      (test-equal? "text-input"
                   (test-display (text-input #:value #"test"))
                   '((input ((name "input_0") (type "text") (value "test")))))
      (test-equal? "text-input"
                   (test-display (text-input #:size 20))
                   '((input ((name "input_0") (type "text") (size "20")))))
      (test-equal? "text-input"
                   (test-display (text-input #:max-length 20))
                   '((input ((name "input_0") (type "text") (maxlength "20")))))
      (test-equal? "text-input"
                   (test-display (text-input #:read-only? #t))
                   '((input ((name "input_0") (type "text") (readonly "true")))))
      (test-equal? "text-input"
                   (test-display (text-input #:read-only? #f))
                   '((input ((name "input_0") (type "text")))))
      (test-equal? "text-input"
                   (test-display (text-input #:attributes '([test "Test"])))
                   '((input ((name "input_0") (type "text") (test "Test")))))
      
      ; password
      (test-equal? "password-input"
                   (->cons (test-process (password-input) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "password-input"
                   (test-display (password-input))
                   '((input ((name "input_0") (type "password")))))
      (test-equal? "password-input"
                   (test-display (password-input #:value #"test"))
                   '((input ((name "input_0") (type "password") (value "test")))))
      (test-equal? "password-input"
                   (test-display (password-input #:size 20))
                   '((input ((name "input_0") (type "password") (size "20")))))
      (test-equal? "password-input"
                   (test-display (password-input #:max-length 20))
                   '((input ((name "input_0") (type "password") (maxlength "20")))))
      (test-equal? "password-input"
                   (test-display (password-input #:read-only? #t))
                   '((input ((name "input_0") (type "password") (readonly "true")))))
      (test-equal? "password-input"
                   (test-display (password-input #:read-only? #f))
                   '((input ((name "input_0") (type "password")))))
      (test-equal? "password-input"
                   (test-display (password-input #:attributes '([test "Test"])))
                   '((input ((name "input_0") (type "password") (test "Test")))))
      
      ; TEXTAREA element
      (test-equal? "textarea-input"
                   (binding:form-value (test-process (textarea-input) (list (make-binding:form #"input_0" #"value"))))
                   #"value")
      (test-equal? "textarea-input"
                   (test-display (textarea-input))
                   '((textarea ([name "input_0"]) "")))
      (test-equal? "textarea-input"
                   (test-display (textarea-input #:rows 80))
                   '((textarea ([name "input_0"] [rows "80"]) "")))
      (test-equal? "textarea-input"
                   (test-display (textarea-input #:cols 80))
                   '((textarea ([name "input_0"] [cols "80"]) "")))
      (test-equal? "textarea-input"
                   (test-display (textarea-input #:value #"starting text" #:cols 80))
                   '((textarea ([name "input_0"] [cols "80"]) "starting text")))
      (test-equal? "textarea-input"
                   (test-display (textarea-input #:cols 80 #:rows 70))
                   '((textarea ([name "input_0"] [rows "70"] [cols "80"]) "")))
      (test-equal? "textarea-input"
                   (test-display (textarea-input #:rows 80 #:attributes '([class "awesome"])))
                   '((textarea ([name "input_0"] [rows "80"] [class "awesome"]) "")))
      (test-equal? "textarea-input"
                   (test-display (textarea-input #:cols 80 #:attributes '([class "awesome"])))
                   '((textarea ([name "input_0"] [cols "80"] [class "awesome"]) "")))
      (test-equal? "textarea-input"
                   (test-display (textarea-input #:cols 80 #:rows 70 #:attributes '([class "awesome"])))
                   '((textarea ([name "input_0"] [rows "70"] [cols "80"] [class "awesome"]) "")))
      
      ; checkbox
      (test-equal? "checkbox"
                   (->cons (test-process (checkbox #"start" #t) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "checkbox"
                   (test-display (checkbox #"start" #t))
                   '((input
                      ((name "input_0") (type "checkbox") (value "start") (checked "true")))))
      (test-equal? "checkbox"
                   (test-display (checkbox #"start" #f))
                   '((input ((name "input_0") (type "checkbox") (value "start")))))
      (test-equal? "checkbox"
                   (test-display (checkbox #"start" #t #:attributes '([test "Test"])))
                   '((input
                      ((name "input_0")
                       (type "checkbox")
                       (value "start")
                       (checked "true")
                       (test "Test")))))
      
      ; radio
      (test-equal? "radio"
                   (->cons (test-process (radio #"start" #t) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "radio"
                   (test-display (radio #"start" #t))
                   '((input ((name "input_0") (type "radio") (value "start") (checked "true")))))
      (test-equal? "radio"
                   (test-display (radio #"start" #f))
                   '((input ((name "input_0") (type "radio") (value "start")))))
      (test-equal? "radio"
                   (test-display (radio #"start" #t #:attributes '([test "Test"])))
                   '((input ((name "input_0") (type "radio") (value "start") (checked "true") (test "Test")))))

      ; radio-group
      (test-equal? "radio-group"
                   (test-process (radio-group (list "1" "2" "3"))
                                 (list (make-binding:form #"input_0" #"0")))
                   "1")
      (test-equal? "radio-group"
                   (test-process (radio-group (list 1 2 3) #:display number->string)
                                 (list (make-binding:form #"input_0" #"0")))
                   1)
      (test-equal? "radio-group"
                   (test-process (radio-group (list 1 2 3) #:display number->string)
                                 (list (make-binding:form #"input_0" #"0")
                                       (make-binding:form #"input_0" #"1")))
                   1)
      (test-equal? "radio-group"
                   (test-display (radio-group 
                                  (list 1 2 3)
                                  #:display number->string 
                                  #:checked? even?
                                  #:attributes (λ (e) (list (list 'plus-one (number->string (add1 e)))))))
                   '((input ((name "input_0") (type "radio") (value "0") (plus-one "2")))
                     "1"
                     (input ((name "input_0") (type "radio") (value "1") (checked "true") (plus-one "3")))
                     "2"
                     (input ((name "input_0") (type "radio") (value "2") (plus-one "4")))
                     "3"))

      ; checkbox-group
      (test-equal? "checkbox-group"
                   (test-process (checkbox-group (list "1" "2" "3"))
                                 (list (make-binding:form #"input_0" #"0")))
                   (list "1"))
      (test-equal? "checkbox-group"
                   (test-process (checkbox-group (list 1 2 3) #:display number->string)
                                 (list (make-binding:form #"input_0" #"0")))
                   (list 1))
      (test-equal? "checkbox-group"
                   (test-process (checkbox-group (list 1 2 3) #:display number->string)
                                 (list (make-binding:form #"input_0" #"0")
                                       (make-binding:form #"input_0" #"1")))
                   (list 1 2))
      (test-equal? "checkbox-group"
                   (test-display (checkbox-group 
                                  (list 1 2 3)
                                  #:display number->string 
                                  #:checked? even?
                                  #:attributes (λ (e) (list (list 'plus-one (number->string (add1 e)))))))
                   '((input ((name "input_0") (type "checkbox") (value "0") (plus-one "2")))
                     "1"
                     (input ((name "input_0") (type "checkbox") (value "1") (checked "true") (plus-one "3")))
                     "2"
                     (input ((name "input_0") (type "checkbox") (value "2") (plus-one "4")))
                     "3"))
      
      ; submit
      (test-equal? "submit"
                   (->cons (test-process (submit #"start") (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "submit"
                   (test-display (submit #"start"))
                   '((input ((name "input_0") (type "submit") (value "start")))))
      (test-equal? "submit"
                   (test-display (submit #"start" #:attributes '([test "Test"])))
                   '((input ((name "input_0") (type "submit") (value "start") (test "Test")))))
      
      ; reset
      (test-equal? "reset"
                   (->cons (test-process (reset #"start") (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "reset"
                   (test-display (reset #"start"))
                   '((input ((name "input_0") (type "reset") (value "start")))))
      (test-equal? "reset"
                   (test-display (reset #"start" #:attributes '([test "Test"])))
                   '((input ((name "input_0") (type "reset") (value "start") (test "Test")))))
      
      ; file-upload
      (test-equal? "file-upload"
                   (->cons (test-process (file-upload) (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "file-upload"
                   (test-display (file-upload))
                   '((input ((name "input_0") (type "file")))))
      (test-equal? "file-upload"
                   (test-display (file-upload #:attributes '([test "Test"])))
                   '((input ((name "input_0") (type "file") (test "Test")))))
      
      ; hidden
      (test-equal? "hidden"
                   (->cons (test-process (hidden #"start") (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "hidden"
                   (test-display (hidden #"start"))
                   '((input ((name "input_0") (type "hidden") (value "start")))))
      (test-equal? "hidden"
                   (test-display (hidden #"start" #:attributes '([test "Test"])))
                   '((input ((name "input_0") (type "hidden") (value "start") (test "Test")))))
      
      ; IMG elements
      (test-equal? "img"
                   (->cons (test-process (img #"start" #"a") (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "img"
                   (test-display (img #"pic" #"http://h.d.com/1"))
                   '((img ((name "input_0") (src "http://h.d.com/1") (alt "pic")))))
      (test-equal? "img"
                   (test-display (img #"pic" #"http://h.d.com/1" #:attributes '([test "Test"])))
                   '((img ((name "input_0") (src "http://h.d.com/1") (alt "pic") (test "Test")))))
      (test-equal? "img"
                   (test-display (img #"pic" #"http://h.d.com/1" #:height 12))
                   '((img ((name "input_0") (src "http://h.d.com/1") (alt "pic") (height "12")))))
      (test-equal? "img"
                   (test-display (img #"pic" #"http://h.d.com/1" #:longdesc #"longer desc"))
                   '((img
                      ((name "input_0")
                       (src "http://h.d.com/1")
                       (alt "pic")
                       (longdesc "longer desc")))))
      (test-equal? "img"
                   (test-display (img #"pic" #"http://h.d.com/1" #:usemap #"#map"))
                   '((img
                      ((name "input_0") (src "http://h.d.com/1") (alt "pic") (usemap "#map")))))
      (test-equal? "img"
                   (test-display (img #"pic" #"http://h.d.com/1" #:width 50))
                   '((img ((name "input_0") (src "http://h.d.com/1") (alt "pic") (width "50")))))
      (test-equal? "img"
                   (test-display (img #"pic" #"http://h.d.com/1" #:height 12 #:longdesc #"longer desc" #:usemap #"#map" #:width 50))
                   '((img
                      ((name "input_0")
                       (src "http://h.d.com/1")
                       (alt "pic")
                       (height "12")
                       (longdesc "longer desc")
                       (usemap "#map")
                       (width "50")))))
      
      ; BUTTON element
      (test-equal? "button"
                   (->cons (test-process (button #"start" #"a") (list (make-binding:form #"input_0" #"value"))))
                   (cons #"input_0" #"value"))
      (test-equal? "button"
                   (test-display (button #"button" #"click me"))
                   '((button ((name "input_0") (type "button")) "click me")))
      (test-equal? "button"
                   (test-display (button #"button" #"click me" #:attributes '([test "Test"])))
                   '((button ((name "input_0") (type "button") (test "Test")) "click me")))
      (test-equal? "button"
                   (test-display (button #"button" #"click me" #:disabled #t))
                   '((button ((name "input_0") (type "button") (disabled "true")) "click me")))
      (test-equal? "button"
                   (test-display (button #"button" #"click me" #:disabled #f))
                   '((button ((name "input_0") (type "button")) "click me")))
      (test-equal? "button"
                   (test-display (button #"button" #"click me" #:value #"b1"))
                   '((button ((name "input_0") (type "button") (value "b1")) "click me")))
      (test-equal? "button"
                   (test-display (button #"button" #"click me" #:disabled #t #:value #"b2"))
                   '((button
                      ((name "input_0") (type "button") (disabled "true") (value "b2"))
                      "click me")))
      
      ; multiselect      
      (test-equal? "multiselect-input"
                   (test-process (multiselect-input (list "1" "2" "3"))
                                 (list (make-binding:form #"input_0" #"0")))
                   (list "1"))
      (test-equal? "multiselect-input"
                   (test-process (multiselect-input (list "1" "2" "3"))
                                 (list (make-binding:form #"input_0" #"0")
                                       (make-binding:form #"input_0" #"2")))
                   (list "1" "3"))
      (test-equal? "multiselect-input"
                   (test-process (multiselect-input (list "1" "2" "3"))
                                 empty)
                   empty)
      (test-equal? "multiselect-input"
                   (test-display (multiselect-input (list "1" "2" "3")))
                   '((select
                      ((multiple "true") (name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "multiselect-input"
                   (test-display (multiselect-input (list "1" "2" "3") #:attributes '([test "val"])))
                   '((select
                      ((multiple "true") (name "input_0") (test "val"))
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "multiselect-input"
                   (test-display (multiselect-input (list "1" "2" "3") #:multiple? #t))
                   '((select
                      ((multiple "true") (name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "multiselect-input"
                   (test-display (multiselect-input (list "1" "2" "3") #:multiple? #f))
                   '((select
                      ((name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "multiselect-input"
                   (test-display (multiselect-input (list "1" "2" "3") #:selected? (compose even? string->number)))
                   '((select
                      ((multiple "true") (name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1") (selected "true")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "multiselect-input"
                   (test-display (multiselect-input (list 1 2 3) #:display number->string))
                   '((select
                      ((multiple "true") (name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "multiselect-input"
                   (test-display (multiselect-input (list "xee" "john" "joe") #:display (curry format "something ~a") ))
                   '((select
                      ((multiple "true") (name "input_0"))
                      (option ((value "0")) "something xee")
                      (option ((value "1")) "something john")
                      (option ((value "2")) "something joe"))))
      
      ; select
      (test-equal? "select-input"
                   (test-process (select-input (list "1" "2" "3"))
                                 (list (make-binding:form #"input_0" #"0")))
                   "1")
      (test-equal? "select-input"
                   (test-process (select-input (list "1" "2" "3"))
                                 (list (make-binding:form #"input_0" #"0")
                                       (make-binding:form #"input_0" #"2")))
                   "1")
      (test-exn "select-input"
                exn?
                (lambda ()
                  (test-process (select-input (list "1" "2" "3"))
                                empty)))
      (test-equal? "select-input"
                   (test-display (select-input (list "1" "2" "3")))
                   '((select
                      ((name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "select-input"
                   (test-display (select-input (list "1" "2" "3") #:attributes '([test "val"])))
                   '((select
                      ((name "input_0") [test "val"])
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "select-input"
                   (test-display (select-input (list "1" "2" "3") #:selected? (compose even? string->number)))
                   '((select
                      ((name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1") (selected "true")) "2")
                      (option ((value "2")) "3"))))
      (test-equal? "select-input"
                   (test-display (select-input (list 1 2 3) #:display number->string))
                   '((select
                      ((name "input_0"))
                      (option ((value "0")) "1")
                      (option ((value "1")) "2")
                      (option ((value "2")) "3")))) 
      
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
               "Name:" ,{input-string . => . name} nbsp
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
                                  nbsp
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

