#lang racket
(require web-server/formlets
         tests/eli-tester
         web-server/http
         net/url)

(define (make-test-req bs)
  (make-request #"GET" (string->url "http://test.com") empty (delay bs) #f "127.0.0.1" 80 "127.0.0.1"))
(define (make-test-bindings . bs)
  (for/list ([b (in-list bs)]
             [i (in-naturals)])
    (make-binding:form (string->bytes/utf-8 (format "input_~a" i)) b)))
(define-syntax-rule (test-formlet formlet display args result)
  (test (formlet-display formlet) => display
        (formlet-process formlet (make-test-req (make-test-bindings . args))) => result))

(test
 (test-formlet 
  (formlet*
   (list 'p (input-string . =>* . name))
   name)
  '((p () (input ((name "input_0") (type "text")))))
  (#"Jay")
  '("Jay"))
 
 (test-formlet 
  (formlet*
   `(p ,(input-string . =>* . name))
   name)
  '((p () (input ((name "input_0") (type "text")))))
  (#"Jay")
  '("Jay"))
 
 (test-formlet 
  (formlet*
   (#%# `(p ,(input-string . =>* . name)))
   name)
  '((p () (input ((name "input_0") (type "text")))))
  (#"Jay")
  '("Jay"))
 
 (test-formlet 
  (formlet*
   `(div 
     ,@(for/list ([i (in-range 3)])
         `(p ,(input-string . =>* . name))))
   name)
  '((div ()
         (p () (input ((name "input_0") (type "text"))))
         (p () (input ((name "input_1") (type "text"))))
         (p () (input ((name "input_2") (type "text"))))))
  (#"Jay" #"Ness" #"Pokey")
  (list "Jay" "Ness" "Pokey"))
 
 (test-formlet 
  (formlet*
   `(div 
     ,@(for/list ([i (in-range 0)])
         `(p ,(input-string . =>* . name))))
   name)
  '((div ()))
  ()
  (list))
 
 (local [(define two-names
           (formlet (#%# ,(input-string . => . first-name)
                         ,(input-string . => . last-name))
                    (values first-name last-name)))]
   (test-formlet 
    (formlet*
     `(div 
       ,@(for/list ([i (in-range 3)])
           `(p ,(two-names . =>* . (values first-name last-name)))))
     (map string-append first-name (for/list ([e (in-list first-name)]) " ") last-name))
    '((div ()
           (p () (input ((name "input_0") (type "text"))) (input ((name "input_1") (type "text"))))
           (p () (input ((name "input_2") (type "text"))) (input ((name "input_3") (type "text"))))
           (p () (input ((name "input_4") (type "text"))) (input ((name "input_5") (type "text"))))))
    (#"Jay" #"McCarthy"
     #"Ness" #"Ninten"
     #"Pokey" #"Porkey")
    (list "Jay McCarthy" 
          "Ness Ninten"
          "Pokey Porkey")))
 
 (local [(define two-names
           (formlet* (#%# (input-string . =>* . first-name)
                          (input-string . =>* . last-name))
                     (values (first first-name) (first last-name))))]
   (test-formlet 
    (formlet*
     `(div 
       ,@(for/list ([i (in-range 3)])
           `(p ,(two-names . =>* . (values first-name last-name)))))
     (map string-append first-name (for/list ([e (in-list first-name)]) " ") last-name))
    '((div ()
           (p () (input ((name "input_0") (type "text"))) (input ((name "input_1") (type "text"))))
           (p () (input ((name "input_2") (type "text"))) (input ((name "input_3") (type "text"))))
           (p () (input ((name "input_4") (type "text"))) (input ((name "input_5") (type "text"))))))
    (#"Jay" #"McCarthy"
     #"Ness" #"Ninten"
     #"Pokey" #"Porkey")
    (list "Jay McCarthy" 
          "Ness Ninten"
          "Pokey Porkey")))
 
 )
