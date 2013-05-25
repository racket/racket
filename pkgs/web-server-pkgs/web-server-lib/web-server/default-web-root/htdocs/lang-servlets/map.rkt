#lang web-server
(require web-server/managers/lru)

(define-native (build-list/native _ ho) build-list)

(define interface-version 'stateless)
(define manager
  (make-threshold-LRU-manager #f (* 1024 1024 128)))

(provide start interface-version manager)

;; get-number-from-user: number -> number
;; ask the user for a number
(define (get-number-from-user message)
  (let ([req
         (send/suspend/url
          (lambda (k-url)
            (response/xexpr
             `(html (head (title ,message))
                    (body
                     (form ([action ,(url->string k-url)]
                            [method "post"]
                            [enctype "application/x-www-form-urlencoded"])
                           ,message
                           (input ([type "text"] [name "number"] [value ""]))
                           (input ([type "submit"]))))))))])
    (string->number
     (bytes->string/utf-8
      (binding:form-value
       (bindings-assq #"number" 
                      (request-bindings/raw req)))))))

(define (start initial-request)
  (define how-many-numbers
    (get-number-from-user "How many numbers do you want to add?"))
  (response/xexpr
   `(html (head (title "Final Page"))
          (body
           (h1 "Final Page")
           (p ,(format "The answer is ~a"
                       (apply +
                              (build-list/native how-many-numbers
                                                 (lambda (i)
                                                   (get-number-from-user
                                                    (format "Enter number ~a" (add1 i))))))))))))
