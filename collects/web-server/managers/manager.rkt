#lang racket/base
(require racket/contract
         web-server/http
         web-server/servlet/servlet-structs)

(define-struct manager (create-instance 
                        adjust-timeout!
                        clear-continuations!
                        continuation-store!
                        continuation-lookup
                        continuation-peek))

(define-struct (exn:fail:servlet-manager:no-instance exn:fail) (expiration-handler))
(define-struct (exn:fail:servlet-manager:no-continuation exn:fail) (expiration-handler))

(provide/contract
 [struct manager ([create-instance ((-> void) . -> . number?)]
                  [adjust-timeout! (number? number? . -> . void)]
                  [clear-continuations! (number? . -> . void)]
                  [continuation-store! 
                   (->
                    number? any/c 
                    (or/c false/c
                          (request? . -> . can-be-response?))
                    (list/c number? number?))]
                  [continuation-lookup (number? number? number? . -> . any/c)]
                  [continuation-peek (number? number? number? . -> . any/c)])]
 [struct (exn:fail:servlet-manager:no-instance exn:fail) 
         ([message string?]
          [continuation-marks continuation-mark-set?]
          [expiration-handler 
           (or/c false/c
                 (request? . -> . can-be-response?))])]
 [struct (exn:fail:servlet-manager:no-continuation exn:fail)
         ([message string?]
          [continuation-marks continuation-mark-set?]
          [expiration-handler 
           (or/c false/c
                 (request? . -> . can-be-response?))])])
