#lang racket/base
(require racket/match
         racket/contract)
(require "manager.rkt")
(require web-server/private/timer
         web-server/http
         web-server/servlet/servlet-structs)
(provide/contract
 [create-timeout-manager 
  (->
   (or/c false/c
         (request? . -> . can-be-response?))
   number? number? 
   manager?)])

;; Utility
(define (make-counter)
  (let ([i 0])
    (lambda ()
      (set! i (add1 i))
      i)))

(define-struct (timeout-manager manager) (instance-expiration-handler
                                          instance-timer-length
                                          continuation-timer-length
                                          ; Private
                                          instances
                                          next-instance-id))
(define (create-timeout-manager
         instance-expiration-handler
         instance-timer-length
         continuation-timer-length)
  ;; Instances
  (define instances (make-hasheq))
  (define next-instance-id (make-counter))    
  
  (define-struct instance (k-table timer))  
  (define (create-instance expire-fn)
    (define instance-id (next-instance-id))
    (hash-set! instances
               instance-id
               (make-instance (create-k-table)
                              (start-timer instance-timer-length
                                           (lambda ()
                                             (expire-fn)
                                             (hash-remove! instances instance-id)))))
    instance-id)
  (define (adjust-timeout! instance-id secs)
    (reset-timer! (instance-timer (instance-lookup instance-id #f))
                  secs))
  
  (define (instance-lookup instance-id peek?)
    (define instance
      (hash-ref instances instance-id
                (lambda ()
                  (raise (make-exn:fail:servlet-manager:no-instance
                          (format "No instance for id: ~a" instance-id)
                          (current-continuation-marks)
                          instance-expiration-handler)))))
    (unless peek?
      (increment-timer! (instance-timer instance)
                        instance-timer-length))
    instance)
  
  ;; Continuation table
  (define-struct k-table (next-id-fn htable))
  (define (create-k-table)
    (make-k-table (make-counter) (make-hasheq)))
  
  ;; Interface    
  (define (clear-continuations! instance-id)
    (match (instance-lookup instance-id #f)
      [(struct instance ((and k-table (struct k-table (next-id-fn htable))) instance-timer))
       (hash-for-each
        htable
        (match-lambda*
          [(list k-id (list salt k expiration-handler k-timer))
           (hash-set! htable k-id
                      (list salt #f expiration-handler k-timer))]))]))
  
  (define (continuation-store! instance-id k expiration-handler)
    (match (instance-lookup instance-id #t)
      [(struct instance ((struct k-table (next-id-fn htable)) instance-timer))
       (define k-id (next-id-fn))
       (define salt (random 100000000))
       (hash-set! htable
                  k-id
                  (list salt k expiration-handler
                        (start-timer continuation-timer-length
                                     (lambda ()
                                       (hash-set! htable k-id
                                                  (list salt #f expiration-handler
                                                        (start-timer 0 void)))))))
       (list k-id salt)]))
  (define (continuation-lookup* instance-id a-k-id a-salt peek?)
    (match (instance-lookup instance-id peek?)
      [(struct instance ((struct k-table (next-id-fn htable)) instance-timer))
       (match
           (hash-ref htable a-k-id
                     (lambda ()
                       (raise (make-exn:fail:servlet-manager:no-continuation
                               (format "No continuation for id: ~a" a-k-id)
                               (current-continuation-marks)
                               instance-expiration-handler))))
         [(list salt k expiration-handler k-timer)
          (unless peek?
            (increment-timer! k-timer
                              continuation-timer-length))
          (if (or (not (eq? salt a-salt))
                  (not k)
                  (and (custodian-box? k)
                       (not (custodian-box-value k))))
              (raise (make-exn:fail:servlet-manager:no-continuation
                      (format "No continuation for id: ~a" a-k-id)
                      (current-continuation-marks)
                      (if expiration-handler
                          expiration-handler
                          instance-expiration-handler)))
              k)])]))
  (define (continuation-lookup instance-id a-k-id a-salt)
    (continuation-lookup* instance-id a-k-id a-salt #f))
  (define (continuation-peek instance-id a-k-id a-salt)
    (continuation-lookup* instance-id a-k-id a-salt #t))
  
  (make-timeout-manager create-instance 
                        adjust-timeout!
                        clear-continuations!
                        continuation-store!
                        continuation-lookup
                        continuation-peek
                        ; Specific
                        instance-expiration-handler
                        instance-timer-length
                        continuation-timer-length
                        ; Private
                        instances
                        next-instance-id))
