#lang racket/base
(require racket/match
         racket/contract)
(require "manager.rkt"
         web-server/http
         web-server/servlet/servlet-structs)
(provide/contract
 [create-LRU-manager 
  (->
   (or/c false/c
         (request? . -> . can-be-response?))
   number? number? (-> boolean?)
   #:initial-count number?
   #:inform-p (number? . -> . void)
   manager?)]
 [make-threshold-LRU-manager 
  (-> 
   (or/c false/c
         (request? . -> . can-be-response?))
   number?
   manager?)])

;; Utility
(define (make-counter)
  (define i 0)
  (lambda ()
    (set! i (add1 i))
    i))

; Copied from InstaServlet, which copied from Continue (I believe?)
(define (make-threshold-LRU-manager instance-expiration-handler
                                    memory-threshold)
  (create-LRU-manager
   ;; Called when an instance has expired.
   instance-expiration-handler
   ;; The condition below is checked every 5 seconds
   5
   ;; One 'life point' is deducted every 10 minutes
   (* 10 60)
   ;; If this condition is true a 'life point' is deducted
   ;; from the continuation
   (lambda ()
     (define memory-use (current-memory-use))
     (define collect?   (or (>= memory-use memory-threshold)
                            (< memory-use 0)))
     collect?)
   ;; The number of 'life points' an continuation starts with
   #:initial-count 24
   ;; Logging done whenever an continuation is collected
   #:inform-p (lambda args
                (void))))

(define-struct (LRU-manager manager) (instance-expiration-handler
                                      ; Private
                                      instances
                                      next-instance-id))
(define (create-LRU-manager
         instance-expiration-handler
         check-interval collect-interval
         collect?
         #:initial-count [initial-count 1]
         #:inform-p [inform-p (lambda _ (void))])
  (define lock (make-semaphore 1))
  ;; Instances
  (define instances (make-hasheq))
  (define next-instance-id (make-counter))    
  
  (define-struct instance (k-table))  
  (define (create-instance expire-fn)
    (define instance-id (next-instance-id))
    (hash-set! instances
               instance-id
               (make-instance (create-k-table)))
    instance-id)
  (define (adjust-timeout! instance-id secs)
    (void))
  
  (define (instance-lookup instance-id)
    (define instance
      (hash-ref instances instance-id
                (lambda ()
                  (raise (make-exn:fail:servlet-manager:no-instance
                          (format "No instance for id: ~a" instance-id)
                          (current-continuation-marks)
                          instance-expiration-handler)))))
    instance)
  
  ;; Continuation table
  (define-struct k-table (next-id-fn htable))
  (define (create-k-table)
    (make-k-table (make-counter) (make-hasheq)))
  
  ;; Interface
  (define (clear-continuations! instance-id)
    (match (instance-lookup instance-id)
      [(struct instance ((and k-table (struct k-table (next-id-fn htable)))))
       (hash-for-each
        htable
        (match-lambda*
          [(list k-id (list salt k expiration-handler count))
           (hash-set! htable k-id
                      (list salt #f expiration-handler count))]))]))
  
  (define (continuation-store! instance-id k expiration-handler)
    (match (instance-lookup instance-id)
      [(struct instance ((struct k-table (next-id-fn htable))))
       (define k-id (next-id-fn))
       (define salt (random 100000000))
       (hash-set! htable
                  k-id
                  (list salt k expiration-handler initial-count))
       (list k-id salt)]))
  (define (continuation-lookup* instance-id a-k-id a-salt peek?)
    (match (instance-lookup instance-id)
      [(struct instance ((struct k-table (next-id-fn htable))))
       (match
           (hash-ref htable a-k-id
                     (lambda ()
                       (raise (make-exn:fail:servlet-manager:no-continuation
                               (format "No continuation for id: ~a" a-k-id)
                               (current-continuation-marks)
                               instance-expiration-handler))))
         [(list salt k expiration-handler count)
          (unless peek?
            (hash-set! htable a-k-id
                       (list salt k expiration-handler (add1 count))))
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
  
  (define (wrap f)
    (lambda args
      (call-with-semaphore lock (lambda () (apply f args)))))
  
  (define the-manager
    (make-LRU-manager (wrap create-instance)
                      adjust-timeout!
                      (wrap clear-continuations!)
                      (wrap continuation-store!)
                      (wrap continuation-lookup)
                      (wrap continuation-peek)
                      ; Specific
                      instance-expiration-handler
                      ; Private
                      instances
                      next-instance-id))
  
  ; Collector
  (define (collect just-go?)
    (call-with-semaphore
     lock
     (lambda ()
       (define removed (box 0))
       (hash-for-each
        instances
        (match-lambda*
          [(list instance-id (struct instance ((struct k-table (next-id-fn htable)))))
           (define empty? (box #t))
           (hash-for-each
            htable
            (match-lambda*
              [(list k-id (list s k eh count))
               (if (zero? count)
                   (begin (set-box! removed (add1 (unbox removed)))
                          (hash-remove! htable k-id))
                   (begin (set-box! empty? #f)
                          (hash-set! htable k-id
                                     (list s k eh (sub1 count)))))]))
           (when (unbox empty?)
             (set-box! removed (add1 (unbox removed)))
             (hash-remove! instances instance-id))]))
       (when (or just-go?
                 (not (zero? (unbox removed))))
         (inform-p (unbox removed))
         (collect-garbage)
         (collect-garbage)))))
  
  (define manager-thread
    (thread
     (lambda ()
       (define (seconds->msecs s)
         (+ (current-inexact-milliseconds)
            (* s 1000)))
       (let loop ([msecs0 (seconds->msecs check-interval)]
                  [msecs1 (seconds->msecs collect-interval)])
         (sync (handle-evt
                (alarm-evt msecs0)
                (lambda _
                  (when (collect?)
                    (collect #f))
                  (loop (seconds->msecs check-interval) msecs1)))
               (handle-evt
                (alarm-evt msecs1)
                (lambda _
                  (collect #t)
                  (loop msecs0 (seconds->msecs collect-interval)))))))))
  
  the-manager)
