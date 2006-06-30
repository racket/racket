(module lru mzscheme
  (require (lib "plt-match.ss")
           (lib "contract.ss")
           (lib "kw.ss"))
  (require "manager.ss"
           "../servlet-structs.ss")
  (provide/contract
   ; XXX contract kw
   [create-LRU-manager ((expiration-handler? number? number? (-> boolean?)) any/c . ->* . (manager?))])
  
  ;; Utility
  (define (make-counter)
    (define i 0)
    (lambda ()
      (set! i (add1 i))
      i))
  
  (define-struct (LRU-manager manager) (instance-expiration-handler
                                        ; Private
                                        instances
                                        next-instance-id))
  (define/kw (create-LRU-manager
              instance-expiration-handler
              time0 time1
              collect?
              #:key 
              [initial-count 1]
              [inform-p (lambda _ (void))])
    ;; Instances
    (define instances (make-hash-table))
    (define next-instance-id (make-counter))    
    
    (define-struct instance (data k-table))  
    (define (create-instance data expire-fn)
      (define instance-id (next-instance-id))
      (hash-table-put! instances
                       instance-id
                       (make-instance data (create-k-table)))
      instance-id)
    (define (adjust-timeout! instance-id secs)
      (void))
    
    (define (instance-lookup instance-id)
      (define instance
        (hash-table-get instances instance-id
                        (lambda ()
                          (raise (make-exn:fail:servlet-manager:no-instance
                                  (string->immutable-string
                                   (format "No instance for id: ~a" instance-id))
                                  (current-continuation-marks)
                                  instance-expiration-handler)))))
      instance)
    
    ;; Continuation table
    (define-struct k-table (next-id-fn htable))
    (define (create-k-table)
      (make-k-table (make-counter) (make-hash-table)))
    
    ;; Interface
    (define (instance-lookup-data instance-id)
      (instance-data (instance-lookup instance-id)))
    
    (define (clear-continuations! instance-id)
      (match (instance-lookup instance-id)
        [(struct instance (data (and k-table (struct k-table (next-id-fn htable)))))
         (hash-table-for-each
          htable
          (match-lambda*
            [(list k-id (list salt k expiration-handler count))
             (hash-table-put! htable k-id
                              (list salt #f expiration-handler count))]))]))
    
    (define (continuation-store! instance-id k expiration-handler)
      (match (instance-lookup instance-id)
        [(struct instance (data (struct k-table (next-id-fn htable))))
         (define k-id (next-id-fn))
         (define salt (random 100000000))
         (hash-table-put! htable
                          k-id
                          (list salt k expiration-handler initial-count))
         (list k-id salt)]))
    (define (continuation-lookup instance-id a-k-id a-salt)
      (match (instance-lookup instance-id)
        [(struct instance (data (struct k-table (next-id-fn htable))))
         (match
             (hash-table-get htable a-k-id
                             (lambda ()
                               (raise (make-exn:fail:servlet-manager:no-continuation
                                       (string->immutable-string 
                                        (format "No continuation for id: ~a" a-k-id))
                                       (current-continuation-marks)
                                       instance-expiration-handler))))
           [(list salt k expiration-handler count)
            (hash-table-put! htable a-k-id
                             (list salt k expiration-handler (add1 count)))
            (if (or (not (eq? salt a-salt))
                    (not k))
                (raise (make-exn:fail:servlet-manager:no-continuation
                        (string->immutable-string
                         (format "No continuation for id: ~a" a-k-id))
                        (current-continuation-marks)
                        (if expiration-handler
                            expiration-handler
                            instance-expiration-handler)))
                k)])]))
    
    (define the-manager
      (make-LRU-manager create-instance 
                        adjust-timeout!
                        instance-lookup-data
                        clear-continuations!
                        continuation-store!
                        continuation-lookup
                        ; Specific
                        instance-expiration-handler
                        ; Private
                        instances
                        next-instance-id))
    
    ; Collector
    (define (collect)
      (define removed (box 0))
      (when (collect?)
        (hash-table-for-each
         instances
         (match-lambda*
           [(list instance-id (struct instance (_ (struct k-table (next-id-fn htable)))))
            (define empty? (box #t))
            (hash-table-for-each
             htable
             (match-lambda*
               [(list k-id (list s k eh count))
                (if (zero? count)
                    (begin (set-box! removed (add1 (unbox removed)))
                           (hash-table-remove! htable k-id))
                    (begin (set-box! empty? #f)
                           (hash-table-put! htable k-id
                                            (list s k eh (sub1 count)))))]))
            (when (unbox empty?)
              (set-box! removed (add1 (unbox removed)))
              (hash-table-remove! instances instance-id))])))
      (unless (zero? (unbox removed))
        (inform-p (unbox removed))
        (collect-garbage)
        (collect-garbage)))
    (define manager-thread
      (thread
       (lambda ()
         (define (seconds->msecs s)
           (+ (current-inexact-milliseconds)
              (* s 1000)))
         (let loop ([msecs0 (seconds->msecs time0)]
                    [msecs1 (seconds->msecs time1)])
           (sync (handle-evt
                  (alarm-evt msecs0)
                  (lambda _
                    (when (collect?)
                      (collect))
                    (loop (seconds->msecs time0) msecs1)))
                 (handle-evt
                  (alarm-evt msecs1)
                  (lambda _
                    (collect)
                    (loop msecs0 (seconds->msecs time1)))))))))
    
    the-manager))