(module timeouts mzscheme
  (require (lib "class.ss")
           (lib "plt-match.ss"))
  (require "manager.ss")
  (require "../timer.ss")
  (provide timeout-manager%)
  
  ;; Utility
  (define (make-counter)
    (let ([i 0])
      (lambda ()
        (set! i (add1 i))
        i)))
  
  (define timeout-manager%
    (class* object% (manager<%>)
      (init-field instance-expiration-handler
                  instance-timer-length
                  continuation-timer-length)
      (public create-instance
              adjust-timeout!
	      instance-lookup-data
	      clear-continuations!
	      continuation-store!
	      continuation-lookup)
      
      ;; Instances
      (define instances (make-hash-table))
      (define next-instance-id (make-counter))    
      
      (define-struct instance (data k-table timer))  
      (define (create-instance data expire-fn)
        (define instance-id (next-instance-id))
        (hash-table-put! instances
                         instance-id
                         (make-instance data
                                        (create-k-table)
                                        (start-timer instance-timer-length
                                                     (lambda ()
                                                       (expire-fn)
                                                       (hash-table-remove! instances instance-id)))))
        instance-id)
      (define (adjust-timeout! instance-id secs)
        (reset-timer! (instance-timer (instance-lookup instance-id))
                      secs))
      
      (define (instance-lookup instance-id)
        (define instance
          (hash-table-get instances instance-id
                          (lambda ()
                            (raise (make-exn:fail:servlet-manager:no-instance
                                    (format "No instance for id: ~a" instance-id)
                                    (current-continuation-marks)
                                    instance-expiration-handler)))))
        (increment-timer! (instance-timer instance)
                          instance-timer-length)
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
          [(struct instance (data (and k-table (struct k-table (next-id-fn htable))) instance-timer))
           (hash-table-for-each
            htable
            (match-lambda*
              [(list k-id (list salt k expiration-handler k-timer))
               (hash-table-put! htable k-id
                                (list salt #f expiration-handler k-timer))]))]))
      
      (define (continuation-store! instance-id k expiration-handler)
        (match (instance-lookup instance-id)
          [(struct instance (data (struct k-table (next-id-fn htable)) instance-timer))
           (define k-id (next-id-fn))
           (define salt (random 100000000))
           (hash-table-put! htable
                            k-id
                            (list salt k expiration-handler
                                  (start-timer continuation-timer-length
                                               (lambda ()
                                                 (hash-table-put! htable k-id
                                                                  (list salt #f expiration-handler
                                                                        (start-timer 0 void)))))))
           (list k-id salt)]))
      (define (continuation-lookup instance-id a-k-id a-salt)
        (match (instance-lookup instance-id)
          [(struct instance (data (struct k-table (next-id-fn htable)) instance-timer))
           (match
               (hash-table-get htable a-k-id
                               (lambda ()
                                 (raise (make-exn:fail:servlet-manager:no-continuation
                                         (format "No continuation for id: ~a" a-k-id)
                                         (current-continuation-marks)
                                         instance-expiration-handler))))
             [(list salt k expiration-handler k-timer)
              (increment-timer! k-timer
                                continuation-timer-length)
              (if (or (not (eq? salt a-salt))
                      (not k))
                  (raise (make-exn:fail:servlet-manager:no-continuation
                          (format "No continuation for id: ~a" a-k-id)
                          (current-continuation-marks)
                          expiration-handler))
                  k)])]))
      
      (super-new))))
