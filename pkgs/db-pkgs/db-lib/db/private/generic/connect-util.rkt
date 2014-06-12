#lang racket/base
(require racket/class
         db/private/generic/interfaces
         db/private/generic/common)
(provide kill-safe-connection
         virtual-connection
         connection-pool
         connection-pool?
         connection-pool-lease)

;; manager% implements kill-safe manager thread w/ request channel
(define manager%
  (class object%
    ;; other-evt : (-> evt)
    ;; generates other evt to sync on besides req-channel, eg timeouts
    (init-field (other-evt (lambda () never-evt))
                (alt-enabled? (lambda () #t)))
    (super-new)

    (define req-channel (make-channel))
    (define alt-req-channel (make-channel))

    (define mthread
      (thread/suspend-to-kill
       (lambda ()
         (let loop ()
           (sync (wrap-evt req-channel (lambda (p) (p)))
                 (if (alt-enabled?)
                     (wrap-evt alt-req-channel (lambda (p) (p)))
                     never-evt)
                 (other-evt))
           (loop)))))

    (define/public (call proc)
      (call* proc req-channel #f))
    (define/public (alt-call-evt proc)
      (call* proc alt-req-channel #t))

    (define/private (call* proc chan as-evt?)
      (thread-resume mthread (current-thread))
      (let* ([result #f]
             [sema (make-semaphore 0)]
             [proc (lambda ()
                     (set! result
                           (with-handlers ([(lambda (e) #t)
                                            (lambda (e) (cons 'exn e))])
                             (cons 'values (call-with-values proc list))))
                     (semaphore-post sema))]
             [handler
              (lambda (_evt)
                (semaphore-wait sema)
                (case (car result)
                  ((values) (apply values (cdr result)))
                  ((exn) (raise (cdr result)))))])
        (if as-evt?
            (wrap-evt (channel-put-evt chan proc) handler)
            (begin (channel-put chan proc)
                   (handler #f)))))))

;; ----

;; Kill-safe wrapper

;; Note: wrapper protects against kill-thread, but not from
;; custodian-shutdown of ports, etc.

(define kill-safe-connection%
  (class* object% (connection<%>)
    (init-private connection)

    (define mgr (new manager%))
    (define last-connected? #t)

    (define-syntax-rule (define-forward (method arg ...) ...)
      (begin
        (define/public (method arg ...)
          (send mgr call (lambda ()
                           (begin0
                               (send connection method arg ...)
                             (set! last-connected? (send connection connected?))))))
        ...))

    (define/public (connected?) last-connected?)

    (define-forward
      (disconnect)
      (get-dbsystem)
      (query fsym stmt cursor?)
      (prepare fsym stmt close-on-exec?)
      (fetch/cursor fsym cursor fetch-size)
      (get-base)
      (free-statement stmt need-lock?)
      (transaction-status fsym)
      (start-transaction fsym isolation option cwt?)
      (end-transaction fsym mode cwt?)
      (list-tables fsym schema))

    (super-new)))

;; ----

(define (kill-safe-connection connection)
  (new kill-safe-connection%
       (connection connection)))

;; ========================================

;; Virtual connection

(define virtual-connection%
  (class* object% (connection<%>)
    (init-private connector     ;; called from client thread
                  get-key)      ;; called from client thread
    (super-new)

    (define custodian (current-custodian))

    ;; == methods called in manager thread ==

    ;; key=>conn : hasheq[key => connection]
    (define key=>conn (make-hasheq))

    (define/private (get key)      ;; also called by client thread for connected?
      (hash-ref key=>conn key #f))

    (define/private (put! key c)
      (hash-set! key=>conn key c))

    (define/private (remove! key)
      (let ([c (get key)])
        (when c
          (hash-remove! key=>conn key)
          (send c disconnect))))

    (define mgr
      (new manager%
           (other-evt
            (lambda ()
              (choice-evt
               (let ([keys (hash-map key=>conn (lambda (k v) k))])
                 (handle-evt (apply choice-evt keys)
                             ;; Assignment to key has expired
                             (lambda (key)
                               (log-db-debug "virtual-connection: key expiration: ~e" key)
                               (remove! key)))))))))

    ;; == methods called in client thread ==

    (define/private (get-connection create?)
      (let* ([key (get-key)]
             [c (send mgr call (lambda () (get key)))])
        (cond [(and c (send c connected?)) c]
              [create?
               (log-db-debug
                (if c
                    "virtual-connection: refreshing connection (old is disconnected)"
                    "virtual-connection: creating new connection"))
               (let ([c* (parameterize ((current-custodian custodian))
                           (connector))])
                 (send mgr call
                       (lambda ()
                         (when c (remove! key))
                         (put! key c*)))
                 c*)]
              [else
               (when c ;; got a disconnected connection
                 (send mgr call (lambda () (remove! key))))
               #f])))

    ;; ----

    (define-syntax-rule (define-forward (req-con? no-con (method arg ...)) ...)
      (begin (define/public (method arg ...)
               (let ([c (get-connection req-con?)])
                 (if c
                     (send c method arg ...)
                     no-con)))
             ...))

    (define-forward
      (#t '_     (get-dbsystem))
      (#t '_     (query fsym stmt cursor?))
      (#t '_     (fetch/cursor fsym stmt fetch-size))
      (#t '_     (start-transaction fsym isolation option cwt?))
      (#f (void) (end-transaction fsym mode cwt?))
      (#f #f     (transaction-status fsym))
      (#t '_     (list-tables fsym schema)))

    (define/public (get-base)
      (get-connection #t))

    (define/public (connected?)
      (let ([c (get (get-key))])
        (and c (send c connected?))))

    (define/public (disconnect)
      (let ([c (get-connection #f)]
            [key (get-key)])
        (when c
          (send c disconnect)
          (send mgr call (lambda () (remove! key)))))
      (void))

    (define/public (prepare fsym stmt close-on-exec?)
      ;; FIXME: hacky way of supporting virtual-statement
      (unless (or close-on-exec? (eq? fsym 'virtual-statement))
        (error fsym "cannot prepare statement with virtual connection"))
      (send (get-connection #t) prepare fsym stmt close-on-exec?))

    (define/public (free-statement stmt need-lock?)
      (error/internal 'free-statement "virtual connection does not own statements"))))

;; ----

(define (virtual-connection connector)
  (let ([connector
         (cond [(connection-pool? connector)
                (lambda () (connection-pool-lease connector))]
               [else connector])]
        [get-key (lambda () (thread-dead-evt (current-thread)))])
    (new virtual-connection%
         (connector connector)
         (get-key get-key))))

;; ========================================

;; Connection pool

(define connection-pool%
  (class* object% ()
    (init-private connector              ;; called from manager thread
                  max-connections
                  max-idle-connections)
    (super-new)

    (define proxy-counter 1) ;; for debugging
    (define actual-counter 1) ;; for debugging
    (define actual=>number (make-weak-hasheq))

    ;; == methods called in manager thread ==

    ;; assigned-connections : nat
    (define assigned-connections 0)

    ;; proxy=>evt : hasheq[proxy-connection => evt]
    (define proxy=>evt (make-hasheq))

    ;; idle-list : (listof raw-connection)
    (define idle-list null)

    (define/private (lease* key)
      (let* ([reused-c (try-take-idle)]
             [raw-c (or reused-c (new-connection))]
             [proxy-number (begin0 proxy-counter (set! proxy-counter (add1 proxy-counter)))]
             [c (new proxy-connection% (pool this) (connection raw-c) (number proxy-number))])
        (log-db-debug "connection-pool: leasing connection #~a (~a @~a)"
                      proxy-number
                      (if reused-c "idle" "new")
                      (hash-ref actual=>number raw-c "???"))
        (hash-set! proxy=>evt c (wrap-evt key (lambda (_e) c)))
        (set! assigned-connections (add1 assigned-connections))
        c))

    (define/private (try-take-idle)
      (and (pair? idle-list)
           (let ([c (car idle-list)])
             (set! idle-list (cdr idle-list))
             (if (send c connected?)
                 c
                 (try-take-idle)))))

    (define/private (release* proxy raw-c why)
      (log-db-debug "connection-pool: releasing connection #~a (~a, ~a)"
                    (send proxy get-number)
                    (cond [(not raw-c) "no-op"]
                          [(< (length idle-list) max-idle-connections) "idle"]
                          [else "disconnect"])
                    why)
      (hash-remove! proxy=>evt proxy)
      (when raw-c
        (with-handlers ([exn:fail? void])
          ;; If in tx, just disconnect (for simplicity; else must loop for nested txs)
          (when (send raw-c transaction-status 'connection-pool)
            (send raw-c disconnect)))
        (cond [(and (< (length idle-list) max-idle-connections)
                    (send raw-c connected?))
               (set! idle-list (cons raw-c idle-list))]
              [else (send raw-c disconnect)])
        (set! assigned-connections (sub1 assigned-connections))))

    (define/private (new-connection)
      (let ([c (connector)]
            [actual-number
             (begin0 actual-counter
               (set! actual-counter (add1 actual-counter)))])
        (when (or (hash-ref proxy=>evt c #f) (memq c idle-list))
          (error 'connection-pool "connect function did not produce a fresh connection"))
        (hash-set! actual=>number c actual-number)
        c))

    (define mgr
      (new manager%
           (other-evt
            (lambda ()
              (let ([evts (hash-values proxy=>evt)])
                (handle-evt (apply choice-evt evts)
                            (lambda (proxy)
                              (release* proxy
                                        (send proxy release-connection)
                                        "release-evt"))))))
           (alt-enabled? (lambda () (< assigned-connections max-connections)))))

    ;; == methods called in client thread ==

    (define/public (lease-evt key)
      (send mgr alt-call-evt (lambda () (lease* key))))

    (define/public (release proxy)
      (let ([raw-c (send proxy release-connection)])
        (send mgr call (lambda () (release* proxy raw-c "proxy disconnect"))))
      (void))))

;; --

(define proxy-connection%
  (class* locking% (connection<%>)
    (init-private connection
                  pool
                  number)
    (inherit call-with-lock)
    (super-new)

    (define-syntax-rule (define-forward defmethod (method arg ...) ...)
      (begin
        (defmethod (method arg ...)
          (call-with-lock 'method
            (lambda ()
              (let ([c connection])
                (unless c (error/not-connected 'method))
                (send c method arg ...)))))
        ...))

    (define-forward define/public
      (get-dbsystem)
      (query fsym stmt cursor?)
      (prepare fsym stmt close-on-exec?)
      (fetch/cursor fsym stmt fetch-size)
      (get-base)
      (free-statement stmt need-lock?)
      (transaction-status fsym)
      (start-transaction fsym isolation option cwt?)
      (end-transaction fsym mode cwt?)
      (list-tables fsym schema))

    (define/override (connected?) (and connection (send connection connected?)))

    (define/public (disconnect)
      (send pool release this))

    (define/public (get-number) number)

    (define/public (release-connection)
      (begin0 connection
        (set! connection #f)))))

;; ----

(define (connection-pool connector
                         #:max-connections [max-connections +inf.0]
                         #:max-idle-connections [max-idle-connections 10])
  (new connection-pool%
       (connector connector)
       (max-connections max-connections)
       (max-idle-connections max-idle-connections)))

(define (connection-pool? x)
  (is-a? x connection-pool%))

(define (connection-pool-lease pool [key (current-thread)])
  (let* ([key
          (cond [(thread? key) (thread-dead-evt key)]
                [(custodian? key) (make-custodian-box key #t)]
                [else key])]
         [result (sync/timeout 0.1 (send pool lease-evt key))])
    (unless result
      (error 'connection-pool-lease "connection pool limit reached"))
    result))
