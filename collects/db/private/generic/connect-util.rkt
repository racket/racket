#lang racket/base
(require racket/class
         "interfaces.rkt")
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
    (init-field (other-evt (lambda () never-evt)))
    (super-new)

    (define req-channel (make-channel))
    (define mthread
      (thread/suspend-to-kill
       (lambda ()
         (let loop ()
           (sync (wrap-evt req-channel (lambda (p) (p)))
                 (other-evt))
           (loop)))))

    (define/public (call proc)
      (thread-resume mthread (current-thread))
      (let ([result #f]
            [sema (make-semaphore 0)])
        (channel-put req-channel
                     (lambda ()
                       (set! result
                             (with-handlers ([(lambda (e) #t)
                                              (lambda (e) (cons 'exn e))])
                               (cons 'values (call-with-values proc list))))
                       (semaphore-post sema)))
        (semaphore-wait sema)
        (case (car result)
          ((values) (apply values (cdr result)))
          ((exn) (raise (cdr result))))))))

;; ----

;; Kill-safe wrapper

;; Note: wrapper protects against kill-thread, but not from
;; custodian-shutdown of ports, etc.

(define kill-safe-connection%
  (class* object% (connection<%>)
    (init-private connection)

    (define mgr (new manager%))

    (define-syntax-rule (define-forward (method arg ...) ...)
      (begin
        (define/public (method arg ...)
          (send mgr call (lambda () (send connection method arg ...)))) ...))

    (define-forward
      (connected?)
      (disconnect)
      (get-dbsystem)
      (query fsym stmt)
      (prepare fsym stmt close-on-exec?)
      (get-base)
      (free-statement stmt)
      (transaction-status fsym)
      (start-transaction fsym isolation cwt?)
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
                  get-key       ;; called from client thread
                  timeout)
    (super-new)

    (define custodian (current-custodian))

    ;; == methods called in manager thread ==

    ;; key=>conn : hasheq[key => connection]
    (define key=>conn (make-hasheq))

    ;; alarms : hasheq[connection => evt] (alarm wrapped to return key)
    (define alarms (make-hasheq))

    (define/private (get key) ;; also refreshes alarm
      (let ([c (hash-ref key=>conn key #f)])
        (when c (hash-set! alarms c (fresh-alarm-for key)))
        c))

    (define/private (put! key c)
      (hash-set! key=>conn key c)
      (hash-set! alarms c (fresh-alarm-for key)))

    (define/private (fresh-alarm-for key)
      (wrap-evt (alarm-evt (+ (current-inexact-milliseconds) timeout))
                (lambda (a) key)))

    (define/private (remove! key timeout?)
      ;; timeout? = if connection open, then wait longer
      (let* ([c (hash-ref key=>conn key #f)]
             [in-trans? (with-handlers ([exn:fail? (lambda (e) #f)])
                          (and c (send c transaction-status 'virtual-connection)))])
        (cond [(not c) (void)]
              [(and timeout? in-trans?)
               (hash-set! alarms c (fresh-alarm-for key))]
              [else
               (hash-remove! key=>conn key)
               (hash-remove! alarms c)
               (send c disconnect)])))

    (define mgr
      (new manager%
           (other-evt
            (lambda ()
              (choice-evt
               (let ([keys (hash-map key=>conn (lambda (k v) k))])
                 (handle-evt (apply choice-evt keys)
                             ;; Assignment to key has expired: move to idle or disconnect.
                             (lambda (key)
                               (dbdebug "virtual-connection: key expiration: ~e" key)
                               (remove! key #f))))
               (let ([alarm-evts (hash-map alarms (lambda (k v) v))])
                 (handle-evt (apply choice-evt alarm-evts)
                             ;; Disconnect idle connection.
                             (lambda (key)
                               (dbdebug "virtual-connection: timeout")
                               (remove! key #t)))))))))

    ;; == methods called in client thread ==

    (define/private (get-connection create?)
      (let* ([key (get-key)]
             [c (send mgr call (lambda () (get key)))])
        (cond [(and c (send c connected?)) c]
              [create?
               (let ([c* (parameterize ((current-custodian custodian))
                           (connector))])
                 (send mgr call
                       (lambda ()
                         (when c (remove! key #f))
                         (put! key c*)))
                 c*)]
              [else
               (when c ;; got a disconnected connection
                 (send mgr call (lambda () (remove! key #f))))
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
      (#f #f     (connected?))
      (#t '_     (get-dbsystem))
      (#t '_     (query fsym stmt))
      (#t '_     (start-transaction fsym isolation cwt?))
      (#f (void) (end-transaction fsym mode cwt?))
      (#f #f     (transaction-status fsym))
      (#t '_     (list-tables fsym schema)))

    (define/public (get-base)
      (get-connection #t))

    (define/public (disconnect)
      (let ([c (get-connection #f)]
            [key (get-key)])
        (when c
          (send c disconnect)
          (send mgr call (lambda () (remove! key #f)))))
      (void))

    (define/public (prepare fsym stmt close-on-exec?)
      ;; FIXME: hacky way of supporting virtual-statement
      (unless (or close-on-exec? (eq? fsym 'virtual-statement))
        (error fsym "cannot prepare statement with virtual connection"))
      (send (get-connection #t) prepare fsym stmt close-on-exec?))

    (define/public (free-statement stmt)
      (error 'free-statement
             "internal error: virtual connection does not own statements"))))

;; ----

(define (virtual-connection connector
                            #:timeout [timeout +inf.0])
  (let ([connector
         (cond [(connection-pool? connector)
                (lambda () (connection-pool-lease connector))]
               [else connector])]
        [get-key (lambda () (thread-dead-evt (current-thread)))])
    (new virtual-connection%
         (connector connector)
         (get-key (lambda () (thread-dead-evt (current-thread))))
         (timeout (* 1000 timeout)))))

;; ========================================

;; Connection pool

(define connection-pool%
  (class* object% ()
    (init-private connector              ;; called from manager thread
                  max-connections
                  max-idle-connections)
    (super-new)

    ;; max-connections is either in [1, 10000] or +inf.0,
    ;; if leave-evt is sema, then counter = (max-connections - assigned connections)
    ;; ie, includes idle connections
    (define lease-evt
      (if (= max-connections +inf.0)
          always-evt
          (make-semaphore max-connections)))

    (define proxy-counter 1) ;; for debugging
    (define actual-counter 1) ;; for debugging
    (define actual=>number (make-weak-hasheq))

    ;; == methods called in manager thread ==

    ;; proxy=>evt : hasheq[proxy-connection => evt]
    (define proxy=>evt (make-hasheq))

    ;; idle-list : (listof raw-connection)
    (define idle-list null)

    (define/private (lease* key)
      (let* ([take-idle? (pair? idle-list)]
             [raw-c
              (cond [take-idle?
                     (begin0 (car idle-list)
                       (set! idle-list (cdr idle-list)))]
                    [else (new-connection)])]
             [proxy-number (begin0 proxy-counter (set! proxy-counter (add1 proxy-counter)))]
             [c (new proxy-connection% (pool this) (connection raw-c) (number proxy-number))])
        (dbdebug "connection-pool: leasing connection #~a (~a @~a)"
                 proxy-number
                 (if take-idle? "idle" "new")
                 (hash-ref actual=>number raw-c "???"))
        (hash-set! proxy=>evt c key)
        c))

    (define/private (release* proxy raw-c why)
      (dbdebug "connection-pool: releasing connection #~a (~a, ~a)"
               (send proxy get-number)
               (cond [(not raw-c) "no-op"]
                     [(< (length idle-list) max-idle-connections) "idle"]
                     [else "disconnect"])
               why)
      (hash-remove! proxy=>evt proxy)
      (when raw-c
        (with-handlers ([exn:fail? void])
          (send raw-c end-transaction 'connection-pool 'rollback))
        (cond [(< (length idle-list) max-idle-connections)
               (set! idle-list (cons raw-c idle-list))]
              [else (send raw-c disconnect)])
        (when (semaphore? lease-evt) (semaphore-post lease-evt))))

    (define/private (new-connection)
      (let ([c (connector)]
            [actual-number
             (begin0 actual-counter
               (set! actual-counter (add1 actual-counter)))])
        (when (or (hash-ref proxy=>evt c #f) (memq c idle-list))
          (uerror 'connection-pool "connect function did not produce a fresh connection"))
        (hash-set! actual=>number c actual-number)
        c))

    (define mgr
      (new manager%
           (other-evt
            (lambda ()
              (let ([evts (hash-map proxy=>evt (lambda (k v) (wrap-evt v (lambda (e) k))))])
                (handle-evt (apply choice-evt evts)
                            (lambda (proxy)
                              (release* proxy
                                        (send proxy release-connection)
                                        "release-evt"))))))))

    ;; == methods called in client thread ==

    (define/public (lease key)
      (wrap-evt lease-evt
                (lambda (_e)
                  (send mgr call (lambda () (lease* key))))))

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
      (query fsym stmt)
      (prepare fsym stmt close-on-exec?)
      (get-base)
      (free-statement stmt)
      (transaction-status fsym)
      (start-transaction fsym isolation cwt?)
      (end-transaction fsym mode cwt?)
      (list-tables fsym schema))

    ;; (define-forward define/override (connected?))
    (define/override (connected?) (and connection #t))

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
         [result (sync/timeout 0.1 (send pool lease key))])
    (unless result
      (uerror 'connection-pool-lease
              "cannot obtain connection; connection pool limit reached"))
    result))
