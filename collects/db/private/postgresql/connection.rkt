#lang racket/base
(require racket/class
         racket/match
         racket/vector
         file/md5
         openssl
         "../generic/interfaces.rkt"
         "../generic/common.rkt"
         "../generic/sql-data.rkt"
         "../generic/prepared.rkt"
         "message.rkt"
         "dbsystem.rkt")
(provide connection%
         password-hash)

;; ========================================

;; connector<%>
;; Manages making connections
(define connector<%>
  (interface ()
    attach-to-ports              ;; input-port output-port -> void
    start-connection-protocol))  ;; string string string/#f -> void

(define connection-base%
  (class* transactions% (connection<%> connector<%>)
    (init-private notice-handler
                  notification-handler
                  allow-cleartext-password?)
    (define inport #f)
    (define outport #f)
    (define process-id #f)

    (inherit call-with-lock
             call-with-lock*
             add-delayed-call!
             get-tx-status
             set-tx-status!
             check-valid-tx-status
             check-statement/tx
             tx-state->string)

    (super-new)

    ;; with-disconnect-on-error
    (define-syntax-rule (with-disconnect-on-error . body)
      (with-handlers ([exn:fail? (lambda (e) (disconnect* #f) (raise e))])
        . body))

    ;; ========================================

    ;; == Debugging

    ;; Debugging
    (define DEBUG? #f)

    (define/public (debug debug?)
      (set! DEBUG? debug?))

    ;; ========================================

    ;; == Communication
    ;; (Must be called with lock acquired.)

    ;; raw-recv : -> message
    (define/private (raw-recv)
      (with-disconnect-on-error
       (let ([r (parse-server-message inport)])
         (when DEBUG?
           (fprintf (current-error-port) "  << ~s\n" r))
         r)))

    ;; recv-message : symbol -> message
    (define/private (recv-message fsym)
      (let ([r (raw-recv)])
        (cond [(ErrorResponse? r)
               (check-ready-for-query fsym #t)
               (raise-backend-error fsym r)]
              [(or (NoticeResponse? r)
                   (NotificationResponse? r)
                   (ParameterStatus? r))
               (handle-async-message fsym r)
               (recv-message fsym)]
              [else r])))

    ;; send-message : message -> void
    (define/private (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    ;; buffer-message : message -> void
    (define/private (buffer-message msg)
      (when DEBUG?
        (fprintf (current-error-port) "  >> ~s\n" msg))
      (with-disconnect-on-error
       (write-message msg outport)))

    ;; flush-message-buffer : -> void
    (define/private (flush-message-buffer)
      (with-disconnect-on-error
       (flush-output outport)))

    ;; check-ready-for-query : symbol -> void
    (define/private (check-ready-for-query fsym or-eof?)
      (let ([r (recv-message fsym)])
        (cond [(ReadyForQuery? r)
               ;; Update transaction status
               (set-tx-status! fsym
                               (case (ReadyForQuery-transaction-status r)
                                 ((idle) #f)
                                 ((transaction) #t)
                                 ((failed) 'invalid)))]
              [(and or-eof? (eof-object? r)) (void)]
              [else (error/comm fsym "expected ready")])))

    ;; == Asynchronous messages

    ;; handle-async-message : message -> void
    (define/private (handle-async-message fsym msg)
      (match msg
        [(struct NoticeResponse (properties))
         (let ([code (cdr (assq 'code properties))]
               [message (cdr (assq 'message properties))])
           (add-delayed-call! (lambda () (notice-handler code message))))]
        [(struct NotificationResponse (pid condition info))
         (add-delayed-call! (lambda () (notification-handler condition)))]
        [(struct ParameterStatus (name value))
         (cond [(equal? name "client_encoding")
                (unless (equal? value "UTF8")
                  (disconnect* #f)
                  (uerror fsym
                          (string-append
                           "server attempted to change the client character encoding "
                           "from UTF8 to ~a, disconnecting")
                          value))]
               [else (void)])]))

    ;; == Connection management

    ;; disconnect : [boolean] -> (void)
    (define/public (disconnect)
      (disconnect* #t))

    ;; disconnect* : boolean -> void
    (define/private (disconnect* no-lock-held?)
      (define (go politely?)
        (when DEBUG?
          (fprintf (current-error-port) "  ** Disconnecting\n"))
        (let ([outport* outport]
              [inport* inport])
          (when outport*
            (when politely?
              (send-message (make-Terminate)))
            (close-output-port outport*)
            (set! outport #f))
          (when inport*
            (close-input-port inport*)
            (set! inport #f))))
      ;; If we don't hold the lock, try to acquire it and disconnect politely.
      ;; Except, if already disconnected, no need to acquire lock.
      (cond [(and no-lock-held? (connected?))
             (call-with-lock* 'disconnect
               (lambda () (go #t))
               (lambda () (go #f))
               #f)]
            [else (go #f)]))

    ;; connected? : -> boolean
    (define/override (connected?)
      (let ([outport outport])
        (and outport (not (port-closed? outport)))))

    ;; == System

    (define/public (get-dbsystem)
      dbsystem)

    ;; ========================================

    ;; == Connect

    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! inport in)
      (set! outport out))

    ;; start-connection-protocol : string string string/#f -> void
    (define/public (start-connection-protocol dbname username password)
      (with-disconnect-on-error
       (call-with-lock 'postgresql-connect
        (lambda ()
          (send-message
           (make-StartupMessage
            (list (cons "user" username)
                  (cons "database" dbname)
                  (cons "client_encoding" "UTF8")
                  (cons "DateStyle" "ISO, MDY"))))
          (connect:expect-auth username password)))))

    ;; connect:expect-auth : string/#f -> ConnectionResult
    (define/private (connect:expect-auth username password)
      (let ([r (recv-message 'postgresql-connect)])
        (match r
          [(struct AuthenticationOk ())
           (connect:expect-ready-for-query)]
          [(struct AuthenticationCleartextPassword ())
           (unless (string? password)
             (error/need-password 'postgresql-connect))
           (unless allow-cleartext-password?
             (uerror 'postgresql-connect (nosupport "cleartext password")))
           (send-message (make-PasswordMessage password))
           (connect:expect-auth username password)]
          [(struct AuthenticationCryptPassword (salt))
           (uerror 'postgresql-connect (nosupport "crypt()-encrypted password"))]
          [(struct AuthenticationMD5Password (salt))
           (unless password
             (error/need-password 'postgresql-connect))
           (send-message (make-PasswordMessage (md5-password username password salt)))
           (connect:expect-auth username password)]
          [(struct AuthenticationKerberosV5 ())
           (uerror 'postgresql-connect (nosupport "KerberosV5 authentication"))]
          [(struct AuthenticationSCMCredential ())
           (uerror 'postgresql-connect (nosupport "SCM authentication"))]
          ;; ErrorResponse handled by recv-message
          [_ (error/comm 'postgresql-connect "during authentication")])))

    ;; connect:expect-ready-for-query : -> void
    (define/private (connect:expect-ready-for-query)
      (let ([r (recv-message 'postgresql-connect)])
        (match r
          [(struct ReadyForQuery (status))
           (void)]
          [(struct BackendKeyData (pid secret))
           (set! process-id pid)
           (connect:expect-ready-for-query)]
          [_
           (error/comm 'postgresql-connect "after authentication")])))

    ;; ============================================================

    ;; == Query

    ;; query : symbol Statement boolean -> QueryResult
    (define/public (query fsym stmt cursor?)
      (let ([result
             (call-with-lock fsym
               (lambda ()
                 (check-valid-tx-status fsym)
                 (let* ([stmt (check-statement fsym stmt cursor?)]
                        [pst (statement-binding-pst stmt)]
                        [stmt-type (send pst get-stmt-type)]
                        [close-on-exec? (and (not cursor?) (send pst get-close-on-exec?))])
                   (check-statement/tx fsym stmt-type)
                   (when cursor?
                     (unless (eq? (get-tx-status) #t)
                       (error fsym "cursor allowed only within transaction")))
                   (query1 fsym stmt close-on-exec? cursor?))))])
        (query1:process-result fsym result)))

    (define/private (query1 fsym stmt close-on-exec? cursor?)
      ;; if stmt is string, must take no params & results must be binary-readable
      (let ([portal (query1:enqueue stmt close-on-exec? cursor?)])
        (send-message (make-Sync))
        (begin0 (query1:collect fsym stmt portal (string? stmt) close-on-exec? cursor?)
          (check-ready-for-query fsym #f)
          (when DEBUG?
            (fprintf (current-error-port) "  ** ~a\n" (tx-state->string))))))

    ;; check-statement : symbol statement -> statement-binding
    ;; Convert to statement-binding; need to prepare to get type information, used to
    ;; choose result formats.
    ;; FIXME: if text format eliminated, can skip prepare
    ;; FIXME: can use classify-pg-sql to avoid preparing stmts with no results
    (define/private (check-statement fsym stmt cursor?)
      (cond [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)])
               (send pst check-owner fsym this stmt)
               stmt)]
            [(string? stmt)
             (let ([pst (prepare1 fsym stmt (not cursor?))])
               (send pst bind fsym null))]))

    ;; query1:enqueue : Statement boolean boolean -> string
    (define/private (query1:enqueue stmt close-on-exec? cursor?)
      (let ([portal (if cursor? (generate-name) "")])
        (cond [(statement-binding? stmt)
               (let* ([pst (statement-binding-pst stmt)]
                      [pst-name (send pst get-handle)]
                      [params (statement-binding-params stmt)])
                 (buffer-message (make-Bind portal pst-name
                                            (map typeid->format (send pst get-param-typeids))
                                            params
                                            (map typeid->format (send pst get-result-typeids)))))]
              [(string? stmt)
               (buffer-message (make-Parse "" stmt '()))
               (buffer-message (make-Bind portal "" '() '() '(1)))])
        (buffer-message (make-Describe 'portal portal))
        (unless cursor?
          (buffer-message (make-Execute portal 0))
          (buffer-message (make-Close 'portal portal))
          (when close-on-exec?
            (let ([pst (statement-binding-pst stmt)])
              (buffer-message (make-Close 'statement (send pst get-handle)))
              (send pst set-handle #f))))
        portal))

    (define/private (query1:collect fsym stmt portal simple? close-on-exec? cursor?)
      (when simple?
        (match (recv-message fsym)
          [(struct ParseComplete ()) (void)]
          [other-r (query1:error fsym other-r)]))
      (match (recv-message fsym)
        [(struct BindComplete ()) (void)]
        [other-r (query1:error fsym other-r)])
      (match (recv-message fsym)
        [(struct RowDescription (field-dvecs))
         (cond [cursor?
                (vector 'cursor field-dvecs stmt portal)]
               [else
                (let* ([rows (query1:data-loop fsym #f)])
                  (query1:expect-close-complete fsym close-on-exec?)
                  (vector 'rows field-dvecs rows))])]
        [(struct NoData ())
         (let* ([command (query1:expect-completion fsym)])
           (query1:expect-close-complete fsym close-on-exec?)
           (vector 'command command))]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:data-loop fsym end-box)
      (match (recv-message fsym)
        [(struct DataRow (row))
         (cons row (query1:data-loop fsym end-box))]
        [(struct CommandComplete (command))
         (when end-box (set-box! end-box #t))
         null]
        [(struct PortalSuspended ())
         null]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:expect-completion fsym)
      (match (recv-message fsym)
        [(struct CommandComplete (command)) `((command . ,command))]
        [(struct EmptyQueryResponse ()) '()]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:expect-close-complete fsym close-on-exec?)
      (match (recv-message fsym)
        [(struct CloseComplete ())
         (when close-on-exec? (query1:expect-close-complete fsym #f))]
        [other-r (query1:error fsym other-r)]))

    (define/private (query1:error fsym r)
      (match r
        [(struct CopyInResponse (format column-formats))
         (uerror fsym (nosupport "COPY IN statements"))]
        [(struct CopyOutResponse (format column-formats))
         (uerror fsym (nosupport "COPY OUT statements"))]
        [_ (error/comm fsym (format "got: ~e" r))]))

    (define/private (get-convert-row! fsym field-dvecs)
      (let* ([type-reader-v
              (list->vector (query1:get-type-readers fsym field-dvecs))])
        (lambda (row)
          (vector-map! (lambda (value type-reader)
                         (cond [(sql-null? value) sql-null]
                               [else (type-reader value)]))
                       row
                       type-reader-v))))

    (define/private (query1:process-result fsym result)
      (match result
        [(vector 'rows field-dvecs rows)
         (for-each (get-convert-row! fsym field-dvecs) rows)
         (rows-result (map field-dvec->field-info field-dvecs) rows)]
        [(vector 'cursor field-dvecs stmt portal)
         (let* ([convert-row! (get-convert-row! fsym field-dvecs)]
                [pst (statement-binding-pst stmt)])
           ;; FIXME: register finalizer to close portal?
           (cursor-result (map field-dvec->field-info field-dvecs)
                          pst
                          (list portal convert-row! (box #f))))]
        [(vector 'command command)
         (simple-result command)]))

    (define/private (query1:get-type-readers fsym field-dvecs)
      (map (lambda (dvec)
             (let ([typeid (field-dvec->typeid dvec)])
               (typeid->type-reader fsym typeid)))
           field-dvecs))

    ;; == Cursor

    (define/public (fetch/cursor fsym cursor fetch-size)
      (let ([pst (cursor-result-pst cursor)]
            [extra (cursor-result-extra cursor)])
        (send pst check-owner fsym this pst)
        (let ([portal (car extra)]
              [convert-row! (cadr extra)]
              [end-box (caddr extra)])
          (let ([rows
                 (call-with-lock fsym
                   (lambda ()
                     (cond [(unbox end-box) #f]
                           [else
                            (buffer-message (make-Execute portal fetch-size))
                            (send-message (make-Sync))
                            (let ([rows (query1:data-loop fsym end-box)])
                              (check-ready-for-query fsym #f)
                              (when (unbox end-box)
                                (cursor:close fsym pst portal))
                              rows)])))])
            (and rows (begin (for-each convert-row! rows) rows))))))

    (define/private (cursor:close fsym pst portal)
      (let ([close-on-exec? (send pst get-close-on-exec?)])
        (buffer-message (make-Close 'portal portal))
        (when close-on-exec?
          (buffer-message (make-Close 'statement (send pst get-handle)))
          (send pst set-handle #f))
        (send-message (make-Sync))
        (query1:expect-close-complete fsym close-on-exec?)
        (check-ready-for-query fsym #f)))

    ;; == Prepare

    (define/public (prepare fsym stmt close-on-exec?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (prepare1 fsym stmt close-on-exec?))))

    (define/private (prepare1 fsym stmt close-on-exec?)
      ;; name generation within exchange: synchronized
      (let ([name (generate-name)])
        (prepare1:enqueue name stmt)
        (send-message (make-Sync))
        (begin0 (prepare1:collect fsym name close-on-exec? (classify-pg-sql stmt))
          (check-ready-for-query fsym #f))))

    (define/private (prepare1:enqueue name stmt)
      (buffer-message (make-Parse name stmt null))
      (buffer-message (make-Describe 'statement name)))

    (define/private (prepare1:collect fsym name close-on-exec? stmt-type)
      (match (recv-message fsym)
        [(struct ParseComplete ()) (void)]
        [other-r (prepare1:error fsym other-r)])
      (let* ([param-typeids (prepare1:describe-params fsym)]
             [field-dvecs (prepare1:describe-result fsym)])
        (new prepared-statement%
             (handle name)
             (close-on-exec? close-on-exec?)
             (param-typeids param-typeids)
             (result-dvecs field-dvecs)
             (stmt-type stmt-type)
             (owner this))))

    (define/private (prepare1:describe-params fsym)
      (match (recv-message fsym)
        [(struct ParameterDescription (param-typeids)) param-typeids]
        [other-r (prepare1:error fsym other-r)]))

    (define/private (prepare1:describe-result fsym)
      (match (recv-message fsym)
        [(struct RowDescription (field-dvecs)) field-dvecs]
        [(struct NoData ()) null]
        [other-r (prepare1:error fsym other-r)]))

    (define/private (prepare1:error fsym r)
      (error/comm fsym "during prepare"))

    ;; name-counter : nat
    (define name-counter 0)

    ;; generate-name : -> string
    (define/private (generate-name)
      (let ([n name-counter])
        (set! name-counter (add1 name-counter))
        (format "λmz_~a_~a" process-id n)))

    (define/public (get-base) this)

    ;; free-statement : prepared-statement -> void
    (define/public (free-statement pst need-lock?)
      (define (do-free-statement)
        (let ([name (send pst get-handle)])
          (when (and name outport) ;; outport = connected?
            (send pst set-handle #f)
            (buffer-message (make-Close 'statement name))
            (buffer-message (make-Sync))
            (let ([r (recv-message 'free-statement)])
              (cond [(CloseComplete? r) (void)]
                    [else (error/comm 'free-statement)])
              (check-ready-for-query 'free-statement #t)))))
      (if need-lock?
          (call-with-lock* 'free-statement
                           do-free-statement
                           void
                           #f)
          (do-free-statement)))

    ;; == Internal query

    (define/private (internal-query1 fsym sql)
      (query1 fsym sql #f #f))

    ;; == Transactions

    (define/override (start-transaction* fsym isolation)
      (cond [(eq? isolation 'nested)
             (let ([savepoint (generate-name)])
               (internal-query1 fsym (format "SAVEPOINT ~a" savepoint))
               savepoint)]
            [else
             (let* ([isolation-level (isolation-symbol->string isolation)]
                    [stmt (if isolation-level
                              (string-append "BEGIN WORK ISOLATION LEVEL " isolation-level)
                              "BEGIN WORK")])
               ;; FIXME: also support
               ;;   'read-only  => "READ ONLY"
               ;;   'read-write => "READ WRITE"
               (internal-query1 fsym stmt)
               #f)]))

    (define/override (end-transaction* fsym mode savepoint)
      (case mode
        ((commit)
         (cond [savepoint
                (internal-query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint))]
               [else
                (internal-query1 fsym "COMMIT WORK")]))
        ((rollback)
         (cond [savepoint
                (internal-query1 fsym (format "ROLLBACK TO SAVEPOINT ~a" savepoint))
                (internal-query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint))]
               [else
                (internal-query1 fsym "ROLLBACK WORK")])))
      (void))

    ;; == Reflection

    (define/public (list-tables fsym schema)
      (let* ([stmt
              (string-append
               "SELECT table_name FROM information_schema.tables WHERE "
               (case schema
                 ((search search-or-current)
                  "table_schema = SOME (current_schemas(false))")
                 ((current)
                  "table_schema = current_schema")))]
             [result (call-with-lock fsym (lambda () (internal-query1 fsym stmt)))]
             [rows (vector-ref result 2)])
        (for/list ([row (in-list rows)])
          (bytes->string/utf-8 (vector-ref row 0)))))
    ))

;; ========================================

;; ssl-connector-mixin
;; Adds SSL connection support.
(define ssl-connector-mixin
  (mixin (connector<%>) ()
    (super-new)

    ;; attach-to-ports : input-port output-port -> void
    (define/override (attach-to-ports in out [ssl 'no] [ssl-context #f])
      (with-handlers ([(lambda _ #t)
                       (lambda (e)
                         (close-input-port in)
                         (close-output-port out)
                         (raise e))])
        (case ssl
          ((yes optional)
           ;; Try negotiating SSL connection
           (write-message (make-SSLRequest) out)
           (flush-output out)
           (let ([response (peek-byte in)])
             (case (integer->char response)
               ((#\S)
                (void (read-byte in))
                (let-values ([(sin sout)
                              (ports->ssl-ports in out
                                                #:mode 'connect
                                                #:context ssl-context
                                                #:close-original? #t)])
                  (super attach-to-ports sin sout)))
               ((#\N)
                ;; Backend gracefully declined
                (void (read-byte in))
                (unless (eq? ssl 'optional)
                  (error 'postgresql-connect "server refused SSL connection"))
                (super attach-to-ports in out))
               ((#\E)
                (let ([r (parse-server-message in)])
                  (raise-backend-error 'postgresql-connect r)))
               (else
                (error/comm 'postgresql-connect "after SSL request")))))
          ((no)
           (super attach-to-ports in out)))))))

;; ========================================

;; nosupport : string -> string
(define (nosupport str)
  (string-append "not supported: " str))

;; ========================================

;; md5-password : string (U string (list 'hash string)) bytes -> string
;; Compute the MD5 hash of a password in the form expected by the PostgreSQL 
;; backend.
(define (md5-password user password salt)
  (let ([hash
         (cond [(pair? password) (string->bytes/latin-1 (cadr password))]
               [(string? password) (password-hash user password)])])
    (bytes->string/latin-1
     (bytes-append #"md5" (md5 (bytes-append hash salt))))))

;; password-hash : string string -> bytes
(define (password-hash user password)
  (let ([user (string->bytes/latin-1 user)]
        [password (string->bytes/latin-1 password)])
    (md5 (bytes-append password user))))

;; ========================================

;; raise-backend-error : symbol ErrorResponse -> raises exn
(define (raise-backend-error who r)
  (define props (ErrorResponse-properties r))
  (define code (cdr (assq 'code props)))
  (define message (cdr (assq 'message props)))
  (raise-sql-error who code message props))

;; ========================================

;; connection%
(define connection%
  (class (ssl-connector-mixin connection-base%)
    (super-new)))
