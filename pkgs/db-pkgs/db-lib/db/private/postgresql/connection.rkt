#lang racket/base
(require racket/class
         racket/match
         file/md5
         openssl
         unstable/error
         db/private/generic/interfaces
         db/private/generic/common
         db/private/generic/sql-data
         db/private/generic/prepared
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
  (class* statement-cache% (connection<%> connector<%>)
    (init-private notice-handler
                  notification-handler
                  allow-cleartext-password?)
    (define inport #f)
    (define outport #f)
    (define process-id #f)
    (define integer-datetimes? 'unknown)  ;; see connect:after-auth

    (inherit call-with-lock
             call-with-lock*
             add-delayed-call!
             get-tx-status
             set-tx-status!
             check-valid-tx-status
             check-statement/tx
             tx-state->string
             dprintf
             prepare1
             check/invalidate-cache)

    (super-new)

    ;; ========================================

    ;; == Communication
    #|
    During initial setup, okay to send and recv directly, since reference
    to connection does not escape to user. In particular, no danger of trying
    to start a new exchange on top of an incomplete failed one.

    After initial setup, communication can only happen within lock, and any
    error (other than exn:fail:sql) that occurs between sending the message
    buffer (flush-message-buffer) and receiving the last message (recv-message)
    must cause the connection to disconnect. Such errors include communication
    errors and breaks.
    |#

    ;; message-buffer : reversed list of messages
    (define message-buffer null)

    ;; fresh-exchange : -> void
    (define/private (fresh-exchange)
      (set! message-buffer null))

    ;; buffer-message : message -> void
    (define/private (buffer-message msg)
      (dprintf "  >> ~s\n" msg)
      (set! message-buffer (cons msg message-buffer)))

    ;; flush-message-buffer : -> void
    (define/private (flush-message-buffer)
      (for ([msg (in-list (reverse message-buffer))])
        (write-message msg outport))
      (set! message-buffer null)
      (flush-output outport))

    ;; send-message : message -> void
    (define/private (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    (define/private (call-with-sync fsym proc)
      (buffer-message (make-Sync))
      (with-handlers ([(lambda (e) #t)
                       (lambda (e)
                         ;; Anything but exn:fail:sql (raised by recv-message) indicates
                         ;; a communication error.
                         ;; Alternative: could have check-ready-for-query set a done-reading flag.
                         (unless (exn:fail:sql? e)
                           (disconnect* #f))
                         (raise e))])
        (flush-message-buffer)
        (begin0 (proc)
          (check-ready-for-query fsym #f))))

    ;; recv-message : symbol -> message
    (define/private (recv-message fsym)
      (let ([r (raw-recv)])
        (cond [(ErrorResponse? r)
               ;; No need to check for FATAL errors here and disconnect, because
               ;; followed by EOF, handled by check-ready-for-query.
               (check-ready-for-query fsym #t)
               (raise-backend-error fsym r)]
              [(or (NoticeResponse? r)
                   (NotificationResponse? r)
                   (ParameterStatus? r))
               (handle-async-message fsym r)
               (recv-message fsym)]
              [else r])))

    ;; raw-recv : -> message
    (define/private (raw-recv)
      (let ([r (parse-server-message inport)])
        (dprintf "  << ~s\n" r)
        r))

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
              [(and or-eof? (eof-object? r))
               (disconnect* #f)]
              [else (error/comm fsym "expecting ready-for-query")])))

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
                  (error* fsym "client character encoding changed, disconnecting"
                          '("new encoding" value) value))]
               [(equal? name "integer_datetimes")
                (set! integer-datetimes? (equal? value "on"))]
               [else (void)])]))

    ;; == Connection management

    ;; disconnect* : boolean -> void
    (define/override (disconnect* politely?)
      (super disconnect* politely?)
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

    ;; connected? : -> boolean
    (define/override (connected?)
      (let ([outport outport])
        (and outport (not (port-closed? outport)))))

    ;; == System

    (define/public (get-dbsystem)
      (if integer-datetimes?
          dbsystem/integer-datetimes
          dbsystem/floating-point-datetimes))

    ;; ========================================

    ;; == Connect

    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! inport in)
      (set! outport out))

    ;; start-connection-protocol : string string string/#f -> void
    (define/public (start-connection-protocol dbname username password)
      (call-with-lock 'postgresql-connect
        (lambda ()
          (send-message
           (make-StartupMessage
            (list (cons "user" username)
                  (cons "database" dbname)
                  (cons "client_encoding" "UTF8")
                  (cons "DateStyle" "ISO, MDY"))))
          (connect:expect-auth username password))))

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
             (error/no-support 'postgresql-connect "cleartext password"))
           (send-message (make-PasswordMessage password))
           (connect:expect-auth username password)]
          [(struct AuthenticationCryptPassword (salt))
           (error/no-support 'postgresql-connect "crypt()-encrypted password")]
          [(struct AuthenticationMD5Password (salt))
           (unless password
             (error/need-password 'postgresql-connect))
           (send-message (make-PasswordMessage (md5-password username password salt)))
           (connect:expect-auth username password)]
          [(struct AuthenticationKerberosV5 ())
           (error/no-support 'postgresql-connect "KerberosV5 authentication")]
          [(struct AuthenticationSCMCredential ())
           (error/no-support 'postgresql-connect "SCM authentication")]
          ;; ErrorResponse handled by recv-message
          [_ (error/comm 'postgresql-connect "during authentication")])))

    ;; connect:expect-ready-for-query : -> void
    (define/private (connect:expect-ready-for-query)
      (let ([r (recv-message 'postgresql-connect)])
        (match r
          [(struct ReadyForQuery (status))
           (connect:after-auth)]
          [(struct BackendKeyData (pid secret))
           (set! process-id pid)
           (connect:expect-ready-for-query)]
          [_
           (error/comm 'postgresql-connect "after authentication")])))

    ;; connect:after-auth : -> void
    (define/private (connect:after-auth)
      (when (eq? integer-datetimes? 'unknown)
        ;; According to http://www.postgresql.org/docs/8.4/static/libpq-status.html
        ;; (see PQparameterStatus), versions of PostgreSQL before 8.0 do not send
        ;; (or AFAICT even *have*) the "integer_datetimes" parameter on startup.
        ;; Version 7.4.8 from Ubuntu 5.10 uses integer datetimes, no config var.
        ;; Version 7.4.6 from Fedora Core 3 uses floating-point, no config var.
        ;; So determine by trying query w/ integer; if wrong result, try float.
        (dprintf "  ** testing datetime representation\n")
        (let ([r (internal-query1 'postgresql-connect "select time '12:34:56'")])
          (define (test-config)
            (match (query1:process-result 'postgresql-connect r)
              [(rows-result _ (list (vector (sql-time 12 34 56 0 #f)))) #t]
              [_ #f]))
          (set! integer-datetimes? #t)
          (unless (test-config)
            (set! integer-datetimes? #f)
            (unless (test-config)
              (error/internal 'postgresql-connect
                              "unable to determine server datetime representation")))
          (dprintf "  ** datetime representation = ~a\n"
                   (if integer-datetimes? "integer" "floating-point")))))

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
      (fresh-exchange)
      (let* ([delenda (check/invalidate-cache stmt)]
             [portal (query1:enqueue delenda stmt close-on-exec? cursor?)])
        (call-with-sync fsym
          (lambda ()
            (query1:collect fsym delenda stmt portal (string? stmt) close-on-exec? cursor?)))))

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
    (define/private (query1:enqueue delenda stmt close-on-exec? cursor?)
      (when delenda
        (for ([(_sql pst) (in-hash delenda)])
          (buffer-message (make-Close 'statement (send pst get-handle)))
          (send pst set-handle #f)))
      (let ([portal (if cursor? (generate-name) "")])
        (cond [(statement-binding? stmt)
               (let* ([pst (statement-binding-pst stmt)]
                      [pst-name (send pst get-handle)]
                      [params (statement-binding-params stmt)])
                 (unless pst-name
                   (error/internal* 'query1:enqueue "statement was deleted"
                                    "statement" (send pst get-stmt)))
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

    (define/private (query1:collect fsym delenda stmt portal simple? close-on-exec? cursor?)
      (when delenda
        (for ([(_sql _pst) (in-hash delenda)])
          (match (recv-message fsym)
            [(struct CloseComplete ()) (void)]
            [other-r (query1:error fsym other-r)])))
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
        [(struct CommandComplete (command)) command]
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
         (error/no-support fsym "COPY IN statements")]
        [(struct CopyOutResponse (format column-formats))
         (error/no-support fsym "COPY OUT statements")]
        [_ (error/internal* fsym "unexpected message from back end"
                            '("message" value) r)]))

    (define/private (query1:process-result fsym result)
      (match result
        [(vector 'rows field-dvecs rows)
         (let ([type-readers (query1:get-type-readers fsym field-dvecs)])
           (parameterize ((use-integer-datetimes? integer-datetimes?))
             (rows-result (map field-dvec->field-info field-dvecs)
                          (map (lambda (data) (bytes->row data type-readers))
                               rows))))]
        [(vector 'cursor field-dvecs stmt portal)
         (let ([pst (statement-binding-pst stmt)]
               [type-readers (query1:get-type-readers fsym field-dvecs)])
           (cursor-result (map field-dvec->field-info field-dvecs)
                          pst
                          (list portal type-readers (box #f))))]
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
              [type-readers (cadr extra)]
              [end-box (caddr extra)])
          (let ([rows
                 (call-with-lock fsym
                   (lambda ()
                     (cond [(unbox end-box) #f]
                           [else
                            (fresh-exchange)
                            (buffer-message (make-Execute portal fetch-size))
                            (let ([rows
                                   (call-with-sync fsym
                                     (lambda () (query1:data-loop fsym end-box)))])
                              (when (unbox end-box)
                                (cursor:close fsym pst portal))
                              rows)])))])
            (parameterize ((use-integer-datetimes? integer-datetimes?))
              (and rows (map (lambda (data) (bytes->row data type-readers)) rows)))))))

    (define/private (cursor:close fsym pst portal)
      (let ([close-on-exec? (send pst get-close-on-exec?)])
        (fresh-exchange)
        (buffer-message (make-Close 'portal portal))
        (when close-on-exec?
          (buffer-message (make-Close 'statement (send pst get-handle)))
          (send pst set-handle #f))
        (call-with-sync fsym
          (lambda () (query1:expect-close-complete fsym close-on-exec?)))))

    ;; == Prepare

    (define/override (classify-stmt sql) (classify-pg-sql sql))

    (define/override (prepare1* fsym stmt close-on-exec? stmt-type)
      ;; name generation within exchange: synchronized
      (let ([name (generate-name)])
        (fresh-exchange)
        (prepare1:enqueue name stmt)
        (call-with-sync fsym
          (lambda () (prepare1:collect fsym stmt name close-on-exec? stmt-type)))))

    (define/private (prepare1:enqueue name stmt)
      (buffer-message (make-Parse name stmt null))
      (buffer-message (make-Describe 'statement name)))

    (define/private (prepare1:collect fsym stmt name close-on-exec? stmt-type)
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
             (stmt stmt)
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
        (format "rkt_~a_~a" process-id n)))

    (define/public (get-base) this)

    ;; free-statement : prepared-statement -> void
    (define/public (free-statement pst need-lock?)
      (define (do-free-statement)
        (let ([name (send pst get-handle)])
          (when (and name outport) ;; outport = connected?
            (send pst set-handle #f)
            (fresh-exchange)
            (buffer-message (make-Close 'statement name))
            (call-with-sync 'free-statement
             (lambda ()
               (let ([r (recv-message 'free-statement)])
                 (cond [(CloseComplete? r) (void)]
                       [else (error/comm 'free-statement)])))))))
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

    (define/override (start-transaction* fsym isolation option)
      (cond [(eq? isolation 'nested)
             ;; FIXME: error if option != #f ?
             (let ([savepoint (generate-name)])
               (internal-query1 fsym (format "SAVEPOINT ~a" savepoint))
               savepoint)]
            [else
             (let* ([isolation-part (isolation-symbol->string isolation)]
                    [option-part
                     (case option
                       ((read-write) " READ WRITE")
                       ((read-only) " READ ONLY")
                       ((#f) #f)
                       (else (raise-argument-error fsym "(or/c 'read-write 'read-only #f)" option)))]
                    [stmt (string-append "BEGIN WORK"
                                         (if isolation-part " ISOLATION LEVEL " "")
                                         (or isolation-part "")
                                         (or option-part ""))])
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
             [rows (vector-ref result 2)]
             [type-readers (list subbytes)])
        (for/list ([row (in-list rows)])
          (let ([row (bytes->row row type-readers)])
            (bytes->string/utf-8 (vector-ref row 0))))))
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
                  ;; See comments below re buffering-output-port
                  (super attach-to-ports sin (buffering-output-port sout))))
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

;; SSL output ports currently seem to create one SSL record per port write.
;; This causes an explosion in the amount of data sent, due to padding, HMAC,
;; and other record overhead. So add an ad hoc buffering port around it.
;; FIXME: This code is probably wrong in various ways related to concurrency,
;; evts, and breaks.
(define (buffering-output-port out)
  (define-syntax-rule (DEBUG expr ...) (when #f expr ...))
  (DEBUG (eprintf "** making buffered output port\n"))
  (define tmp (open-output-bytes))
  (define (write-out buf start end flush? eb?)
    ;; This code just ignores the eb? (enable-break) argument.
    ;; The 'flush-output' operation seems to trigger a zero-byte write; I haven't
    ;; seen the flush? argument being used.
    (DEBUG (eprintf "** write ~s bytes, flush?=~s, eb?=~s\n" (- end start) flush? eb?))
    (begin0 (if (> end start) (write-bytes buf tmp start end) 0)
      (when (or flush? (= end start)) (flush))))
  (define (flush)
    (define buf (get-output-bytes tmp #t))
    (define end (bytes-length buf))
    (DEBUG (eprintf "** flushing ~s bytes\n" end))
    ;; SSL seems to have trouble accepting too much data at once, but breaking
    ;; it up into 10MB chunks seems to work.
    (for ([start (in-range 0 end #e1e7)])
      (DEBUG (eprintf "-- flushing range [~s,~s)\n" start (min end (+ start #e1e7))))
      (write-bytes buf out start (min end (+ start #e1e7)))
      (flush-output out))
    (when (zero? end) (flush-output out)))
  (define (close)
    (DEBUG (eprintf "** closing\n"))
    (flush)
    (close-output-port out))
  (make-output-port (object-name out)
                    always-evt
                    write-out
                    close))

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
