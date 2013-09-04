#lang racket/base
(require racket/class
         racket/match
         openssl
         openssl/sha1
         unstable/error
         db/private/generic/interfaces
         db/private/generic/common
         db/private/generic/prepared
         db/private/generic/sql-data
         "message.rkt"
         "dbsystem.rkt")
(provide connection%
         mysql-password-hash)

(define MAX-ALLOWED-PACKET (expt 2 30))

;; ========================================

(define connection%
  (class* statement-cache% (connection<%>)
    (init-private notice-handler)
    (define inport #f)
    (define outport #f)

    (inherit call-with-lock
             call-with-lock*
             add-delayed-call!
             check-valid-tx-status
             get-tx-status
             set-tx-status!
             check-statement/tx
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
    buffer (flush-message-buffer) and receiving the last message (recv)
    must cause the connection to disconnect. Such errors include communication
    errors and breaks.
    |#

    (define msg-buffer null)
    (define next-msg-num 0)

    (define/private (fresh-exchange)
      (set! next-msg-num 0))

    ;; buffer-message : message -> void
    (define/private (buffer-message msg)
      (dprintf "  >> ~s\n" msg)
      (set! msg-buffer (cons (cons msg next-msg-num) msg-buffer))
      (set! next-msg-num (add1 next-msg-num)))

    ;; flush-message-buffer : -> void
    (define/private (flush-message-buffer)
      (for ([msg+num (in-list (reverse msg-buffer))])
        (write-packet outport (car msg+num) (cdr msg+num)))
      (set! msg-buffer null)
      (flush-output outport))

    ;; send-message : message -> void
    (define/private (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    (define/private (call-with-sync fsym proc)
      (with-handlers ([(lambda (e) #t)
                       (lambda (e)
                         ;; Anything but exn:fail:sql (raised by recv-message) indicates
                         ;; a communication error.
                         (unless (exn:fail:sql? e)
                           (disconnect* #f))
                         (raise e))])
        (flush-message-buffer)
        (proc)))

    ;; recv : symbol/#f [(list-of symbol)] -> message
    ;; Automatically handles asynchronous messages
    (define/private (recv fsym expectation [field-dvecs #f])
      (define r (recv* fsym expectation field-dvecs))
      (when (error-packet? r)
        (raise-backend-error fsym r))
      r)

    (define/private (recv* fsym expectation field-dvecs)
      (define (advance . ss)
        (unless (or (not expectation) 
                    (null? ss)
                    (memq expectation ss))
          (error/comm fsym)))
      (define (err packet)
        (error/comm fsym))
      (let-values ([(msg-num next) (parse-packet inport expectation field-dvecs)])
        (set! next-msg-num (add1 msg-num))
        (dprintf "  << ~s\n" next)
        ;; Update transaction status (see Transactions below)
        (when (ok-packet? next)
          (set-tx-status! fsym (bitwise-bit-set? (ok-packet-server-status next) 0)))
        (when (eof-packet? next)
          (set-tx-status! fsym (bitwise-bit-set? (eof-packet-server-status next) 0)))
        (when (error-packet? next)
          (when (member (error-packet-errno next) '(1213 1205))
            (when (get-tx-status)
              (set-tx-status! fsym 'invalid))))
        (match next
          [(? handshake-packet?)
           (advance 'handshake)]
          [(? ok-packet?)
           (advance)]
          [(? change-plugin-packet?)
           (advance 'auth)]
          [(? error-packet?)
           (advance)]
          [(? result-set-header-packet?)
           (advance 'result)]
          [(? field-packet?)
           (advance 'field)]
          [(? row-data-packet?)
           (advance 'data)]
          [(? binary-row-data-packet?)
           (advance 'binary-data)]
          [(? ok-prepared-statement-packet?)
           (advance 'prep-ok)]
          [(? eof-packet?)
           (advance 'field 'data 'binary-data)]
          [(struct unknown-packet (expected contents))
           (error/comm fsym expected)]
          [else
           (err next)])
        next))

    ;; ========================================

    ;; Connection management

    (define/override (disconnect* politely?)
      (super disconnect* politely?)
      (let ([outport* outport]
            [inport* inport])
        (when outport*
          (when politely?
            (fresh-exchange)
            (send-message (make-command-packet 'quit "")))
          (with-handlers ([exn:fail? void]) (close-output-port outport*))
          (set! outport #f))
        (when inport*
          (with-handlers ([exn:fail? void]) (close-input-port inport*))
          (set! inport #f))))

    ;; connected? : -> boolean
    (define/override (connected?)
      (let ([outport outport])
        (and outport (not (port-closed? outport)))))

    (define/public (get-dbsystem)
      dbsystem)

    ;; ========================================

    ;; == Connect

    ;; attach-to-ports : input-port output-port -> void
    (define/public (attach-to-ports in out)
      (set! inport in)
      (set! outport out))

    ;; start-connection-protocol : string/#f string string/#f -> void
    (define/public (start-connection-protocol dbname username password ssl ssl-context)
      (fresh-exchange)
      (let ([r (recv 'mysql-connect 'handshake)])
        (match r
          [(struct handshake-packet (pver sver tid scramble capabilities charset status auth))
           (check-required-flags capabilities)
           (unless (member auth '("mysql_native_password" #f))
             (error* 'mysql-connect "back end requested unsupported authentication plugin"
                     '("plugin" value) auth))
           (define do-ssl?
             (and (case ssl ((yes optional) #t) ((no) #f))
                  (memq 'ssl capabilities)))
           (when (and (eq? ssl 'yes) (not do-ssl?))
             (error 'mysql-connect "back end refused SSL connection"))
           (define wanted-capabilities (desired-capabilities capabilities do-ssl? dbname))
           (when do-ssl?
             (send-message (make-abbrev-client-auth-packet wanted-capabilities))
             (let-values ([(sin sout)
                           (ports->ssl-ports inport outport
                                             #:mode 'connect
                                             #:context ssl-context
                                             #:close-original? #t)])
               (attach-to-ports sin sout)))
           (authenticate wanted-capabilities username password dbname
                         (or auth "mysql_native_password") scramble)]
          [_ (error/comm 'mysql-connect "during authentication")])))

    (define/private (authenticate capabilities username password dbname auth-plugin scramble)
      (let loop ([auth-plugin auth-plugin] [scramble scramble] [first? #t])
        (define (auth data)
          (if first?
              (make-client-auth-packet capabilities MAX-ALLOWED-PACKET 'utf8-general-ci
                                       username data dbname auth-plugin)
              (make-auth-followup-packet data)))
        (cond [(equal? auth-plugin "mysql_native_password")
               (send-message (auth (scramble-password scramble password)))]
              [(equal? auth-plugin "mysql_old_password")
               (send-message (auth (bytes-append (old-scramble-password scramble password)
                                                 (bytes 0))))]
              [else (error* 'mysql-connect "back end does not support authentication plugin"
                            '("plugin" value) auth-plugin)])
        (match (recv 'mysql-connect 'auth)
          [(struct ok-packet (_ _ status warnings message))
           (after-connect)]
          [(struct change-plugin-packet (plugin data))
           ;; if plugin = #f, means "mysql_old_password"
           (loop (or plugin "mysql_old_password") (or data scramble) #f)])))

    (define/private (check-required-flags capabilities)
      (for-each (lambda (rf)
                  (unless (memq rf capabilities)
                    (error* 'mysql-connect "server does not support required capability"
                            "capability" rf)))
                REQUIRED-CAPABILITIES))

    (define/private (desired-capabilities capabilities ssl? dbname)
      (append (if ssl?   '(ssl)             '())
              (if dbname '(connect-with-db) '())
              '(interactive)
              (filter (lambda (c) (memq c DESIRED-CAPABILITIES)) capabilities)))

    ;; Set connection to use utf8 encoding
    (define/private (after-connect)
      (query 'mysql-connect "set names 'utf8'" #f)
      (void))

    ;; ========================================

    ;; == Query

    ;; query : symbol Statement boolean -> QueryResult
    (define/public (query fsym stmt cursor?)
      (let ([result
             (call-with-lock fsym
               (lambda ()
                 (check-valid-tx-status fsym)
                 (let* ([stmt (check-statement fsym stmt cursor?)]
                        [stmt-type
                         (cond [(statement-binding? stmt)
                                (send (statement-binding-pst stmt) get-stmt-type)]
                               [(string? stmt)
                                (classify-my-sql stmt)])])
                   (check-statement/tx fsym stmt-type)
                   (begin0 (query1 fsym stmt cursor? #t)
                     (statement:after-exec stmt #f)))))])
        (query1:process-result fsym result)))

    ;; query1 : symbol Statement -> QueryResult
    (define/private (query1 fsym stmt cursor? warnings?)
      (let ([delenda (check/invalidate-cache stmt)])
        (when delenda
          (for ([(_sql pst) (in-hash delenda)])
            (free-statement pst #f))))
      (let ([wbox (and warnings? (box 0))])
        (fresh-exchange)
        (query1:enqueue stmt cursor?)
        (begin0 (call-with-sync fsym
                  (lambda () (query1:collect fsym stmt (not (string? stmt)) cursor? wbox)))
          (when (and warnings? (not (zero? (unbox wbox))))
            (fetch-warnings fsym)))))

    ;; check-statement : symbol any boolean -> statement-binding
    ;; For cursor, need to clone pstmt, because only one cursor can be
    ;; open for a statement at a time. (Could delay clone until
    ;; needed, but that seems more complicated.)
    (define/private (check-statement fsym stmt cursor?)
      (cond [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)])
               (send pst check-owner fsym this stmt)
               (for ([typeid (in-list (send pst get-result-typeids))])
                 (unless (supported-result-typeid? typeid)
                   (error/unsupported-type fsym typeid)))
               (cond [cursor?
                      (let ([pst* (prepare1 fsym (send pst get-stmt) #f)])
                        (statement-binding pst* (statement-binding-params stmt)))]
                     [else stmt]))]
            [(and (string? stmt) (force-prepare-sql? fsym stmt))
             (let ([pst (prepare1 fsym stmt (not cursor?))])
               (check-statement fsym (send pst bind fsym null) #f))]
            [else stmt]))

    ;; query1:enqueue : statement -> void
    (define/private (query1:enqueue stmt cursor?)
      (cond [(statement-binding? stmt)
             (let* ([pst (statement-binding-pst stmt)]
                    [id (send pst get-handle)]
                    [params (statement-binding-params stmt)]
                    [param-count (length params)]
                    [null-map (map sql-null? params)]
                    [flags (if cursor? '(cursor/read-only) '())])
               ;; Assume max_packet_length = 16M = 2^24,
               ;; overhead of 20 bytes for other packet fields.
               ;; Oversimplified param size estimate:
               ;;   - 20 bytes per param (fixed size <= 20, string length code <= 20)
               ;;   - bytes-length for bytes, 4*string-length for strings
               ;; Use long data for any param that takes more than its "fair share".
               (define (param-size p)
                 (cond [(string? p) (* 4 (string-length p))]
                       [(bytes? p) (bytes-length p)]
                       [else 0]))
               (let* ([space (- (expt 2 24) 20 (* 20 param-count))]
                      [var-param-size (for/sum ([p (in-list params)]) (param-size p))])
                 (cond [(and (< var-param-size space))
                        (buffer-message (make-execute-packet id flags null-map params))]
                       [else
                        (let* ([var-param-count
                                (for/sum ([p (in-list params)]
                                          #:when (or (string? p) (bytes? p)))
                                  1)]
                               [fair-share (floor (/ space (max 1 var-param-count)))]
                               [param+evict-list
                                (for/list ([p (in-list params)])
                                  (cons p (> (param-size p) fair-share)))]
                               [short-params
                                (for/list ([p+e (in-list param+evict-list)])
                                  (let ([p (car p+e)])
                                    (if (cdr p+e)
                                        (if (string? p) 'long-string 'long-binary)
                                        p)))])
                          (for ([p+e (in-list param+evict-list)]
                                [param-id (in-naturals)]
                                #:when (cdr p+e))
                            (let* ([p (car p+e)]
                                   [pb (if (string? p) (string->bytes/utf-8 p) p)]
                                   [pblen (bytes-length pb)]
                                   [CHUNK #e1e6])
                              (let chunkloop ([sent 0])
                                (when (< sent pblen)
                                  (let ([next (min pblen (+ sent CHUNK))])
                                    (buffer-message
                                     (make-long-data-packet id param-id (subbytes pb sent next)))
                                    (fresh-exchange)
                                    (chunkloop next))))))
                          (buffer-message (make-execute-packet id flags null-map short-params)))])))]
            [else ;; string
             (buffer-message (make-command-packet 'query stmt))]))

    ;; query1:collect : symbol bool -> QueryResult stream
    (define/private (query1:collect fsym stmt binary? cursor? wbox)
      (let ([r (recv fsym 'result)])
        (match r
          [(struct ok-packet (affected-rows insert-id status warnings message))
           (when wbox (set-box! wbox warnings))
           (vector 'command `((affected-rows . ,affected-rows)
                              (insert-id . ,(if (zero? insert-id) #f insert-id))
                              (status . ,status)
                              (message . ,message)))]
          [(struct result-set-header-packet (fields extra))
           (let* ([field-dvecs (query1:get-fields fsym binary?)])
             (if cursor?
                 (vector 'cursor field-dvecs (statement-binding-pst stmt))
                 (vector 'rows
                         field-dvecs
                         (query1:get-rows fsym field-dvecs binary? wbox #f))))])))

    (define/private (query1:get-fields fsym binary?)
      (let ([r (recv fsym 'field)])
        (match r
          [(? field-packet?)
           (cons (parse-field-dvec r) (query1:get-fields fsym binary?))]
          [(struct eof-packet (warning status))
           null])))

    (define/private (query1:get-rows fsym field-dvecs binary? wbox end-box)
      ;; Note: binary? should always be #t, unless force-prepare-sql? misses something.
      (let ([r (recv fsym (if binary? 'binary-data 'data) field-dvecs)])
        (match r
          [(struct row-data-packet (data))
           (cons data (query1:get-rows fsym field-dvecs binary? wbox end-box))]
          [(struct binary-row-data-packet (data))
           (cons data (query1:get-rows fsym field-dvecs binary? wbox end-box))]
          [(struct eof-packet (warnings status))
           (when wbox (set-box! wbox warnings))
           (when (and end-box (bitwise-bit-set? status 7)) ;; 'last-row-sent
             (set-box! end-box #t))
           null])))

    (define/private (query1:process-result fsym result)
      (match result
        [(vector 'rows field-dvecs rows)
         (rows-result (map field-dvec->field-info field-dvecs) rows)]
        [(vector 'command command-info)
         (simple-result command-info)]
        [(vector 'cursor field-dvecs pst)
         (cursor-result (map field-dvec->field-info field-dvecs)
                        pst
                        (list field-dvecs (box #f)))]))

    ;; == Cursor

    (define/public (fetch/cursor fsym cursor fetch-size)
      (let ([pst (cursor-result-pst cursor)]
            [extra (cursor-result-extra cursor)])
        (send pst check-owner fsym this pst)
        (let ([field-dvecs (car extra)]
              [end-box (cadr extra)])
          (call-with-lock fsym
            (lambda ()
              (cond [(unbox end-box)
                     #f]
                    [else
                     (let ([wbox (box 0)])
                       (fresh-exchange)
                       (buffer-message (make-fetch-packet (send pst get-handle) fetch-size))
                       (begin0 (call-with-sync fsym
                                 (lambda () (query1:get-rows fsym field-dvecs #t wbox end-box)))
                         (when (not (zero? (unbox wbox)))
                           (fetch-warnings fsym))))]))))))

    ;; == Prepare

    (define/override (classify-stmt sql) (classify-my-sql sql))

    (define/override (prepare1* fsym stmt close-on-exec? stmt-type)
      (fresh-exchange)
      (buffer-message (make-command-packet 'statement-prepare stmt))
      (call-with-sync fsym
        (lambda ()
          (let ([r (recv fsym 'prep-ok)])
            (match r
              [(struct ok-prepared-statement-packet (id fields params))
               (let ([param-dvecs
                      (if (zero? params) null (prepare1:get-field-descriptions fsym))]
                     [field-dvecs
                      (if (zero? fields) null (prepare1:get-field-descriptions fsym))])
                 (new prepared-statement%
                      (handle id)
                      (close-on-exec? close-on-exec?)
                      (param-typeids (map field-dvec->typeid param-dvecs))
                      (result-dvecs field-dvecs)
                      (stmt stmt)
                      (stmt-type stmt-type)
                      (owner this)))])))))

    (define/private (prepare1:get-field-descriptions fsym)
      (let ([r (recv fsym 'field)])
        (match r
          [(struct eof-packet (warning-count status))
           null]
          [(? field-packet?)
           (cons (parse-field-dvec r) (prepare1:get-field-descriptions fsym))])))

    (define/public (get-base) this)

    (define/public (free-statement pst need-lock?)
      ;; Important: *buffer* statement-close message, but do not send (ie, flush).
      ;; That way, message included in same TCP packet as next query message, avoiding
      ;; write-write-read TCP packet sequence, Nagle's algorithm & delayed ACK issue.
      (define (do-free-statement)
        (let ([id (send pst get-handle)])
          (when (and id outport) ;; outport = connected?
            (send pst set-handle #f)
            (fresh-exchange)
            (buffer-message (make-command:statement-packet 'statement-close id)))))
      (if need-lock?
          (call-with-lock* 'free-statement do-free-statement void #f)
          (do-free-statement)))

    ;; == Warnings

    (define/private (fetch-warnings fsym)
      (unless (eq? notice-handler void)
        (let ([result (query1 fsym "SHOW WARNINGS" #f #f)])
          (define (find-index name dvecs)
            (for/or ([dvec (in-list dvecs)]
                     [i (in-naturals)])
              (and (equal? (field-dvec->name dvec) name) i)))
          (match result
            [(vector 'rows field-dvecs rows)
             (let ([code-index (find-index "Code" field-dvecs)]
                   [message-index (find-index "Message" field-dvecs)])
               (for ([row (in-list rows)])
                 (let ([code (string->number (vector-ref row code-index))]
                       [message (vector-ref row message-index)])
                   (add-delayed-call! (lambda () (notice-handler code message))))))]))))

    ;; == Transactions

    ;; MySQL: what causes implicit commit, when is transaction rolled back
    ;;   http://dev.mysql.com/doc/refman/5.1/en/implicit-commit.html
    ;;   http://dev.mysql.com/doc/refman/5.1/en/innodb-error-handling.html
    ;;   http://dev.mysql.com/doc/refman/5.1/en/innodb-error-codes.html
    ;;
    ;; Sounds like MySQL rolls back transaction (but may keep open!) on
    ;;   - transaction deadlock = 1213 (ER_LOCK_DEADLOCK)
    ;;   - lock wait timeout (depends on config) = 1205 (ER_LOCK_WAIT_TIMEOUT)

    (define/override (start-transaction* fsym isolation option)
      (cond [(eq? isolation 'nested)
             (let ([savepoint (generate-name)])
               (query1 fsym (format "SAVEPOINT ~a" savepoint) #f #t)
               savepoint)]
            [else
             (let ([isolation-level (isolation-symbol->string isolation)])
               (when option
                 ;; No options supported
                 (raise-argument-error fsym "#f" option))
               (when isolation-level
                 (query1 fsym (format "SET TRANSACTION ISOLATION LEVEL ~a" isolation-level) #f #t))
               (query1 fsym "START TRANSACTION" #f #t)
               #f)]))

    (define/override (end-transaction* fsym mode savepoint)
      (case mode
        ((commit)
         (cond [savepoint
                (query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint) #f #t)]
               [else
                (query1 fsym "COMMIT" #f #t)]))
        ((rollback)
         (cond [savepoint
                (query1 fsym (format "ROLLBACK TO SAVEPOINT ~a" savepoint) #f #t)
                (query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint) #f #t)]
               [else
                (query1 fsym "ROLLBACK" #f #t)])))
      (void))

    ;; name-counter : number
    (define name-counter 0)

    ;; generate-name : -> string
    (define/private (generate-name)
      (let ([n name-counter])
        (set! name-counter (add1 name-counter))
        (format "λmz_~a" n)))

    ;; Reflection

    (define/public (list-tables fsym schema)
      (let* ([stmt
              ;; schema is ignored; search = current
              (string-append "SELECT table_name FROM information_schema.tables "
                             "WHERE table_schema = schema()")]
             [rows
              (vector-ref (call-with-lock fsym (lambda () (query1 fsym stmt #f #t))) 2)])
        (for/list ([row (in-list rows)])
          (vector-ref row 0))))

    ))

;; ========================================

;; mysql-password-hash : string -> string
(define (mysql-password-hash password)
  (bytes->hex-string (password-hash password)))

;; scramble-password : bytes string -> bytes
(define (scramble-password scramble password)
  (and scramble password
       (let* ([stage1 (cond [(string? password) (password-hash password)]
                            [(pair? password) (hex-string->bytes (cadr password))])]
              [stage2 (sha1-bytes (open-input-bytes stage1))]
              [stage3 (sha1-bytes (open-input-bytes (bytes-append scramble stage2)))]
              [reply (bytes-xor stage1 stage3)])
         reply)))

;; password-hash : string -> bytes
(define (password-hash password)
  (let* ([password (string->bytes/latin-1 password)]
         [stage1 (sha1-bytes (open-input-bytes password))])
    stage1))

;; bytes-xor : bytes bytes -> bytes
;; Assumes args are same length
(define (bytes-xor a b)
  (let ([c (make-bytes (bytes-length a))])
    (let loop ([i 0])
      (when (< i (bytes-length c))
        (bytes-set! c i
                    (bitwise-xor (bytes-ref a i) (bytes-ref b i)))
        (loop (add1 i))))
    c))

;; =======================================

(provide old-scramble-password
         hash323
         hash323->string)

(define (old-scramble-password scramble password)
  (define (xor a b) (bitwise-xor a b))
  (define RMAX #x3FFFFFFF)
  (and scramble password
       (let* ([scramble (subbytes scramble 0 8)]
              [password (string->bytes/utf-8 password)]
              [hp (hash323 password)]
              [hm (hash323 scramble)]
              [r1 (modulo (xor (car hp) (car hm)) RMAX)]
              [r2 (modulo (xor (cdr hp) (cdr hm)) RMAX)]
              [out (make-bytes 8 0)])
         (define (rnd)
           (set! r1 (modulo (+ (* 3 r1) r2) RMAX))
           (set! r2 (modulo (+ r1 r2 33) RMAX))
           (/ (exact->inexact r1) (exact->inexact RMAX)))
         (for ([i (in-range (bytes-length scramble))])
           (let ([b (+ (inexact->exact (floor (* (rnd) 31))) 64)])
             (bytes-set! out i b)
             (values r1 r2)))
         (let ([extra (inexact->exact (floor (* (rnd) 31)))])
           (for ([i (in-range (bytes-length scramble))])
             (bytes-set! out i (xor (bytes-ref out i) extra))))
         out)))

(define (hash323 bs)
  (define (xor a b) (bitwise-xor a b))
  (define-syntax-rule (normalize! var)
    (set! var (bitwise-and var (sub1 (arithmetic-shift 1 64)))))
  (let ([nr 1345345333]
        [add 7]
        [nr2 #x12345671])
    (for ([i (in-range (bytes-length bs))]
          #:when (not (memv (bytes-ref bs i) '(#\space #\tab))))
      (let ([tmp (bytes-ref bs i)])
        (set! nr  (xor nr
                       (+ (* (+ (bitwise-and nr 63) add) tmp)
                          (arithmetic-shift nr 8))))
        (normalize! nr)
        (set! nr2 (+ nr2
                     (xor (arithmetic-shift nr2 8) nr)))
        (normalize! nr2)
        (set! add (+ add tmp))
        (normalize! add)))
    (cons (bitwise-and nr  (sub1 (arithmetic-shift 1 31)))
          (bitwise-and nr2 (sub1 (arithmetic-shift 1 31))))))

(define (hash323->string bs)
  (let ([p (hash323 bs)])
    (bytes-append (integer->integer-bytes (car p) 4 #f #f)
                  (integer->integer-bytes (cdr p) 4 #f #f))))

;; ========================================

(define REQUIRED-CAPABILITIES
  '(long-flag
    connect-with-db
    protocol-41
    secure-connection))

(define DESIRED-CAPABILITIES
  '(long-password
    long-flag
    transactions
    protocol-41
    secure-connection
    plugin-auth))

;; raise-backend-error : symbol ErrorPacket -> raises exn
(define (raise-backend-error who r)
  (define code (error-packet-sqlstate r))
  (define message (error-packet-message r))
  (define props (list (cons 'errno (error-packet-errno r))
                      (cons 'code code)
                      (cons 'message message)))
  (raise-sql-error who code message props))

;; ========================================

#|
MySQL allows only certain kinds of statements to be prepared; the rest
must go through the old execution path. See here:
  http://dev.mysql.com/doc/refman/5.0/en/c-api-prepared-statements.html
According to that page, the following statements may be prepared:

  CALL, CREATE TABLE, DELETE, DO, INSERT, REPLACE, SELECT, SET, UPDATE,
  and most SHOW statements

On the other hand, we want to force all rows-returning statements
through the prepared-statement path to use the binary data
protocol. That would seem to be the following:

  SELECT and SHOW
|#

(define (force-prepare-sql? fsym stmt)
  (memq (classify-my-sql stmt) '(select show)))
