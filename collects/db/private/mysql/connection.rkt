#lang racket/base
(require racket/class
         racket/match
         openssl
         openssl/sha1
         "../generic/interfaces.rkt"
         "../generic/prepared.rkt"
         "../generic/sql-data.rkt"
         "message.rkt"
         "dbsystem.rkt")
(provide connection%
         password-hash)

(define MAX-PACKET-LENGTH #x1000000)

;; ========================================

(define connection%
  (class* transactions% (connection<%>)
    (init-private notice-handler)
    (define inport #f)
    (define outport #f)

    (inherit call-with-lock
             call-with-lock*
             add-delayed-call!
             check-valid-tx-status
             check-statement/tx)
    (inherit-field tx-status)

    (super-new)

    ;; with-disconnect-on-error
    (define-syntax-rule (with-disconnect-on-error . body)
      (with-handlers ([exn:fail? (lambda (e) (disconnect* #f) (raise e))])
        . body))

    ;; ========================================

    ;; == Debugging

    (define DEBUG? #f)

    (define/public (debug debug?)
      (set! DEBUG? debug?))

    ;; ========================================

    ;; == Communication
    ;; (Must be called with lock acquired.)

    (define next-msg-num 0)

    (define/private (fresh-exchange)
      (set! next-msg-num 0))

    ;; send-message : message -> void
    (define/private (send-message msg)
      (buffer-message msg)
      (flush-message-buffer))

    ;; buffer-message : message -> void
    (define/private (buffer-message msg)
      (when DEBUG?
        (fprintf (current-error-port) "  >> ~s\n" msg))
      (with-disconnect-on-error
       (write-packet outport msg next-msg-num)
       (set! next-msg-num (add1 next-msg-num))))

    ;; flush-message-buffer : -> void
    (define/private (flush-message-buffer)
      (with-disconnect-on-error
       (flush-output outport)))

    ;; recv : symbol/#f [(list-of symbol)] -> message
    ;; Automatically handles asynchronous messages
    (define/private (recv fsym expectation [field-dvecs #f])
      (define r
        (with-disconnect-on-error
         (recv* fsym expectation field-dvecs)))
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
        (when DEBUG?
          (eprintf "  << ~s\n" next))
        ;; Update transaction status (see Transactions below)
        (when (ok-packet? next)
          (set! tx-status
                (bitwise-bit-set? (ok-packet-server-status next) 0)))
        (when (eof-packet? next)
          (set! tx-status
                (bitwise-bit-set? (eof-packet-server-status next) 0)))
        (when (error-packet? next)
          (when tx-status
            (when (member (error-packet-errno next) '(1213 1205))
              (set! tx-status 'invalid))))
        (match next
          [(? handshake-packet?)
           (advance 'handshake)]
          [(? ok-packet?)
           (advance)]
          [(? change-plugin-packet?)
           (advance 'auth)]
          [(? error-packet?)
           (advance)]
          [(struct result-set-header-packet (field-count _))
           (advance 'result)]
          [(? field-packet?)
           (advance 'field)]
          [(? row-data-packet?)
           (advance 'data)]
          [(? binary-row-data-packet?)
           (advance 'binary-data)]
          [(? ok-prepared-statement-packet? result)
           (advance 'prep-ok)]
          [(? parameter-packet? result)
           (advance 'prep-params)]
          [(? eof-packet?)
           (advance 'field 'data 'binary-data 'prep-params)]
          [(struct unknown-packet (expected contents))
           (error/comm fsym expected)]
          [else
           (err next)])
        next))

    ;; ========================================

    ;; Connection management

    ;; disconnect : -> (void)
    (define/public (disconnect)
      (disconnect* #t))

    (define/private (disconnect* lock-not-held?)
      (define (go politely?)
        (when DEBUG?
          (eprintf "  ** Disconnecting\n"))
        (let ([outport* outport]
              [inport* inport])
          (when outport
            (when politely?
              (fresh-exchange)
              (send-message (make-command-packet 'quit "")))
            (close-output-port outport)
            (set! outport #f))
          (when inport
            (close-input-port inport)
            (set! inport #f))))
      ;; If we don't hold the lock, try to acquire it and disconnect politely.
      ;; Except, if already disconnected, no need to acquire lock.
      (cond [(and lock-not-held? (connected?))
             (call-with-lock* 'disconnect
               (lambda () (go #t))
               (lambda () (go #f))
               #f)]
            [else (go #f)]))

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
      (with-disconnect-on-error
        (fresh-exchange)
        (let ([r (recv 'mysql-connect 'handshake)])
          (match r
            [(struct handshake-packet (pver sver tid scramble capabilities charset status auth))
             (check-required-flags capabilities)
             (unless (member auth '("mysql_native_password" #f))
               (uerror 'mysql-connect "unsupported authentication plugin: ~s" auth))
             (define do-ssl?
               (and (case ssl ((yes optional) #t) ((no) #f))
                    (memq 'ssl capabilities)))
             (when (and (eq? ssl 'yes) (not do-ssl?))
               (uerror 'mysql-connect "server refused SSL connection"))
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
            [_ (error/comm 'mysql-connect "during authentication")]))))

    (define/private (authenticate capabilities username password dbname auth-plugin scramble)
      (let loop ([auth-plugin auth-plugin] [scramble scramble] [first? #t])
        (define (auth data)
          (if first?
              (make-client-auth-packet capabilities MAX-PACKET-LENGTH 'utf8-general-ci
                                       username data dbname auth-plugin)
              (make-auth-followup-packet data)))
        (cond [(equal? auth-plugin "mysql_native_password")
               (send-message (auth (scramble-password scramble password)))]
              [(equal? auth-plugin "mysql_old_password")
               (send-message (auth (bytes-append (old-scramble-password scramble password)
                                                 (bytes 0))))]
              [else (uerror 'mysql-connect
                            "server does not support authentication plugin: ~s"
                            auth-plugin)])
        (match (recv 'mysql-connect 'auth)
          [(struct ok-packet (_ _ status warnings message))
           (after-connect)]
          [(struct change-plugin-packet (plugin data))
           ;; if plugin = #f, means "mysql_old_password"
           (loop (or plugin "mysql_old_password") (or data scramble) #f)])))

    (define/private (check-required-flags capabilities)
      (for-each (lambda (rf)
                  (unless (memq rf capabilities)
                    (uerror 'mysql-connect
                            "server does not support required capability: ~s"
                            rf)))
                REQUIRED-CAPABILITIES))

    (define/private (desired-capabilities capabilities ssl? dbname)
      (append (if ssl?   '(ssl)             '())
              (if dbname '(connect-with-db) '())
              '(interactive)
              (filter (lambda (c) (memq c DESIRED-CAPABILITIES)) capabilities)))

    ;; Set connection to use utf8 encoding
    (define/private (after-connect)
      (query 'mysql-connect "set names 'utf8'")
      (void))


    ;; ========================================

    ;; == Query

    ;; query : symbol Statement -> QueryResult
    (define/public (query fsym stmt)
      (check-valid-tx-status fsym)
      (let*-values ([(stmt result)
                     (call-with-lock fsym
                       (lambda ()
                         (let* ([stmt (check-statement fsym stmt)]
                                [stmt-type
                                 (cond [(statement-binding? stmt)
                                        (send (statement-binding-pst stmt) get-stmt-type)]
                                       [(string? stmt)
                                        (classify-my-sql stmt)])])
                           (check-statement/tx fsym stmt-type)
                           (values stmt (query1 fsym stmt #t)))))])
        (when #f ;; DISABLED---for some reason, *really* slow
          (statement:after-exec stmt))
        (query1:process-result fsym result)))

    ;; query1 : symbol Statement -> QueryResult
    (define/private (query1 fsym stmt warnings?)
      (let ([wbox (and warnings? (box 0))])
        (fresh-exchange)
        (query1:enqueue stmt)
        (begin0 (query1:collect fsym (not (string? stmt)) wbox)
          (when (and warnings? (not (zero? (unbox wbox))))
            (fetch-warnings fsym)))))

    ;; check-statement : symbol any -> statement-binding
    (define/private (check-statement fsym stmt)
      (cond [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)])
               (send pst check-owner fsym this stmt)
               (for ([typeid (in-list (send pst get-result-typeids))])
                 (unless (supported-result-typeid? typeid)
                   (error/unsupported-type fsym typeid)))
               stmt)]
            [(and (string? stmt) (force-prepare-sql? fsym stmt))
             (let ([pst (prepare1 fsym stmt #t)])
               (check-statement fsym (send pst bind fsym null)))]
            [else stmt]))

    ;; query1:enqueue : statement -> void
    (define/private (query1:enqueue stmt)
      (cond [(statement-binding? stmt)
             (let* ([pst (statement-binding-pst stmt)]
                    [id (send pst get-handle)]
                    [params (statement-binding-params stmt)]
                    [null-map (map sql-null? params)])
               (send-message
                (make-execute-packet id null null-map params)))]
            [else ;; string
             (send-message (make-command-packet 'query stmt))]))

    ;; query1:collect : symbol bool -> QueryResult stream
    (define/private (query1:collect fsym binary? wbox)
      (let ([r (recv fsym 'result)])
        (match r
          [(struct ok-packet (affected-rows insert-id status warnings message))
           (when wbox (set-box! wbox warnings))
           (vector 'command `((affected-rows . ,affected-rows)
                              (insert-id . ,insert-id)
                              (status . ,status)
                              (message . ,message)))]
          [(struct result-set-header-packet (fields extra))
           (let* ([field-dvecs (query1:get-fields fsym binary?)]
                  [rows (query1:get-rows fsym field-dvecs binary? wbox)])
             (vector 'rows field-dvecs rows))])))

    (define/private (query1:get-fields fsym binary?)
      (let ([r (recv fsym 'field)])
        (match r
          [(? field-packet?)
           (cons (parse-field-dvec r) (query1:get-fields fsym binary?))]
          [(struct eof-packet (warning status))
           null])))

    (define/private (query1:get-rows fsym field-dvecs binary? wbox)
      ;; Note: binary? should always be #t, unless force-prepare-sql? misses something.
      (let ([r (recv fsym (if binary? 'binary-data 'data) field-dvecs)])
        (match r
          [(struct row-data-packet (data))
           (cons data (query1:get-rows fsym field-dvecs binary? wbox))]
          [(struct binary-row-data-packet (data))
           (cons data (query1:get-rows fsym field-dvecs binary? wbox))]
          [(struct eof-packet (warnings status))
           (when wbox (set-box! wbox warnings))
           null])))

    (define/private (query1:process-result fsym result)
      (match result
        [(vector 'rows field-dvecs rows)
         (rows-result (map field-dvec->field-info field-dvecs) rows)]
        [(vector 'command command-info)
         (simple-result command-info)]))

    ;; == Prepare

    ;; prepare : symbol string boolean -> PreparedStatement
    (define/public (prepare fsym stmt close-on-exec?)
      (check-valid-tx-status fsym)
      (call-with-lock fsym
        (lambda ()
          (prepare1 fsym stmt close-on-exec?))))

    (define/private (prepare1 fsym stmt close-on-exec?)
      (fresh-exchange)
      (send-message (make-command-packet 'statement-prepare stmt))
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
                  (stmt-type (classify-my-sql stmt))
                  (owner this)))])))

    (define/private (prepare1:get-field-descriptions fsym)
      (let ([r (recv fsym 'field)])
        (match r
          [(struct eof-packet (warning-count status))
           null]
          [(? field-packet?)
           (cons (parse-field-dvec r) (prepare1:get-field-descriptions fsym))])))

    (define/public (get-base) this)

    (define/public (free-statement pst)
      (call-with-lock* 'free-statement
        (lambda ()
          (let ([id (send pst get-handle)])
            (when (and id outport) ;; outport = connected?
              (send pst set-handle #f)
              (fresh-exchange)
              (send-message (make-command:statement-packet 'statement-close id)))))
        void
        #f))

    ;; == Warnings

    (define/private (fetch-warnings fsym)
      (unless (eq? notice-handler void)
        (let ([result (query1 fsym "SHOW WARNINGS" #f)])
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

    (define/override (start-transaction* fsym isolation)
      (cond [(eq? isolation 'nested)
             (let ([savepoint (generate-name)])
               (query1 fsym (format "SAVEPOINT ~a" savepoint) #t)
               savepoint)]
            [else
             (let ([isolation-level (isolation-symbol->string isolation)])
               (when isolation-level
                 (query1 fsym (format "SET TRANSACTION ISOLATION LEVEL ~a" isolation-level) #t))
               (query1 fsym "START TRANSACTION" #t)
               #f)]))

    (define/override (end-transaction* fsym mode savepoint)
      (case mode
        ((commit)
         (cond [savepoint
                (query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint) #t)]
               [else
                (query1 fsym "COMMIT" #t)]))
        ((rollback)
         (cond [savepoint
                (query1 fsym (format "ROLLBACK TO SAVEPOINT ~a" savepoint) #t)
                (query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint) #t)]
               [else
                (query1 fsym "ROLLBACK" #t)])))
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
              (vector-ref (call-with-lock fsym (lambda () (query1 fsym stmt #t))) 2)])
        (for/list ([row (in-list rows)])
          (vector-ref row 0))))

    ))

;; ========================================

;; scramble-password : bytes string -> bytes
(define (scramble-password scramble password)
  (and scramble password
       (let* ([stage1 (cond [(string? password) (password-hash password)]
                            [(pair? password)
                             (hex-string->bytes (cadr password))])]
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
