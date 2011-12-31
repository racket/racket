#lang racket/base
(require racket/class
         racket/string
         ffi/unsafe/atomic)
(provide connection<%>
         dbsystem<%>
         prepared-statement<%>

         (struct-out simple-result)
         (struct-out rows-result)

         (struct-out statement-binding)

         init-private

         define-type-table

         locking%
         transactions%

         isolation-symbol->string
         make-sql-classifier
         sql-skip-comments

         hex-string->bytes

         make-handler
         guess-socket-path/paths

         dblogger
         dbdebug

         (struct-out exn:fail:sql)
         raise-sql-error)

;; ==== Connection

;; connection<%>
(define connection<%>
  (interface ()
    connected?    ;; -> boolean
    disconnect    ;; -> void
    get-dbsystem  ;; -> dbsystem<%>
    query         ;; symbol statement -> QueryResult
    prepare       ;; symbol preparable boolean -> prepared-statement<%>
    get-base      ;; -> connection<%> or #f (#f means base isn't fixed)
    list-tables   ;; symbol symbol -> (listof string)

    ;; in start-tx and end-tx, the final boolean arg indicates whether the
    ;; transaction is managed manually (#f) or by call-with-tx (#t)
    start-transaction  ;; symbol (U 'serializable ...) boolean -> void
    end-transaction    ;; symbol (U 'commit 'rollback) boolean -> void
    transaction-status ;; symbol -> (U boolean 'invalid)
    free-statement))   ;; prepared-statement<%> -> void

;; ==== DBSystem

;; dbsystem<%>
;; Represents brand of database system, SQL dialect, etc
(define dbsystem<%>
  (interface ()
    get-short-name         ;; -> symbol

    get-parameter-handlers ;; (listof typeid) -> (listof ParameterHandler)
    field-dvecs->typeids   ;; (listof field-dvec) -> (listof typeid)

    ;; inspection only
    get-known-types        ;; -> (listof symbol)
    describe-typeids))     ;; (listof typeid) -> (listof TypeDesc)


;; ParameterHandler = (fsym index datum -> ???)
;; Each system gets to choose its checked-param representation.
;; Maybe check and convert to string. Maybe just check, do binary conversion later.

;; TypeDesc = (list boolean symbol/#f typeid)

;; ==== Prepared

;; prepared-statement<%>
(define prepared-statement<%>
  (interface ()
    get-handle         ;; -> Handle (depends on database system)
    set-handle         ;; Handle -> void

    after-exec         ;; -> void (for close-after-exec)

    get-param-count    ;; -> nat or #f
    get-param-typeids  ;; -> (listof typeid)

    get-result-dvecs   ;; -> (listof vector)
    get-result-count   ;; -> nat or #f
    get-result-typeids ;; -> (listof typeid) or #f

    check-owner        ;; symbol connection any -> #t (or error)
    bind               ;; symbol (listof param) -> statement-binding

    ;; extension hooks: usually shouldn't need to override
    finalize           ;; -> void

    ;; inspection only
    get-param-types    ;; -> (listof TypeDesc)
    get-result-types   ;; -> (listof TypeDesc)
    ))


;; ==== Auxiliary structures

;; A statement-binding is:
;;   - (statement-binding prepared-statement ??? (listof ???))
;;     meta might include information such as text vs binary format
(struct statement-binding (pst meta params))

;; A YesNoOptional is one of 'yes, 'no, 'optional
;; An SSLMode is one of 'sslv2-or-v3, 'sslv2, 'sslv3, 'tls

;; An query-result is one of:
;;  - (simple-result alist)
;;  - (rows-result Header data)
;;    for user-visible rows-results: headers present, data is (listof vector)
(struct simple-result (info) #:transparent)
(struct rows-result (headers rows) #:transparent)

;; A Header is (listof FieldInfo)
;; A FieldInfo is an alist, contents dbsys-dependent


;; === Class utilities

;; Here just because ...

(define-syntax-rule (init-private iid ...)
  (begin (init-private1 iid) ...))

(define-syntax-rule (init-private1 iid)
  (begin (init ([private-iid iid]))
         (define iid private-iid)))


;; === Util for defining type tables

(define-syntax-rule (define-type-table (supported-types
                                        type-alias->type
                                        typeid->type
                                        type->typeid
                                        describe-typeid)
                      (typeid type (alias ...) supported?) ...)
  (begin
    (define all-types '((type supported?) ...))
    (define supported-types
      (sort (map car (filter cadr all-types))
            string<?
            #:key symbol->string
            #:cache-keys? #t))
    (define (type-alias->type x)
      (case x
        ((alias ...) 'type) ...
        (else x)))
    (define (typeid->type x)
      (case x
        ((typeid) 'type) ...
        (else #f)))
    (define (type->typeid x)
      (case x
        ((type) 'typeid) ...
        (else #f)))
    (define (describe-typeid x)
      (let ([t (typeid->type x)]
            [ok? (case x ((typeid) supported?) ... (else #f))])
        (list ok? t x)))))


;; == Notice/notification handler maker

;; make-handler : output-port/symbol string -> string string -> void
(define (make-handler out header)
  (if (procedure? out)
      out
      (lambda (code message)
        (fprintf (case out
                   ((output) (current-output-port))
                   ((error) (current-error-port))
                   (else out))
                 "~a: ~a (SQLSTATE ~a)\n" header message code))))

;; == Socket paths

(define (guess-socket-path/paths function paths)
  (or (for/or ([path (in-list paths)])
        (and (file-exists? path) path))
      (error function
             "could not find socket path")))

;; ----------------------------------------

;; Connection base class (locking)

(define locking%
  (class object%

    ;; == Communication locking

    ;; Goal: we would like to be able to detect if a thread has
    ;; acquired the lock and then died, leaving the connection
    ;; permanently locked.
    ;;
    ;; lock-holder=(thread-dead-evt thd) iff thd has acquired inner-lock
    ;;  - lock-holder, inner-lock always modified together within
    ;;    atomic block
    ;;
    ;; Thus if (thread-dead-evt thd) is ready, thd died holding
    ;; inner-lock, so hopelessly locked.
    ;;
    ;; outer-sema = inner-lock
    ;;  - outer-sema, inner-lock always modified together within atomic
    ;;
    ;; The outer-lock just prevents threads from spinning polling
    ;; inner-lock. If a thread gets past outer-lock and dies before
    ;; acquiring inner-lock, ok, because outer-lock still open at that
    ;; point, so other threads can enter outer-lock and acquire inner-lock.

    (define outer-sema (make-semaphore 1))
    (define outer-lock (semaphore-peek-evt outer-sema))
    (define inner-lock (make-semaphore 1))
    (define lock-holder never-evt)

    ;; Delay async calls (eg, notice handler) until unlock
    (define delayed-async-calls null)

    ;; ----

    (define/public (call-with-lock who proc)
      (call-with-lock* who proc #f #t))

    (define/public-final (call-with-lock* who proc hopeless require-connected?)
      (let ([me (thread-dead-evt (current-thread))]
            [result (sync outer-lock lock-holder)])
        (cond [(eq? result outer-lock)
               ;; Got past outer stage
               (let ([proceed?
                      (begin (start-atomic)
                             (let ([proceed? (semaphore-try-wait? inner-lock)])
                               (when proceed?
                                 (set! lock-holder me)
                                 (semaphore-wait outer-sema))
                               (end-atomic)
                               proceed?))])
                 (cond [proceed?
                        ;; Acquired lock
                        ;;  - lock-holder = me, and outer-lock is closed again
                        (when (and require-connected? (not (connected?)))
                          (unlock)
                          (error/not-connected who))
                        (with-handlers ([values (lambda (e) (unlock) (raise e))])
                          (begin0 (proc) (unlock)))]
                       [else
                        ;; Didn't acquire lock; retry
                        (call-with-lock* who proc hopeless require-connected?)]))]
              [(eq? result lock-holder)
               ;; Thread holding lock is dead
               (if hopeless (hopeless) (error/hopeless who))]
              [else
               ;; lock-holder was stale; retry
               (call-with-lock* who proc hopeless require-connected?)])))

    (define/private (unlock)
      (let ([async-calls (reverse delayed-async-calls)])
        (set! delayed-async-calls null)
        (start-atomic)
        (set! lock-holder never-evt)
        (semaphore-post inner-lock)
        (semaphore-post outer-sema)
        (end-atomic)
        (for-each call-with-continuation-barrier async-calls)))

    ;; needs overriding
    (define/public (connected?) #f)

    (define/public-final (add-delayed-call! proc)
      (set! delayed-async-calls (cons proc delayed-async-calls)))

    (super-new)))

;; ----------------------------------------

(define transactions%
  (class locking%
    (inherit call-with-lock)

    #|
    A transaction created via SQL is "unmanaged".
    A transaction created via start-tx, call-with-tx is "managed".

    FIXME: eliminate distinction, if possible.
      - currently: tx-stack != null means tx-status != #f
      - would also like: tx-stack = null iff tx-status = #f
    |#

    ;; tx-status : #f, #t, 'invalid
    (field [tx-status #f])

    ;; tx-stack : (list (cons string boolean) ... (cons #f boolean)
    ;; Represents the "managed" transaction stack.
    (field [tx-stack null])

    ;; check-valid-tx-status : symbol -> void
    (define/public (check-valid-tx-status fsym)
      (when (eq? tx-status 'invalid)
        (uerror fsym "current transaction is invalid")))

    ;; ----

    (define/public (transaction-status fsym)
      (call-with-lock fsym (lambda () tx-status)))

    ;; transaction-nesting : -> (U #f 'unmanaged 'top-level 'nested)
    (define/public (transaction-nesting)
      (cond [(eq? tx-status #f) #f]
            [(null? tx-stack) 'unmanaged]
            [(null? (cdr tx-stack)) 'top-level]
            [else 'nested]))

    (define/public (tx-state->string)
      (string-append (case (transaction-nesting)
                       ((#f) "not in transaction")
                       ((unmanaged) "in unmanaged transaction")
                       ((top-level nested) "in managed transaction"))
                     (let ([savepoints (filter string? (map car tx-stack))])
                       (if (pair? savepoints)
                           (string-append "; savepoints: "
                                          (string-join savepoints ", "))
                           ""))))

    ;; ----

    (define/public (start-transaction fsym isolation cwt?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (cond [(not tx-status)
                 (start-transaction* fsym isolation)
                 (set! tx-stack (list (cons #f cwt?)))]
                [else ;; in transaction
                 (unless (eq? isolation #f)
                   (error fsym "invalid isolation level for nested transaction: ~e" isolation))
                 (let ([savepoint (start-transaction* fsym 'nested)])
                   (set! tx-stack (cons (cons savepoint cwt?) tx-stack)))])))
      (void))

    (define/public (start-transaction* fsym isolation)
      ;; returns string (savepoint name) if isolation = 'nested, #f otherwise
      (error/internal fsym "not implemented"))

    (define/public (end-transaction fsym mode cwt?)
      (call-with-lock fsym
        (lambda ()
          (unless (eq? mode 'rollback)
            ;; PostgreSQL: otherwise COMMIT statement would cause silent ROLLBACK!
            (check-valid-tx-status fsym))
          (define tx-stack*
            (cond [(and (eq? mode 'rollback) cwt?)
                   ;; Need to rollback any open start-tx transactions within call-with-tx.
                   ;; No need to complain, because cwt/rollback means exn already raised,
                   ;; either by thunk or commit attempt.
                   (let loop ([tx-stack* tx-stack])
                     (cond [(pair? tx-stack*)
                            (if (cdar tx-stack*)
                                tx-stack*
                                (loop (cdr tx-stack*)))]
                           [else
                            (error/internal "unmatched end of call-with-transaction")]))]
                  [else tx-stack]))
          (cond [(pair? tx-stack*)
                 (let ([savepoint (caar tx-stack*)]
                       [saved-cwt? (cdar tx-stack*)])
                   (unless (eq? saved-cwt? cwt?)
                     (case saved-cwt?
                       ((#f) ;; saved-cwt = #f, cwt = #t
                        (error/unclosed-tx fsym mode #t))
                       ((#t) ;; saved-cwt = #t, cwt = #f: possible
                        (error/unbalanced-tx fsym mode #t))))
                   (end-transaction* fsym mode savepoint)
                   (set! tx-stack (cdr tx-stack*)))]
                [else  ;; not in managed transaction
                 (when #f  ;; DISABLED!
                   #|
                   FIXME: Unmatched {commit,rollback}-transaction should
                   probably be illegal outside of transaction for consistency
                   with requirements within call-with-tx. But that would break
                   backwards compatibility, so disabled.
                   |#
                   (error/unbalanced-tx fsym mode #f))
                 (when tx-status
                   ;; Allow closing unmanaged transaction
                   (end-transaction* fsym mode #f))])
          (void))))

    (define/public (end-transaction* fsym mode savepoint)
      (error/internal fsym "not implemented"))

    ;; check-statement/tx-status : symbol symbol/#f -> void
    ;; Used to check whether SQL command is allowed given managed tx status.
    (define/public (check-statement/tx fsym stmt-type)
      #|
      Nested transaction safety

      For simplicity, we put rules for all statement types here, including
      non-standard statements. FIXME: need to decouple eventually.

      if in "unmanaged" top-level transaction
       - allow all SQL commands (but restrict tx functions)
       - yes, even implicit-commit

      if in "managed" top-level transaction (no "managed" savepoints):
       - START not allowed
       - COMMIT, ROLLBACK not allowed (for now!)
       - SAVEPOINT allowed
       - RELEASE TO, ROLLBACK TO allowed
       - implicit-commit not allowed

      if in nested "managed" transaction (impl as "managed" savepoint):
       - START not allowed
       - COMMIT, ROLLBACK not allowed
       - SAVEPOINT not allowed -- because it could not be used; see next
       - RELEASE TO, ROLLBACK TO not allowed -- because it may cross nesting levels
       - implicit-commit now allowed
      |#

      (define (no! why)
        (error fsym "~a not allowed~a"
               (or (statement-type->string stmt-type)
                   (case stmt-type
                     ((implicit-commit) "statement with implicit commit")
                     (else "unknown")))
               (or why "")))

      (case (transaction-nesting)
        ((#f)
         (void))
        ((unmanaged)
         (void))
        ((top-level)
         (case stmt-type
           ((start)
            (no! " within transaction"))
           ((commit rollback
             implicit-commit)
            (no! " within managed transaction"))
           (else (void))))
        ((nested)
         (case stmt-type
           ((start)
            (no! " within transaction"))
           ((commit rollback
             savepoint prepare-transaction
             release-savepoint rollback-savepoint
             implicit-commit)
            (no! " in managed transaction"))
           (else (void))))))

    (super-new)))

;; ----------------------------------------

;; Isolation levels

(define (isolation-symbol->string isolation)
  (case isolation
    ((serializable)     "SERIALIZABLE")
    ((repeatable-read)  "REPEATABLE READ")
    ((read-committed)   "READ COMMITTED")
    ((read-uncommitted) "READ UNCOMMITTED")
    (else #f)))

;; ----------------------------------------

;; Simple SQL "parsing" (just classification)

(define (make-sql-classifier table-spec
                             #:hash-comments? [hash-comments? #f])
  (define (make-sql-regexp stmt-str)
    ;; eg, turns "alter table" into #px"^[[:space:]]*(?i:alter)[[:space:]](?i:table)"
    ;; FIXME/TODO: comments (need real tokenizer; keep regexps as fast path?)
    (pregexp
     (apply string-append
            "^"
            (for/list ([piece (in-list (regexp-split #rx" " stmt-str))])
              (format "[[:space:]]*(?i:~a)(?i:[[:space:]]|$)" piece)))))
  (define classifier-table
    (for/list ([rule-spec (in-list table-spec)])
      (cons (make-sql-regexp (car rule-spec)) (cadr rule-spec))))
  (lambda (str [start 0])
    (let ([start (sql-skip-comments str start #:hash-comments? hash-comments?)])
      (for/first ([rule (in-list classifier-table)]
                  #:when (regexp-match? (car rule) str start))
        (cdr rule)))))

;; sql-skip-comments : string nat -> nat
(define (sql-skip-comments str start #:hash-comments? [hash-comments? #f])
  (define dash-rx    #px"^[[:space:]]*-- [^\n\r]*(?:[\n\r]|$)")
  (define sh-like-rx #px"^[[:space:]]*#[^\n\r]*(?:[\n\r]|$)")
  (define c-like-rx  #px"^[[:space:]]*/\\*(?:[^\\*]|\\*[^/])*\\*/")
  (let loop ([start start])
    (cond [(or (regexp-match-positions dash-rx str start)
               (regexp-match-positions c-like-rx str start)
               (and hash-comments?
                    (regexp-match-positions sh-like-rx str start)))
           => (lambda (pl) (loop (cdar pl)))]
          [else start])))

;; statement-type->string : symbol -> string/#f
(define (statement-type->string stmt-type)
  (case stmt-type
    ;; standard
    ((start) "START TRANSACTION")
    ((commit) "COMMIT")
    ((rollback) "ROLLBACK")
    ((savepoint) "SAVEPOINT")
    ((release-savepoint) "RELEASE SAVEPOINT")
    ((rollback-savepoint) "ROLLBACK TO SAVEPOINT")
    ;; postgresql extensions
    ((prepare-transaction) "PREPARE TRANSACTION")
    ;; unknown
    (else #f)))

;; ----------------------------------------

;; Passwords

#|
;; Also in file/sha1
(define (bytes->hex-string b)
  (define (int->hex-digit n)
    (string-ref "0123456789abcdef" n))
  (let* ([c (bytes-length b)]
         [s (make-string (* 2 c))])
    (for ([i (in-range c)])
      (string-set! s (+ i i 0) (int->hex-digit (arithmetic-shift (bytes-ref b i) -4)))
      (string-set! s (+ i i 1) (int->hex-digit (bitwise-and (bytes-ref b) #xFF))))
    s))
|#

(define (hex-string->bytes s)
  (define (hex-digit->int c)
    (let ([c (char->integer c)])
      (cond [(<= (char->integer #\0) c (char->integer #\9))
             (- c (char->integer #\0))]
            [(<= (char->integer #\a) c (char->integer #\f))
             (- c (char->integer #\a))]
            [(<= (char->integer #\A) c (char->integer #\F))
             (- c (char->integer #\A))])))
  (unless (and (string? s) (even? (string-length s))
               (regexp-match? #rx"[0-9a-zA-Z]*" s))
    (raise-type-error 'hex-string->bytes
                      "string containing an even number of hexadecimal digits" s))
  (let* ([c (quotient (string-length s) 2)]
         [b (make-bytes c)])
    (for ([i (in-range c)])
      (let ([high (hex-digit->int (string-ref s (+ i i)))]
            [low  (hex-digit->int (string-ref s (+ i i 1)))])
        (bytes-set! b i (+ (arithmetic-shift high 4) low))))
    b))

;; ----------------------------------------

;; Logging

(define dblogger (make-logger 'db (current-logger)))

(define (dbdebug fmt . args)
  (log-message dblogger 'debug (apply format fmt args) #f))

;; ----------------------------------------

#|
Exceptions

Only errors with an associated SQLSTATE are represented by
exn:fail:sql, specifically only errors originating from a database
backend or library. Other errors are typically raised using 'error',
producing plain old exn:fail.
|#

;; exn:fail:sql
;; Represents an error with an associated SQLSTATE
(define-struct (exn:fail:sql exn:fail) (sqlstate info))

;; raise-sql-error : symbol string string alist -> raises exn
(define (raise-sql-error who sqlstate message info)
  (raise 
   (make-exn:fail:sql (format "~a: ~a (SQLSTATE ~a)" who message sqlstate)
                      (current-continuation-marks)
                      sqlstate
                      info)))

;; ----------------------------------------

;; Common Errors

(provide uerror
         error/internal
         error/not-connected
         error/need-password
         error/comm
         error/hopeless
         error/unsupported-type
         error/no-convert)

;;(define uerror raise-user-error)
(define uerror error)

(define (error/internal fsym fmt . args)
  (apply error fsym (string-append "internal error: " fmt) args))

(define (error/not-connected fsym)
  (uerror fsym "not connected"))

(define (error/need-password fsym)
  (uerror fsym "password needed but not supplied"))

(define (error/comm fsym [when-occurred #f])
  (if when-occurred
      (error/internal fsym "communication problem ~a" when-occurred)
      (error/internal fsym "communication problem")))

(define (error/hopeless fsym)
  (uerror fsym "connection is permanently locked due to a terminated thread"))

(define (error/unsupported-type fsym typeid [type #f])
  (if type
      (uerror fsym "unsupported type: ~a (typeid ~a)" type typeid)
      (uerror fsym "unsupported type: (typeid ~a)" typeid)))

(define (error/no-convert fsym sys type param [note #f])
  (uerror fsym "cannot convert to ~a ~a type~a~a: ~e"
          sys type (if note " " "") (or note "") param))

(define (error/unbalanced-tx fsym mode saved-cwt?)
  (error fsym "~a-transaction without matching start-transaction~a"
         mode (if saved-cwt? " (within the extent of call-with-transaction)" "")))

(define (error/unclosed-tx fsym mode saved-cwt?)
  (error fsym "unclosed nested transaction~a"
         (if saved-cwt? " (within extent of call-with-transaction)" "")))
