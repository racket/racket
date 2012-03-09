#lang racket/base
(require racket/class
         racket/string
         ffi/unsafe/atomic
         "interfaces.rkt")
(provide define-type-table
         locking%
         debugging%
         transactions%
         statement-cache%
         isolation-symbol->string
         make-sql-classifier
         sql-skip-comments
         make-handler
         guess-socket-path/paths)

;; Common connection-implementation code

;; ----------------------------------------

;; Defining type tables

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

;; ----------------------------------------

;; Notice/notification handler maker

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

;; ----------------------------------------

;; Socket paths

(define (guess-socket-path/paths function paths)
  (or (for/or ([path (in-list paths)])
        (and (file-exists? path) path))
      (error function
             "could not find socket path")))

;; ----------------------------------------

(define debugging%
  (class object%
    (super-new)

    (field [DEBUG? #f])

    (define/public (debug debug?)
      (set! DEBUG? debug?))

    (define/public (dprintf fmt . args)
      (when DEBUG? (apply fprintf (current-error-port) fmt args)))
    ))

;; ----------------------------------------

(define locking%
  (class debugging%

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
            [eb? (break-enabled)]
            [result (sync outer-lock lock-holder)])
        (cond [(eq? result outer-lock)
               ;; Got past outer stage
               (break-enabled #f)
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
                          (break-enabled eb?)
                          (unlock #f)
                          (error/not-connected who))
                        (with-handlers ([(lambda (e) #t)
                                         (lambda (e)
                                           (when (exn:break? e) (on-break-within-lock))
                                           (unlock #f)
                                           (raise e))])
                          (break-enabled eb?)
                          (begin0 (proc) (unlock #t)))]
                       [else
                        ;; Didn't acquire lock; retry
                        (break-enabled eb?)
                        (call-with-lock* who proc hopeless require-connected?)]))]
              [(eq? result lock-holder)
               ;; Thread holding lock is dead
               (if hopeless (hopeless) (error/hopeless who))]
              [(eq? me lock-holder)
               (error/internal who "attempted to recursively acquire lock")]
              [else
               ;; lock-holder was stale; retry
               (call-with-lock* who proc hopeless require-connected?)])))

    (define/private (unlock run-async-calls?)
      (let ([async-calls (reverse delayed-async-calls)])
        (set! delayed-async-calls null)
        (start-atomic)
        (set! lock-holder never-evt)
        (semaphore-post inner-lock)
        (semaphore-post outer-sema)
        (end-atomic)
        (when run-async-calls?
          (for-each call-with-continuation-barrier async-calls))))

    ;; needs overriding
    (define/public (connected?) #f)

    (define/public (add-delayed-call! proc)
      (set! delayed-async-calls (cons proc delayed-async-calls)))

    ;; on-break-within-lock : -> void
    ;; Called before unlock; makes it easy to disconnect on any break
    ;; within lock.
    (define/public (on-break-within-lock)
      (void))

    (super-new)))

;; ----------------------------------------

(define disconnect%
  (class locking%
    (inherit dprintf
             call-with-lock*
             connected?)
    (super-new)

    ;; disconnect : -> void
    (define/public (disconnect)
      (when (connected?)
        (call-with-lock* 'disconnect
                         (lambda () (disconnect* #t))
                         (lambda () (disconnect* #f))
                         #f)))

    (define/public (disconnect* politely?)
      (dprintf "  ** disconnecting~a\n" (if politely? " politely" ""))
      (void))

    (define/override (on-break-within-lock)
      (dprintf "  ** break occurred within lock\n")
      (disconnect* #f))))

;; ----------------------------------------

(define transactions%
  (class disconnect%
    (inherit dprintf)
    (inherit-field DEBUG?)

    #|
    A transaction created via SQL is "unmanaged".
    A transaction created via start-tx, call-with-tx is "managed".

    tx-status : #f, #t, 'invalid
    Indicates whether in a transaction (managed or unmanaged) and if
    transaction is valid or invalid.

    tx-stack : (list (cons string boolean) ... (cons #f boolean))
    Represents the "managed" transaction stack.

    If tx-status = #f, then tx-stack = null (except temporarily,
    within lock). But it is possible for tx-status != #f and
    tx-stack = null; that indicates an unmanaged tx.
    |#

    (define tx-status #f)
    (define tx-stack null)

    (define/public (get-tx-status) tx-status)
    (define/public (set-tx-status! fsym s)
      (set! tx-status s))

    ;; check-valid-tx-status : symbol -> void
    (define/public (check-valid-tx-status fsym)
      (when (eq? tx-status 'invalid)
        (uerror fsym "current transaction is invalid")))

    ;; ----

    ;; (inherit call-with-lock)
    (define/override (call-with-lock fsym proc)
      (super call-with-lock fsym
             (lambda ()
               (begin0 (proc)
                 (when DEBUG? (dprintf "  ** ~a\n" (tx-state->string)))
                 (when (and (eq? tx-status #f) (not (null? tx-stack)))
                   (error/internal fsym "managed transaction unexpectedly closed"))))))

    ;; ----

    (define/public (transaction-status fsym)
      (call-with-lock fsym (lambda () tx-status)))

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

    (define/private (transaction-nesting)
      (cond [(eq? tx-status #f) #f]
            [(null? tx-stack) 'unmanaged]
            [(null? (cdr tx-stack)) 'top-level]
            [else 'nested]))

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
       - SAVEPOINT not allowed (for consistency, for ease of stmt cache)
       - RELEASE TO, ROLLBACK TO not allowed (for consistency, for ease of stmt cache)
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
        ((top-level nested)
         (case stmt-type
           ((start)
            (no! " within transaction"))
           ((commit rollback
             savepoint prepare-transaction
             release-savepoint rollback-savepoint
             implicit-commit)
            (no! " within managed transaction"))
           (else (void))))))

    (super-new)))

;; ----------------------------------------

(define statement-cache%
  (class transactions%
    (init-field [cache-statements 'in-transaction])
    (inherit call-with-lock
             get-tx-status
             check-valid-tx-status
             dprintf)
    (super-new)

    (field [max-cache-size 50])

    ;; Statement Cache
    ;; updated by prepare; potentially invalidated by query (via check/invalidate-cache)

    (define pst-cache '#hash())

    (define/public (get-cached-statement stmt)
      (let ([cached-pst (hash-ref pst-cache stmt #f)])
        (cond [cached-pst
               (dprintf "  ** using cached statement\n")
               cached-pst]
              [else
               (dprintf "  ** statement not in cache\n")
               #f])))

    (define/public (safe-statement-type? stmt-type)
      (memq stmt-type '(select insert update delete with)))

    (define/public (cache-statement! pst)
      (when (and (use-cache?) (safe-statement-type? (send pst get-stmt-type)))
        (let ([sql (send pst get-stmt)])
          (when sql
            (dprintf "  ** caching statement\n")
            (set! pst-cache (hash-set pst-cache sql pst))))))

    (define/private (use-cache?)
      (case cache-statements
        ((always) #t)
        ((never) #f)
        ((in-transaction) (eq? (get-tx-status) #t))))

    ;; check/invalidate-cache : statement/pst -> hash/#f
    ;; Returns old cache on invalidation, or #f if stmt is safe.
    ;; May also return part of old cache (excluding pst) when cache gets too big.
    (define/public (check/invalidate-cache x)
      #|
      Sufficient to check on every query execution whether statement type is safe
      (ie, SELECT, INSERT, etc). All statements sent as strings are considered
      unsafe, because they're usually transactional SQL.
      |#
      (define (invalidate! except)
        (dprintf "  ** invalidating statement cache~a\n" (if except " (too big)" ""))
        (let ([cache pst-cache])
          (set! pst-cache '#hash())
          (cond [except
                 (cache-statement! except)
                 (hash-remove cache (send except get-stmt))]
                [else
                 cache])))
      (cond [(statement-binding? x)
             (check/invalidate-cache (statement-binding-pst x))]
            [(prepared-statement? x)
             (let ([stmt-type (send x get-stmt-type)])
               (cond [(safe-statement-type? stmt-type)
                      (if (< (hash-count pst-cache) max-cache-size)
                          #f
                          (invalidate! x))]
                     [else
                      (invalidate! #f)]))]
            [else (invalidate! #f)]))

    ;; Prepare

    (define/public (prepare fsym stmt close-on-exec?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (prepare1 fsym stmt close-on-exec?))))

    (define/public (prepare1 fsym stmt close-on-exec?)
      (cond [(and close-on-exec? (use-cache?))
             (or (get-cached-statement stmt)
                 (let* ([stmt-type (classify-stmt stmt)]
                        [safe? (safe-statement-type? stmt-type)]
                        [pst (prepare1* fsym stmt (if safe? #f close-on-exec?) stmt-type)])
                   (when safe? (cache-statement! pst))
                   pst))]
            [else
             (dprintf "  ** not using statement cache\n")
             (prepare1* fsym stmt close-on-exec? (classify-stmt stmt))]))

    (define/public (prepare1* fsym stmt close-on-exec?)
      (error/internal 'prepare1* "not implemented"))

    (define/public (classify-stmt stmt)
      (error/internal 'classify-stmt "not implemented"))

    ))

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
