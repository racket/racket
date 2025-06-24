#lang racket/base

(require compiler/cm
         racket/list
         racket/match
         racket/path
         racket/serialize
         "private/cc-struct.rkt"
         setup/parallel-do
         setup/path-to-relative
         racket/class
         compiler/find-exe
         racket/place
         syntax/modresolve
         "private/format-error.rkt"
         "private/time.rkt"
         (for-syntax racket/base))


(provide parallel-compile
         parallel-compile-files)

(define-syntax-rule (DEBUG_COMM a ...)
  (void)
;  (begin a ...)
)

(struct parallel-compile-event (worker value) #:prefab)
;; Logger that joins the events of the compiler/cm logger of the different places.
;; The attached values are (parallel-compile-event <worker-id> <original-data>).
(define pb-logger (make-logger 'setup/parallel-build (current-logger)))

(define-logger concurrentometer)

(define lock-manager% 
  (class object%
    (init-field worker-count [blocked-notify void] [unblocked-notify void])
    (define locks (make-hash))
    (define depends (make-hash))
    (define currently-idle 0)
    (define/private (idle! delta)
      (set! currently-idle (+ currently-idle delta))
      (log-concurrentometer-debug "~s" (- worker-count currently-idle)))
    (define/public (lock fn wrkr)
      (let ([v (hash-ref locks fn #f)])
        (hash-set!
         locks fn
         (match v
           ['done
            (wrkr/send wrkr (list 'compiled))
            'done]
           [(list w waitlst) 
            (let ([fns (check-cycles w (hasheq wrkr #t) null)])
              (cond
               [fns
                (wrkr/send wrkr (list 'cycle (cons fn fns)))
                v]
               [else
                (idle! +1)
                (hash-set! depends wrkr (cons w fn))
                (blocked-notify wrkr fn)
                (list w (append waitlst (list wrkr)))]))]
           [_
            (wrkr/send wrkr (list 'locked))
            (list wrkr null)]))
        (not v)))
    (define/public (unlock fn)
      (match (hash-ref locks fn)
        [(list w waitlst)
         (for ([x (in-list waitlst)])
            (idle! -1)
            (hash-remove! depends x)
            (unblocked-notify x)
            (wrkr/send x (list 'compiled)))
          (hash-set! locks fn 'done)]))
    (define/private (check-cycles w seen fns)
      (cond
       [(hash-ref seen w #f) fns]
       [(hash-ref depends w #f)
        => (lambda (d)
             (check-cycles (car d) (hash-set seen w #t) (cons (cdr d) fns)))]
       [else #f]))
    (super-new)))

(define/class/generics lock-manager%
  (lm/lock lock fn wrkr)
  (lm/unlock unlock fn))

(define (->bytes x)
  (cond [(path? x) (path->bytes x)]
        [(string? x) (string->bytes/locale x)]
        [(equal? x 'relative) (->bytes (path->complete-path (current-directory)))]
        [else (raise-argument-error '->bytes "(or/c path? string? 'relative)" x)]))

(define (dir->bytes x)
  (cond [(path? x) (path->bytes (path->complete-path x))]
        [else (->bytes x)]))

(define collects-queue% 
  (class* object% (work-queue<%>) 
    (init-field cclst printer append-error options)
    (init worker-count)
    (define lock-mgr
      (let ([cache (make-hash)])
        (new lock-manager%
             [worker-count worker-count]
             [blocked-notify
              (λ (wrkr fn)
                (define id (send wrkr get-id))
                (printer
                 (current-output-port)
                 #:n id
                 #:only-if-terminal? #t
                 (format "~a awaiting" id)
                 "~a"
                 (path->relative-string/library
                  (bytes->path fn)
                  #:cache cache)))]
             [unblocked-notify
              (λ (wrkr)
                (say-making
                 (send wrkr get-id)
                 #:only-if-terminal? #t))])))

    ;; assigned-ccs : (hash natural?[workerid] -o> (μX. (or/c '() (cons (list cc? (listof file?) X) X))))
    (define assigned-ccs (make-hash))
    (inspect #f)

    (define/public (work-done work wrkr msg)
      (define id (send wrkr get-id))
      (match (list work msg)
        [(list (list cc file last) (list result-type out err))
          (begin0
            (match result-type
              [(list 'ERROR long-msg short-msg)
                (append-error cc "making" (cons long-msg short-msg) out err "error")
                (say-idle id)
                #t]
              [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
              [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
              [(list 'LOG level msg data)
               (when (log-level? pb-logger level)
                 (log-message pb-logger level msg (parallel-compile-event id data)))
               #f]
              ['DONE
               (say-idle id)
               (cond
                 [#f
                  ;; treat output as an error:
                  (define (string-!empty? s) (not (zero? (string-length s))))
                  (when (ormap string-!empty? (list out err))
                    (append-error cc "making" #f out err "output"))]
                 [else
                  ;; copy output to here:
                  (display out)
                  (display err (current-error-port))])
               ;(when last (printer (current-output-port) "made" "~a" (cc-name cc)))
               #t]
              [_ (eprintf "Failed trying to match:\n~s\n" result-type)]))]
        [(list _ (list 'ADD fn))
         ;; Currently ignoring queued individual files
         #f]
        [_
          (match work 
            [(list-rest (list cc file last) message)
              (append-error cc "making" #f "" "" "error")
              (eprintf "work-done match cc failed.\n")
              (eprintf "trying to match:\n~e\n" (list work msg))
              #t]
            [_
              (eprintf "work-done match cc failed.\n")
              (eprintf "trying to match:\n~e\n" (list work msg))
              (eprintf "FATAL\n")
              (exit 1)])]))

    (define/public (get-job workerid)
      (define (extract-job-from-assigned-cc id)
        (cond
          [(hash-has-key? assigned-ccs id)
           (let loop ()
             (match (hash-ref assigned-ccs id)
               [(list)
                (hash-remove! assigned-ccs id)
                #f]
               [(cons (list cc (list) subs) tail)
                (hash-set! assigned-ccs id (append subs tail))
                (loop)]
               [(cons (list cc (cons file ft) subs) tail)
                (hash-set! assigned-ccs id (cons (list cc ft subs) tail))
                (vector cc file (null? ft))]))]
          [else #f]))

      (define cc-before
        (match (hash-ref assigned-ccs workerid #f)
          [(cons (list cc files subs) ccs) cc]
          [_ #f]))

      (define-values (stolen-work? next-assigned-job)
        (let/ec got-it

          ;; try the next file in our already assigned ccs
          (define next-assigned-job-in-given-cc (extract-job-from-assigned-cc workerid))
          (when next-assigned-job-in-given-cc (got-it #f next-assigned-job-in-given-cc))

          ;; try to grab an unallocated cc
          (let loop ()
            (when (pair? cclst)
              (define new-cc (list (car cclst)))
              (set! cclst (cdr cclst))
              (hash-set! assigned-ccs workerid new-cc)
              (define job-in-new-cc (extract-job-from-assigned-cc workerid))
              (when job-in-new-cc (got-it #f job-in-new-cc))
              (loop)))

          ;; try to steal some work from someone else's ccs
          (for ([fellow-id (in-list (sort (hash-keys assigned-ccs) <))])
            (define job-in-fellows-cc (extract-job-from-assigned-cc fellow-id))
            (when job-in-fellows-cc
              (match-define (vector cc _ _) job-in-fellows-cc)
              ;; update hash to record which cc we're working
              ;; in so that `say-making` knows what to say
              (hash-set! assigned-ccs workerid (list (list cc (list) (list))))
              (got-it #t job-in-fellows-cc)))

          (error 'parallel-build.rkt "found no work (but we should have)")))

      (match-define (vector cc file last) next-assigned-job)
      (define same-cc-as-last-time?
        (cond
          [(not cc-before) #f]
          [else (equal? (cc-name cc-before) (cc-name cc))]))

      (say-making workerid #:only-if-terminal? (or stolen-work? same-cc-as-last-time?))

      (values
       (list cc file last)
       (list (->bytes (cc-name cc))
             (->bytes (build-path (cc-path cc) file))
             options)))

    (define/public (say-making id #:only-if-terminal? [only-if-terminal? #f])
      (match (hash-ref assigned-ccs id #f)
        [(cons (list cc files subs) _)
         (printer (current-output-port)
                  #:n id
                  #:only-if-terminal? only-if-terminal?
                  #:%age (/ (- original-count (jobs-cnt)) original-count)
                  (format "~a making" id)
                  "~a"
                  (cc-name cc))]
        [_ (say-idle id)]))

    (define/private (say-idle id)
      (printer
       (current-output-port)
       #:n id
       #:only-if-terminal? #t
       (format "~a" id)
       "idle"))

      (define/public (has-jobs?)
        (define (hasjob?  cct)
          (let loop ([cct cct])
            (ormap (lambda (x) (or (pair? (second x)) (loop (third x)))) cct)))

        (or (hasjob? cclst)
            (for/or ([cct (in-hash-values assigned-ccs)])
              (hasjob? cct))))

      (define/public (jobs-cnt)
        (define (count-cct cct)
          (let loop ([cct cct])
            (apply + (map (lambda (x) (+ (length (second x)) (loop (third x)))) cct))))

        (+ (count-cct cclst)
           (for/fold ([cnt 0]) ([cct (in-hash-values assigned-ccs)])
              (+ cnt (count-cct cct)))))
      (define/public (get-results) (void))
      (super-new)

    (define original-count (jobs-cnt))))

(define file-list-queue% 
  (class* object% (work-queue<%>) 
    (init-field filelist handler options)
    (init worker-count)
    (define lock-mgr (new lock-manager% [worker-count worker-count]))
    (define results (void))
    (inspect #f)

    (define seen
      (for/hash ([k (in-list filelist)])
        (values k #t)))

    (define/public (work-done work wrkr msg)
      (define id (send wrkr get-id))
      (match msg
        [(list result-type out err)
          (match result-type
            [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
            [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
            [(list 'ERROR long-msg short-msg)
             (handler id 'error work long-msg out err)
             (set! results #f)
             #t]
            [(list 'LOG level msg data)
             (when (log-level? pb-logger level)
               (log-message pb-logger level msg (parallel-compile-event id data)))
             #f]
            ['DONE
              (define (string-!empty? s) (not (zero? (string-length s))))
              (if (ormap string-!empty? (list out err))
                (handler id 'output work "" out err)
                (handler id 'done work "" "" ""))
              #t])]
        [(list 'ADD fn)
         (unless (hash-ref seen fn #f)
           (set! filelist (cons fn filelist))
           (set! seen (hash-set seen fn #t)))
         #f]
        [_
          (handler id 'fatalerror (format "Error matching work: ~a queue ~a" work filelist) "" "") #t]))
           
      (define/public (get-job workerid)
        (match filelist
          [(cons hd tail)
           (define-values (dir file b) (split-path hd))
           (set! filelist tail)
           (handler workerid 'start hd "" "" "")
           (values hd (list (->bytes hd) (->bytes (path->complete-path hd)) options))]))

      (define/public (has-jobs?) (not (null? filelist)))
      (define/public (jobs-cnt) (length filelist))
      (define/public (get-results) results)
      (super-new)))

(define (parallel-build work-queue worker-count #:use-places? use-places?)
  (define do-log-forwarding (log-level? pb-logger 'info 'setup/parallel-build))
  (parallel-do
   #:use-places? use-places?
    worker-count
    (lambda (workerid) (list workerid do-log-forwarding))
    work-queue
    (define-worker (parallel-compile-worker worker-id do-log-forwarding)
      (DEBUG_COMM (eprintf "WORKER ~a\n" worker-id))
      (define prev-uncaught-exception-handler (uncaught-exception-handler))
      (uncaught-exception-handler 
       (lambda (x)
         (when (exn:break? x) (exit 1))
         (prev-uncaught-exception-handler x)))

      (define cmc (make-caching-managed-compile-zo))
      (match-message-loop
        [(list name _full-file options)
          (DEBUG_COMM (eprintf "COMPILING ~a ~a ~a\n" worker-id name _full-file))
          (define full-file (bytes->path _full-file))
          (define-values (dir file _) (split-path full-file))
          (define out-str-port (open-output-string))
          (define err-str-port (open-output-string))
          (define cip (current-input-port))
          (define cop (current-output-port))
          (define cep (current-error-port))
          (define (send/recv msg) (send/msg msg) (recv/req))
          (define (send/add fn) (send/msg (list 'ADD fn)))
          (define (send/resp type)
            (send/msg (list type (get-output-string out-str-port) (get-output-string err-str-port))))
          (define (pp x) (fprintf cep "COMPILING ~a ~a ~a ~a\n" worker-id name file x))
          (define (lock-client cmd fn)
           (match cmd
             ['lock
               (DEBUG_COMM (eprintf "REQUESTING LOCK ~a ~a ~a\n" worker-id name _full-file))
               (match (send/recv (list (list 'LOCK (path->bytes fn)) "" ""))
                 [(list 'locked) #t]
                 [(list 'cycle fns)
                  (error 'setup "dependency cycle: ~s" fns)]
                 [(list 'compiled) #f]
                 [(list 'DIE) (worker/die 1)]
                 [x (send/error (format "DIDNT MATCH B ~v\n" x))]
                 [_ (send/error (format "DIDNT MATCH B\n"))])]
             ['unlock 
               (DEBUG_COMM (eprintf "UNLOCKING ~a ~a ~a\n" worker-id name _full-file))
              (send/msg (list (list 'UNLOCK (path->bytes fn)) "" ""))]
             [x (send/error (format "DIDNT MATCH C ~v\n" x))]
             [_ (send/error (format "DIDNT MATCH C\n"))]))
         (with-handlers ([exn:fail? (lambda (x)             
                                      (send/resp (list 'ERROR
                                                       ;; Long form shows context:
                                                       (format-error x #:long? #t #:to-string? #t)
                                                       ;; Short form for summary omits context:
                                                       (format-error x #:long? #f #:to-string? #t))))])
           (parameterize ([parallel-lock-client lock-client]
                          [compile-context-preservation-enabled (member 'disable-inlining options )]
                          [compile-enforce-module-constants (not (memq 'disable-constant options))]
                          [manager-trace-handler (if (member 'very-verbose options)
                                                     (lambda (p) (printf "  ~a\n" p))
                                                     (manager-trace-handler))]
                          [current-namespace (make-base-empty-namespace)]
                          [current-directory (if (memq 'set-directory options)
                                                 dir
                                                 (current-directory))]
                          [current-compile-target-machine (if (memq 'compile-any options)
                                                              #f
                                                              (current-compile-target-machine))]
                          [managed-recompile-only (if (memq 'recompile-only options)
                                                      #t
                                                      (managed-recompile-only))]
                          [managed-recompile-cache-dir (let ([dir-bytes (for/or ([o (in-list options)])
                                                                          (and (pair? o)
                                                                               (eq? (car o) 'recompile-cache)
                                                                               (cadr o)))])
                                                         (and dir-bytes
                                                              (bytes->path dir-bytes)))]
                          [current-load-relative-directory dir]
                          [current-input-port (open-input-string "")]
                          [current-output-port out-str-port]
                          [current-error-port err-str-port]
                          ;[manager-compile-notify-handler pp]
                         )

             ;; Watch for module-prefetch events, and queue jobs in response
             (define prefetch-thread (start-prefetch-thread send/add))
             ;; Watch for logging events, and send log messages to parent
             (define stop-logging-thread
               (if do-log-forwarding
                   (start-logging-thread send/log)
                   void))

             (cmc (build-path dir file))

             (kill-thread prefetch-thread)
             (stop-logging-thread))
           (send/resp 'DONE))]
        [x (send/error (format "DIDNT MATCH A ~v\n" x))]
        [_ (send/error (format "DIDNT MATCH A\n"))]))))
  
(define (parallel-compile-files list-of-files
                                #:worker-count [worker-count (processor-count)]
                                #:handler [handler void]
                                #:options [options '()]
                                #:use-places? [use-places? #t])
  (unless (exact-positive-integer? worker-count)
    (raise-argument-error 'parallel-compile-files "exact-positive-integer?" worker-count))
  (unless (and (list? list-of-files) (andmap path-string? list-of-files))
    (raise-argument-error 'parallel-compile-files "(listof path-string?)" list-of-files))
  (unless (and (procedure? handler) (procedure-arity-includes? handler 6))
    (raise-argument-error 'parallel-compile-files "(procedure-arity-includes/c 6)" handler))
  (parallel-build (make-object file-list-queue% list-of-files handler options worker-count) worker-count
                  #:use-places? use-places?))

(define (parallel-compile worker-count setup-fprintf append-error collects-tree
                          #:options [options '()]
                          #:use-places? [use-places? #t])
  (setup-fprintf (current-output-port) #f (add-time
                                           (format "--- parallel build using ~a jobs ---" worker-count)))
  (define collects-queue (make-object collects-queue% collects-tree setup-fprintf append-error
                                      (append options '(set-directory))
                                      worker-count))
  (parallel-build collects-queue worker-count
                  #:use-places? use-places?))

(define (start-prefetch-thread send/add)
  (define pf (make-log-receiver (current-logger) 'info 'module-prefetch))
  (thread
   (lambda ()
     (let loop ()
       (let ([v (sync pf)])
         (define l (vector-ref v 2))
         (when (and (list? l)
                    (= 2 (length l))
                    (list? (car l))
                    (path? (cadr l))
                    (andmap module-path? (car l)))
           (define dir (cadr l))
           (define (quote? p) (and (pair? p) (eq? (car p) 'quote)))
           (define (planet? p) (and (pair? p) (eq? (car p) 'planet)))
           (define (submod? p) (and (pair? p) (eq? (car p) 'submod)))
           ;; Add prefetch modules to work queue --- but skip the first one,
           ;; because it's going to be compiled immediately, anyway:
           (for/fold ([prev #f]) ([p (in-list (reverse (car l)))])
             (cond
              [(or (quote? p)
                   (and (submod? p) (quote? (cadr p))))
               ;; skip `quote` module paths
               prev]
              [(or (planet? p)
                   (and (submod? p) (planet? (cadr p))))
               ;; skip `planet` module paths
               prev]
              [else
               (when prev
                 (define path
                   (let loop ([prev prev])
                     (cond
                       [(submod? prev)
                        (define base (cadr prev))
                        (cond
                          [(or (equal? base "..") (equal? base "."))
                           #f]
                          [else
                           (loop (cadr prev))])]
                       [else (resolve-module-path prev (build-path dir "dummy.rkt"))])))
                 (when (path? path)
                   (send/add path)))
               p])))
         (loop))))))

;; This thread is run in the worker's place. For every compiler event in the worker, this sends a
;; message back to the original place, which will be turned into a log event in `pb-logger`.
(define (start-logging-thread send/log)
  (define log-rec (make-log-receiver (current-logger) 'info 'compiler/cm))
  (define sema (make-semaphore))
  (define t
    (thread
      (lambda ()
        (define (handle-msg v)
          (send/log (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)))
        (define (drain)
          (sync/timeout 0 (handle-evt log-rec (λ (v) (handle-msg v) (drain)))))

        (let loop ()
          (sync
            (handle-evt log-rec
              (λ (v) (handle-msg v) (loop)))
            (handle-evt sema
              (λ (_) (drain))))))))
  (lambda ()
    (semaphore-post sema)
    (sync t)))

