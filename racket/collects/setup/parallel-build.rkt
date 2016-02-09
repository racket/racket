#lang racket/base

(require compiler/cm
         racket/list
         racket/match
         racket/path
         racket/fasl
         racket/serialize
         "private/cc-struct.rkt"
         setup/parallel-do
         racket/class
         racket/future
         compiler/find-exe
         racket/place
         syntax/modresolve
         "private/format-error.rkt"
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

(define lock-manager% 
  (class object%
    (field (locks (make-hash)))
    (define depends (make-hash))
    (define/public (lock fn wrkr)
      (let ([v (hash-ref locks fn #f)])
        (hash-set!
         locks fn
         (match v
           [(list w waitlst) 
            (hash-set! depends wrkr (cons w fn))
            (let ([fns (check-cycles wrkr (hash) null)])
              (cond
               [fns
                (wrkr/send wrkr (list 'cycle (cons fn fns)))
                v]
               [else
                (list w (append waitlst (list wrkr)))]))]
           [else
            (wrkr/send wrkr (list 'locked))
            (list wrkr null)]))
        (not v)))
    (define/public (unlock fn)
      (match (hash-ref locks fn)
        [(list w waitlst)
          (for ([x (second (hash-ref locks fn))])
            (hash-remove! depends x)
            (wrkr/send x (list 'compiled)))
          (hash-remove! locks fn)]))
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
    (field (lock-mgr (new lock-manager%)))
    (field (hash (make-hash)))
    (inspect #f)

    (define/public (work-done work wrkr msg)
      (define id (send wrkr get-id))
      (match (list work msg)
        [(list (list cc file last) (list result-type out err))
          (begin0
            (match result-type
              [(list 'ERROR long-msg short-msg)
                (append-error cc "making" (cons long-msg short-msg) out err "error")
                #t]
              [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
              [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
              [(list 'LOG level msg data)
               (when (log-level? pb-logger level)
                 (log-message pb-logger level msg (parallel-compile-event id data)))
               #f]
              ['DONE
                (define (string-!empty? s) (not (zero? (string-length s))))
                (when (ormap string-!empty? (list out err))
                  (append-error cc "making" #f out err "output"))
                ;(when last (printer (current-output-port) "made" "~a" (cc-name cc)))
                #t]
              [else (eprintf "Failed trying to match:\n~e\n" result-type)]))]
        [(list _ (list 'ADD fn))
         ;; Currently ignoring queued individual files
         #f]
        [else
          (match work 
            [(list-rest (list cc file last) message)
              (append-error cc "making" #f "" "" "error")
              (eprintf "work-done match cc failed.\n")
              (eprintf "trying to match:\n~e\n" (list work msg))
              #t]
            [else
              (eprintf "work-done match cc failed.\n")
              (eprintf "trying to match:\n~e\n" (list work msg))
              (eprintf "FATAL\n")
              (exit 1)])]))
           
      ;; assigns a collection to each worker to be compiled
      ;; when it runs out of collections, steals work from other workers collections
      (define/public (get-job workerid)
        (define (say-making id x)
          (unless (null? x)
            (printer (current-output-port) 
                     (format "~a making"  id)
                     "~a" 
                     (cc-name (car (car x))))))
        (define (find-job-in-cc cc id)
          (define (retry) (get-job workerid))
          (define (build-job cc file last)
            (values
              (list cc file last)
              (list (->bytes (cc-name cc))
                    (->bytes (build-path (cc-path cc) file))
                    options)))
          (match cc
            [(list)
              (hash-remove! hash id) (retry)]
            [(list (list cc (list) (list)))       ;empty collect
              (hash-remove! hash id) (retry)]
            [(cons (list cc (list) (list)) tail)  ;empty parent collect
              (say-making id tail)
              (hash-set! hash id tail) (retry)]
            [(cons (list cc (list) subs) tail)    ;empty srcs list
              (define nl (append subs tail))
              (say-making id nl)
              (hash-set! hash id nl) (retry)]
            [(cons (list cc (list file) subs) tail)
              (define nl (append subs tail))
              (hash-set! hash id nl)
              (say-making id nl)
              (build-job cc file #t)]
            [(cons (list cc (cons file ft) subs) tail)
              (hash-set! hash id (cons (list cc ft subs) tail))
              (build-job cc file #f)]
            [else
              (eprintf "get-job match cc failed.\n")
              (eprintf "trying to match:\n~v\n" cc)]))


        ; find a cc 
        (cond
          ; lookup already assigned cc 
          [(hash-ref hash workerid #f) => (lambda (x)
            (find-job-in-cc x workerid))]
          ; get next cc from cclst
          [(pair? cclst)
            (define workercc (list (car cclst)))
            (say-making workerid workercc)
            (set! cclst (cdr cclst))
            (hash-set! hash workerid workercc)
            (find-job-in-cc workercc workerid)]
          ; try to steal work from another workers cc
          [(hash-iterate-first hash) => (lambda (x)
            (find-job-in-cc (hash-iterate-value hash x)
                            (hash-iterate-key hash x)))]))
          ; no work left
          ; should never get here, get-job only called when the queue has work

      (define/public (has-jobs?)
        (define (hasjob?  cct)
          (let loop ([cct cct])
            (ormap (lambda (x) (or ((length (second x)) . > . 0) (loop (third x)))) cct)))

        (or (hasjob? cclst)
            (for/or ([cct (in-hash-values hash)])
              (hasjob? cct))))

      (define/public (jobs-cnt)
        (define (count-cct cct)
          (let loop ([cct cct])
            (apply + (map (lambda (x) (+ (length (second x)) (loop (third x)))) cct))))

        (+ (count-cct cclst)
           (for/fold ([cnt 0]) ([cct (in-hash-values hash)])
              (+ cnt (count-cct cct)))))
      (define/public (get-results) (void))
      (super-new)))

(define file-list-queue% 
  (class* object% (work-queue<%>) 
    (init-field filelist handler options)
    (field (lock-mgr (new lock-manager%)))
    (field [results (void)])
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
        [else
          (handler id 'fatalerror (format "Error matching work: ~a queue ~a" work filelist) "" "") #t]))
           
      (define/public (get-job workerid)
        (match filelist
          [(cons hd tail)
           (define-values (dir file b) (split-path hd))
           (set! filelist tail)
           (handler workerid 'start hd "" "" "")
           (values hd (list (->bytes hd) (->bytes (path->complete-path hd)) null))]))

      (define/public (has-jobs?) (not (null? filelist)))
      (define/public (jobs-cnt) (length filelist))
      (define/public (get-results) results)
      (super-new)))

(define (parallel-build work-queue worker-count)
  (define do-log-forwarding (log-level? pb-logger 'info 'setup/parallel-build))
  (parallel-do
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
                 [else (send/error (format "DIDNT MATCH B\n"))])]
             ['unlock 
               (DEBUG_COMM (eprintf "UNLOCKING ~a ~a ~a\n" worker-id name _full-file))
              (send/msg (list (list 'UNLOCK (path->bytes fn)) "" ""))]
             [x (send/error (format "DIDNT MATCH C ~v\n" x))]
             [else (send/error (format "DIDNT MATCH C\n"))]))
         (with-handlers ([exn:fail? (lambda (x)             
                                      (send/resp (list 'ERROR
                                                       ;; Long form shows context:
                                                       (format-error x #:long? #t #:to-string? #t)
                                                       ;; Short form for summary omits context:
                                                       (format-error x #:long? #f #:to-string? #t))))])
           (parameterize ([parallel-lock-client lock-client]
                          [compile-context-preservation-enabled (member 'disable-inlining options )]
                          [manager-trace-handler
                            (lambda (p)
                              (when (member 'very-verbose options)
                                (printf "  ~a\n" p)))]
                          [current-namespace (make-base-empty-namespace)]
                          [current-directory dir]
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
        [else (send/error (format "DIDNT MATCH A\n"))]))))
  
(define (parallel-compile-files list-of-files
                                #:worker-count [worker-count (processor-count)]
                                #:handler [handler void]
                                #:options [options '()])
  (unless (exact-positive-integer? worker-count)
    (raise-argument-error 'parallel-compile-files "exact-positive-integer?" worker-count))
  (unless (and (list? list-of-files) (andmap path-string? list-of-files))
    (raise-argument-error 'parallel-compile-files "(listof path-string?)" list-of-files))
  (unless (and (procedure? handler) (procedure-arity-includes? handler 6))
    (raise-argument-error 'parallel-compile-files "(procedure-arity-includes/c 6)" handler))
  (parallel-build (make-object file-list-queue% list-of-files handler options) worker-count))

(define (parallel-compile worker-count setup-fprintf append-error collects-tree)
  (setup-fprintf (current-output-port) #f "--- parallel build using ~a jobs ---" worker-count)
  (define collects-queue (make-object collects-queue% collects-tree setup-fprintf append-error '()))
  (parallel-build collects-queue worker-count))

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
                       (loop (cadr prev))]
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

