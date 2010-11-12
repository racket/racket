#lang racket/base

(require compiler/cm
         racket/list
         racket/match
         racket/path
         setup/collects
         setup/parallel-do
         unstable/generics)

(provide parallel-compile
         parallel-build-worker)

(define-struct collects-queue (cclst hash collects-dir printer append-error) #:transparent
  #:mutable
  #:property prop:jobqueue
  (define-methods jobqueue
    (define (work-done jobqueue work workerid msg)
      (match (list work msg)
        [(list (list cc file last) (list result-type out err))
          (let ([cc-name (cc-name cc)])
            (match result-type
              [(list 'ERROR msg)
                ((collects-queue-append-error jobqueue) cc "making" (exn msg (current-continuation-marks)) out err "error")]
              ['DONE
                (when (or (not (zero? (string-length out))) (not (zero? (string-length err))))
                  ((collects-queue-append-error jobqueue) cc "making" null out err "output"))])
            (when last ((collects-queue-printer jobqueue) (current-output-port) "made" "~a" cc-name )))]))
    ;; assigns a collection to each worker to be compiled
    ;; when it runs out of collections, steals work from other workers collections
    (define (get-job jobqueue workerid)
      (define (hash/first-pair hash)
         (match (hash-iterate-first hash)
           [#f #f]
           [x (cons (hash-iterate-key hash x) (hash-iterate-value hash x))]))
      (define (hash-ref!/true hash key thunk)
        (hash-ref hash key (lambda ()
                              (match (thunk)
                                [#f #f]
                                [x (hash-set! hash key x) x]))))
      (define (take-cc)
        (match (collects-queue-cclst jobqueue)
          [(list) #f]
          [(cons h t)
            (set-collects-queue-cclst! jobqueue t)
            (list h)]))
      (let ([w-hash (collects-queue-hash jobqueue)])
        (define (build-job cc file last)
          (define (->bytes x)
            (cond [(path? x) (path->bytes x)]
                  [(string? x) (string->bytes/locale x)]))
          (let* ([cc-name (cc-name cc)]
                 [cc-path (cc-path cc)]
                 [full-path (path->string (build-path cc-path file))])
            ;(printf "JOB ~a ~a ~a ~a\n" workerid cc-name cc-path file)
            (values (list cc file last) (list cc-name (->bytes cc-path) (->bytes file)))))
        (let retry ()
          (define (find-job-in-cc cc id)
            (match cc
              [(list)
                (hash-remove! w-hash id) (retry)]
              [(list (list cc (list) (list)))       ;empty collect
                (hash-remove! w-hash id) (retry)]
              [(cons (list cc (list) (list)) tail)  ;empty parent collect
                (hash-set! w-hash id tail) (retry)]
              [(cons (list cc (list) subs) tail)    ;empty srcs list
                (hash-set! w-hash id (append subs tail)) (retry)]
              [(cons (list cc (list file) subs) tail)
                (hash-set! w-hash id (append subs tail))
                  (build-job cc file #t)]
              [(cons (list cc (cons file ft) subs) tail)
                (hash-set! w-hash id (cons (list cc ft subs) tail))
                (build-job cc file #f)]))
          (match (hash-ref!/true w-hash workerid take-cc)
            [#f 
                (match (hash/first-pair w-hash)
                  [(cons id cc) (find-job-in-cc cc id)])]
            [cc (find-job-in-cc cc workerid)]))))
    (define (has-jobs? jobqueue)
      (define (hasjob?  cct)
        (let loop ([cct cct])
          (ormap (lambda (x) (or ((length (second x)) . > . 0) (loop (third x)))) cct)))

      (or (hasjob? (collects-queue-cclst jobqueue))
          (for/or ([cct (in-hash-values (collects-queue-hash jobqueue))])
            (hasjob? cct))))
    (define (jobs-cnt jobqueue)
      (define (count-cct cct)
        (let loop ([cct cct])
          (apply + (map (lambda (x) (+ (length (second x)) (loop (third x)))) cct))))

      (+ (count-cct (collects-queue-cclst jobqueue))
         (for/fold ([cnt 0]) ([cct (in-hash-values (collects-queue-hash jobqueue))])
            (+ cnt (count-cct cct)))))))

(define (parallel-compile worker-count setup-fprintf append-error collects-tree)
  (let ([collects-dir (current-collects-path)]) 
    (setup-fprintf (current-output-port) #f "--- parallel build using ~a processor cores ---" worker-count)
    (parallel-do-event-loop #f
                            values ; identity function
                            (list (current-executable-path)
                                  "-X"
                                  (path->string collects-dir)
                                  "-l"
                                  "setup/parallel-build-worker.rkt")
                            (make-collects-queue collects-tree (make-hash) collects-dir setup-fprintf append-error)
                            worker-count 999999999)))

(define (parallel-build-worker)
  (let ([cmc (make-caching-managed-compile-zo)]
        [worker-id (read)])
   (let loop ()
     (match (read)
       [(list 'DIE) void]
       [(list name dir file)
         (let ([dir (bytes->path dir)]
               [file (bytes->path file)])
          (let ([out-str-port (open-output-string)]
                [err-str-port (open-output-string)])
            (define (send/resp type)
              (let ([msg (list type (get-output-string out-str-port) (get-output-string err-str-port))])
                (write msg)))
            (let ([cep (current-error-port)])
              (define (pp x)
                (fprintf cep "COMPILING ~a ~a ~a ~a\n" worker-id name file x))
            (with-handlers ([exn:fail? (lambda (x)
                             (send/resp (list 'ERROR (exn-message x))))])
              (parameterize (
                             [current-namespace (make-base-empty-namespace)]
                             [current-directory dir]
                             [current-load-relative-directory dir]
                             [current-output-port out-str-port]
                             [current-error-port err-str-port]
                             ;[manager-compile-notify-handler pp]
                            )

                (cmc (build-path dir file)))
              (send/resp 'DONE))))
          (flush-output)
          (loop))]))))
