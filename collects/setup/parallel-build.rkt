#lang racket/base

(require compiler/cm
         racket/list
         racket/match
         racket/path
         setup/collects
         setup/parallel-do
         racket/class
         racket/future)

(provide parallel-compile
         parallel-compile-files)


(define Lock-Manager% (class object%
  (field (locks (make-hash)))
  (define/public (lock fn wrkr)
    (let ([v (hash-ref locks fn #f)])
      (hash-set! locks fn
        (if v
          (match v [(list w waitlst) (list w (append waitlst (list wrkr)))])
          (begin
            (wrkr/send wrkr (list 'locked))
            (list wrkr null))))
      (not v)))
  (define/public (unlock fn)
    (match (hash-ref locks fn)
      [(list w waitlst)
        (for ([x (second (hash-ref locks fn))])
          (wrkr/send x (list 'compiled)))
        (hash-remove! locks fn)]))
  (super-new)))

(define/class/generics Lock-Manager%
  (lm/lock lock fn wrkr)
  (lm/unlock unlock fn))

(define (->bytes x)
  (cond [(path? x) (path->bytes x)]
        [(string? x) (string->bytes/locale x)]))

(define CollectsQueue% (class* object% (WorkQueue<%>) 
  (init-field cclst printer append-error)
  (field (lock-mgr (new Lock-Manager%)))
  (field (hash (make-hash)))
  (inspect #f)

  (define/public (work-done work wrkr msg)
    (match (list work msg)
      [(list (list cc file last) (list result-type out err))
        (begin0
          (match result-type
            [(list 'ERROR msg)
              (append-error cc "making" (exn msg (current-continuation-marks)) out err "error")
              #t]
            [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
            [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
            ['DONE
              (define (string-!empty? s) (not (zero? (string-length s))))
              (when (ormap string-!empty? (list out err))
                (append-error cc "making" null out err "output"))
              (when last (printer (current-output-port) "made" "~a" (cc-name cc)))
              #t]))]
      [else
        (match work 
          [(list-rest (list cc file last) message)
            (append-error cc "making" null "" "" "error")
            (eprintf "work-done match cc failed.\n")
            (eprintf "trying to match:\n~a\n" (list work msg))
            #t]
          [else
            (eprintf "work-done match cc failed.\n")
            (eprintf "trying to match:\n~a\n" (list work msg))
            (eprintf "FATAL\n")
            (exit 1)])]))
         
    ;; assigns a collection to each worker to be compiled
    ;; when it runs out of collections, steals work from other workers collections
    (define/public (get-job workerid)
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
        (match cclst
          [(list) #f]
          [(cons h t)
            (set! cclst t)
            (list h)]))
      (let ([w-hash hash])
        (define (build-job cc file last)
          (let* ([cc-name (cc-name cc)]
                 [cc-path (cc-path cc)]
                 [full-path (path->string (build-path cc-path file))])
            ;(printf "JOB ~a ~a ~a ~a\n" workerid cc-name cc-path file)
            (values (list cc file last) (list (->bytes cc-name) (->bytes cc-path) (->bytes file)))))
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
                (build-job cc file #f)]
              [else
                (eprintf "get-job match cc failed.\n")
                (eprintf "trying to match:\n~a\n" cc)]))
 
          (match (hash-ref!/true w-hash workerid take-cc)
            [#f 
                (match (hash/first-pair w-hash)
                  [(cons id cc) (find-job-in-cc cc id)])]
            [cc (find-job-in-cc cc workerid)]))))

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

(define FileListQueue% (class* object% (WorkQueue<%>) 
  (init-field filelist handler)
  (field (lock-mgr (new Lock-Manager%)))
  (inspect #f)

  (define/public (work-done work wrkr msg)
    (match msg
      [(list result-type out err)
        (match result-type
          [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
          [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
          [(list 'ERROR msg) (handler 'error work msg out err) #t]
          ['DONE
            (define (string-!empty? s) (not (zero? (string-length s))))
            (if (ormap string-!empty? (list out err))
              (handler 'output work "" out err)
              (handler 'done work "" "" ""))
            #t])]
      [else
        (handler 'fatalerror (format "Error matching work: ~a queue ~a" work filelist) "" "") #t]))
         
    (define/public (get-job workerid)
      (match filelist
        [(cons hd tail)
            (define-values (dir file b) (split-path hd))
            (set! filelist tail)
            (values hd (list (->bytes hd) (->bytes dir) (->bytes file)))]
        [(list) null]))
    (define/public (has-jobs?) (not (null? filelist)))
    (define/public (jobs-cnt) (length filelist))
    (define/public (get-results) (void))
    (super-new)))


(define (build-parallel-build-worker-args)
  (list (current-executable-path)
        "-X"
        (path->string (current-collects-path))
        "-l"
        "setup/parallel-build-worker.rkt"))
  
(define (parallel-compile-files list-of-files
  #:worker-count [worker-count (processor-count)]
  #:handler [handler (lambda args (void))])

  (parallel-do-event-loop #f
                          values ; identity function
                          (build-parallel-build-worker-args)
                          (make-object FileListQueue% list-of-files handler)
                          worker-count 999999999))

(define (parallel-compile worker-count setup-fprintf append-error collects-tree)
  (setup-fprintf (current-output-port) #f "--- parallel build using ~a processor cores ---" worker-count)
  (parallel-do-event-loop #f
                          values ; identity function
                          (build-parallel-build-worker-args)
                          (make-object CollectsQueue% collects-tree setup-fprintf append-error)
                          worker-count 999999999))
