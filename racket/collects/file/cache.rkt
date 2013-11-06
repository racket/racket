#lang racket/base
(require racket/file
         racket/path
         racket/contract/base)

(provide 
 (contract-out [cache-file
                (->* (path-string?
                      (not/c #f)
                      path-string?
                      (-> any))
                     (#:log-error-string (string? . -> . any)
                                         #:log-debug-string (string? . -> . any)
                                         #:notify-cache-use (string? . -> . any)
                                         #:max-cache-files real?
                                         #:max-cache-size real?
                                         #:evict-before? (hash? hash? . -> . boolean?)
                                         #:exists-ok? any/c)
                     void?)]
               [cache-remove
                (->* (any/c
                      path-string?)
                     (#:log-error-string (string? . -> . any)
                                         #:log-debug-string (string? . -> . any))
                     void?)]))

(define (cache-file dest-file ; the file that `fetch` is supposed to put in place
                    source-key ; use (or write) cache entry with this key
                    cache-dir ; direct to hold cache files and database
                    fetch ; the download operation for cache misses; writes `dest-file`
                    #:log-error-string [log-error-string log-error-string]
                    #:log-debug-string [log-debug-string log-debug-string]
                    #:notify-cache-use [notify-cache-use void]
                    #:max-cache-files [max-cache-files 1024]
                    #:max-cache-size [max-cache-size (* 64 1024 1024)]
                    #:evict-before? [evict-before? (lambda (a b)
                                                     (< (hash-ref a 'modify-seconds)
                                                        (hash-ref b 'modify-seconds)))]
                    #:exists-ok? [exists-ok? #f])
  ;; First phase, returns a thunk to continue (i.e., download or not):
  (define (try-read-cache)
    (call-with-cache-db/catch-exn-until-success-finishes
     cache-dir
     log-error-string
     'shared
     ;; On success:
     (lambda (db cache-db-file)
       ;; Lock is still held here to make sure a cache file isn't
       ;; deleted and replaced with a different file while we're
       ;; trying to copy it.
       (cond
        [(hash-ref db source-key #f)
         => (lambda (fn)
              (notify-cache-use fn)
              (copy-file (build-path cache-dir fn) dest-file exists-ok?)
              ;; no work in continuation:
              void)]
        [else fetch-and-continue]))
     ;; On failure (exception is logged already):
     (lambda (exn) fetch-and-continue)))

  ;; Second phase (used when cache read fails):
  (define (fetch-and-continue)
    (fetch)
    (when (file-exists? dest-file)
      (try-write-cache)))

  ;; Third phase, when fetching seems to have worked:
  (define (try-write-cache)
    (call-with-cache-db/catch-exn-until-success-finishes
     cache-dir
     log-error-string
     'exclusive
     ;; On success getting the current db:
     (lambda (db cache-db-file)
       ;; Lock is still held here to make sure no one else
       ;; tries to clean up while we copy a file into
       ;; the cache.
       
       ;; We assume that the cost of this filesystem traversal
       ;; is small compared to download costs:
       (define revised-db (limit-sizes cache-dir
                                       (clean-database db cache-dir
                                                       log-debug-string)
                                       (sub1 max-cache-files)
                                       (- max-cache-size (file-size dest-file))
                                       evict-before?
                                       log-debug-string))
       
       ;; Copy file into cache and update cache db:
       (log-debug-string (format "caching for ~s" source-key))
       (define cache-file (make-temporary-file "cached~a" #f cache-dir))
       (copy-file dest-file cache-file #t)
       (write-db cache-db-file
                 (hash-set revised-db
                           source-key
                           (path->string
                            (file-name-from-path cache-file)))))
     ;; On failure, we can just give up writing to the cache;
     ;; the exception is already logged:
     void))

  ;; Start with first phase, and continue with its result:
  ((try-read-cache)))

(define (cache-remove source-key ; #f => clear all
                      cache-dir
                      #:log-error-string [log-error-string log-error-string]
                      #:log-debug-string [log-debug-string log-debug-string])
  (call-with-cache-db/catch-exn-until-success-finishes
   cache-dir
   log-error-string
   'exclusive
   ;; success:
   (lambda (db cache-db-file)
     (write-db cache-db-file
               (if source-key
                   (hash-remove db source-key)
                   (hash))))
   ;; failure:
   (lambda (exn) (raise exn))))

;; Extract cache-db, holding file lock (in `lock-mode`) while calling
;; `success-handler` on the db. If the cache-db read fails, use an
;; empty db. If any failure happens other than reading the db,
;; including duing `success-handler`, then call `failure-handler` with
;; the exception after logging it. Note that breaks are disabled when
;; calling the failure handler, so it should return quickly.
(define call-with-cache-db/catch-exn-until-success-finishes
  (lambda (cache-dir log-error-string lock-mode 
                     success-handler
                     failure-handler)
    (define cache-db-file (build-path cache-dir "cache.rktd"))
    (define (succeed db)
      (success-handler db cache-db-file))
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (log-error-string (format "cache attempt failed: ~a"
                                                 (exn-message exn)))
                       (failure-handler exn))])
      (make-directory* cache-dir)
      (call-with-file-lock/timeout
       cache-db-file
       lock-mode
       (lambda ()
         (succeed (read-db cache-db-file log-error-string)))
       (lambda ()
         ;; raise exception to be caught above:
         (error (format "could not acquire ~s lock" lock-mode)))))))

;; Called with read lock, and handles failure by returning
;; an empty db:
(define (read-db cache-db-file log-error-string)
  (if (not (file-exists? cache-db-file)) ; avoid logging error if the file does not exist
      (hash)
      (with-handlers ([exn:fail?
                       (lambda (exn)
                         (log-error-string (format "cache file read failure: ~a"
                                                   (exn-message exn)))
                         (hash))])
        (define db (call-with-input-file*
                    cache-db-file
                    (lambda (i)
                      (call-with-default-reading-parameterization
                       (lambda ()
                         (read i))))))
        (cond
         [(and (hash? db)
               ;; be picky about hash content:
               (for/and ([v (in-hash-values db)])
                 (and (string? v)
                      (regexp-match? #rx"[a-zA-Z0-9]*" v)
                      ((string-length v) . <= . 64))))
          db]
         [else
          (log-error-string "cache file read failure: ill-formed content")
          (hash)]))))

;; Called with write lock:
(define (write-db cache-db-file db)
  (call-with-output-file*
   cache-db-file
   #:exists 'truncate/replace
   (lambda (o)
     (write db o)
     (newline o))))

;; Called with write lock; delete files not in
;; db, and removes non-existent files from db:
(define (clean-database db cache-dir log-debug-string)
  (define rev (for/hash ([(k v) (in-hash db)])
                (values v k)))
  ;; Delete files not in db:
  (define exist
    (for/fold ([exist (hash)]) ([f (directory-list cache-dir)])
      (define s (path->string f))
      (cond
       [(hash-ref rev s #f) 
        (hash-set exist s #t)]
       [(not (regexp-match? #rx"^cached" s))
        ;; not a cache file, so leave it alone:
        exist]
       [else 
        ;; delete unrecorded cache file:
        (define p (build-path cache-dir f))
        (log-debug-string (format "removing file from cache: ~a" p))
        (delete-file p)
        exist])))
  ;; Filter db by existing files:
  (for/hash ([(k v) (in-hash db)]
             #:when (or (hash-ref exist v #f)
                        (begin
                          (log-debug-string (format "lost cache file for entry: ~s" k))
                          #f)))

    (values k v)))

;; Called with the write lock, deletes oldest files as needed
;; to meet count and size constraints, returns updated db:
(define (limit-sizes cache-dir db 
                     max-count max-size evict-before?
                     log-debug-string)
  (define files
    (for/list ([(k v) (in-hash db)])
      (define path (build-path cache-dir v))
      (hash 'name v
            'key k
            'size (file-size path)
            'modify-seconds (file-or-directory-modify-seconds path))))
  (define count (hash-count db))
  (define size (for/sum ([f (in-list files)])
                 (hash-ref f 'size)))
  (cond
   [(and (count . < . max-count)
         (size . < . max-size))
    ;; no need to remove anything
    db]
   [else
    ;; Sort, so we can remove the oldest (or whatever `evict-before?`
    ;; says should be evicted first):
    (define sorted (sort files evict-before?))
    (let loop ([sorted sorted]
               [db db]
               [count count]
               [size size])
      (cond
       [(or (null? sorted)
            (and (count . < . max-count)
                 (size . < . max-size)))
        db]
       [else
        ;; Delete first cached file in the eviction list:
        (define key (hash-ref (car sorted) 'key))
        (log-debug-string (format "deleting cached file for ~s" key))
        (delete-file (build-path cache-dir (hash-ref (car sorted) 'name)))
        (loop (cdr sorted)
              (hash-remove db key)
              (sub1 count)
              (- size (hash-ref (car sorted) 'size)))]))]))

;; Default logging functions
(define (log-error-string s)
  (log-error s))
(define (log-debug-string s)
  (log-debug s))
