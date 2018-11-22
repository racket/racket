#lang racket/base
(require racket/file
         racket/port
         file/sha1
         "../compile/read-linklet.rkt")

(provide make-cache
         get-cached-compiled
         cache-compiled!
         register-dependency!
         
         current-cache-layer
         make-cache-layer

         cache->used-paths)

(struct cache (dir 
               [table #:mutable] ; filename -> entry [used for a cache file]
               used              ; to track dependencies
               in-memory))       ; key -> code       [when no cache filed is in use]

(struct entry (key               ; sha1 of filename
               content           ; sha1 of file content
               dependencies)     ; list of key
        #:prefab)

(define current-cache-layer (make-parameter #f))

;; A cache later collects immediate dependencies
;; for a module as it is compiled
(define (make-cache-layer) (box null))

(define (cache-dir->file cache-dir)
  (build-path cache-dir "cache.rktd"))

(define (make-cache cache-dir out-of-date-callback)
  (define cache-file (and cache-dir
                          (cache-dir->file cache-dir)))
  (define table
    (if (and cache-file
             (file-exists? cache-file))
        (only-up-to-date (call-with-input-file* cache-file read)
                         cache-dir
                         out-of-date-callback)
        #hash()))
  (cache cache-dir table (make-hash) (make-hash)))

(define (only-up-to-date table cache-dir out-of-date-callback)
  ;; Build a new table imperatively (as a kind of memoization)
  (define new-table (make-hash))
  (define reported (make-hash))
  (define (up-to-date? path e)
    (or (hash-ref new-table path #f)
        (and (file-exists? path)
             (file-exists? (build-path cache-dir (entry-key e)))
             (equal? (call-with-input-file* path sha1-with-version)
                     (entry-content e))
             (for/and ([path (in-list (entry-dependencies e))])
               (define e (hash-ref table path #f))
               (and e (up-to-date? path e)))
             (begin
               (hash-set! new-table path e)
               #t))
        (begin
          (unless (hash-ref reported path #f)
            (hash-set! reported path #t)
            (out-of-date-callback path))
          #f)))
  ;; Check all file content and dependencies:
  (for ([(k e) (in-hash table)])
    (up-to-date? k e))
  ;; Convert back to immutable:
  (for/hash ([(k e) (in-hash new-table)])
    (values k e)))

(define (get-cached-compiled cache path [notify-success void])
  (hash-set! (cache-used cache) path #t)
  (define e (hash-ref (cache-table cache)
                      (path->string path)
                      #f))
  (define cached-file (and e
                           (cache-dir cache)
                           (build-path (cache-dir cache)
                                       (entry-key e))))
  (cond
   [(and cached-file
         (file-exists? cached-file))
    (notify-success)
    (parameterize ([read-accept-compiled #t])
      (call-with-input-file*
       cached-file
       (lambda (i)
         (read-bytes 2 i) ; consume "#~"
         (read-linklet-bundle-or-directory i))))]
   [(and e
         (hash-ref (cache-in-memory cache) (entry-key e) #f))
    => (lambda (c)
         (notify-success)
         c)]
   [else #f]))

(define (register-dependency! cache path)
  (define l (current-cache-layer))
  (when l
    (define deps (unbox l))
    (define s (path->string path))
    (unless (member s deps)
      (set-box! l (cons s deps)))))

(define (cache-compiled! cache path c layer)
  (define key (sha1 (open-input-bytes (path->bytes path))))
  (define file-content (call-with-input-file* path sha1-with-version))
  (define new-table (hash-set (cache-table cache) (path->string path)
                              (entry key
                                     file-content
                                     (unbox layer))))
  (set-cache-table! cache new-table)
  (cond
   [(cache-dir cache)
    (define cache-file (cache-dir->file (cache-dir cache)))
    (make-directory* (cache-dir cache))
    (call-with-output-file*
     #:exists 'truncate
     (build-path (cache-dir cache) key)
     (lambda (o) (write c o)))
    (call-with-atomic-output-file
     cache-file
     (lambda (o path) (writeln new-table o)))]
   [else
    (hash-set! (cache-in-memory cache) key c)]))

(define (cache->used-paths cache)
  (hash-keys (cache-used cache)))

(define (sha1-with-version i)
  (sha1 (input-port-append
         #f
         (open-input-string (version))
         i)))
