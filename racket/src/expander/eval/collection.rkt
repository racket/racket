#lang racket/base
(require racket/private/check
         racket/private/config
         racket/private/place-local
         "parameter.rkt"
         ;; Avoid keyword-argument variant:
         (only-in '#%kernel directory-list))

(provide collection-path
         collection-file-path
         find-library-collection-paths
         find-library-collection-links

         find-col-file

         collection-place-init!)

(define (relative-path-string? s)
  (and (path-string? s) (relative-path? s)))

(define (check-collection who s l)
  (check who relative-path-string?
         #:contract "(and/c path-string? relative-path?)"
         s)
  (check who (lambda (l)
               (and (list? l)
                    (andmap relative-path-string? l)))
         #:contract "(listof (and/c path-string? relative-path?))"
         l))

(define (check-fail who fail)
  (check who (procedure-arity-includes/c 1) fail))

;; Non-keyword variant is wrapped by a kerword variant in `racket/base`
(define/who (collection-path fail collection collection-path) 
  (check-collection who collection collection-path)
  (check-fail who fail)
  (find-col-file fail
                 collection collection-path
                 #f
                 #f))

;; Non-keyword variant is wrapped by a kerword variant in `racket/base`
(define/who (collection-file-path fail check-compiled? file-name collection collection-path)
  (check who relative-path-string?
         #:contract "(and/c path-string? relative-path?)"
         file-name)
  (check-collection who collection collection-path)
  (check-fail who fail)
  (find-col-file fail
                 collection collection-path
                 file-name
                 check-compiled?))

(define (get-config-table d)
  (define p (and d (build-path d "config.rktd")))
  (or (and p
           (file-exists? p)
           (with-input-from-file p
             (lambda ()
               (let ([v (call-with-default-reading-parameterization read)])
                 (and (hash? v)
                      v)))))
      #hash()))

(define (get-installation-name config-table)
  (hash-ref config-table
            'installation-name 
            (version)))

(define (coerce-to-path p)
  (cond
    [(string? p) (collects-relative-path->complete-path (string->path p))]
    [(bytes? p) (collects-relative-path->complete-path (bytes->path p))]
    [(path? p) (collects-relative-path->complete-path p)]
    [else p]))

(define (collects-relative-path->complete-path p)
  (cond
    [(complete-path? p) p]
    [else
     (path->complete-path p (or (find-main-collects)
                                ;; If we get here, then something is configured wrong,
                                ;; and making up paths relative to the current directory
                                ;; is not great --- but we have to come up with some
                                ;; path at this point.
                                (current-directory)))]))

(define (add-config-search ht key orig-l)
  (define l (hash-ref ht key #f))
  (if l
      (let loop ([l l])
        (cond
          [(null? l) null]
          [(not (car l)) (append orig-l (loop (cdr l)))]
          [else (cons (coerce-to-path (car l)) (loop (cdr l)))]))
      orig-l))

(define (find-library-collection-links)
  (define ht (get-config-table (find-main-config)))
  (define lf (coerce-to-path
              (or (hash-ref ht 'links-file #f)
                  (build-path (or (hash-ref ht 'share-dir #f)
                                  (build-path 'up "share"))
                              "links.rktd"))))
  (append
   ;; `#f' means `current-library-collection-paths':
   (list #f)
   ;; user-specific
   (if (and (use-user-specific-search-paths)
            (use-collection-link-paths))
       (list (build-path (find-system-path 'addon-dir)
                         (get-installation-name ht)
                         "links.rktd"))
       null)
   ;; installation-wide:
   (if (use-collection-link-paths)
       (add-config-search
        ht
        'links-search-files
        (list lf))
       null)))

;; map from link-file names to cached information:
(define-place-local links-cache (make-weak-hash))

(define (collection-place-init!)
  (set! links-cache (make-weak-hash)))

;; used for low-level exception abort below:
(define stamp-prompt-tag (make-continuation-prompt-tag 'stamp))

(define (file->stamp path old-stamp)
  ;; Using just the file's modification date almost works as a stamp,
  ;; but 1-second granularity isn't fine enough. A stamp is therefore
  ;; the file content paired with a filesystem-change event (where
  ;; supported), and the event lets us recycle the old stamp almost
  ;; always.
  (cond
    [(and old-stamp
          (cdr old-stamp)
          (not (sync/timeout 0 (cdr old-stamp))))
     old-stamp]
    [else
     (call-with-continuation-prompt
      (lambda ()
        (call-with-exception-handler
         (lambda (exn)
           (abort-current-continuation 
            stamp-prompt-tag
            (if (exn:fail:filesystem? exn)
                (lambda () #f)
                (lambda () (raise exn)))))
         (lambda ()
           (define dir-evt
             (and (vector-ref (system-type 'fs-change) 2) ; 'low-latency ?
                  (let loop ([path path])
                    (let-values ([(base name dir?) (split-path path)])
                      (and (path? base)
                           (if (directory-exists? base)
                               (filesystem-change-evt base (lambda () #f))
                               (loop base)))))))
           (cond
             [(not (file-exists? path))
              (cons #f dir-evt)]
             [else
              (define evt (and (vector-ref (system-type 'fs-change) 2) ; 'low-latency ?
                               (filesystem-change-evt path (lambda () #f))))
              (when dir-evt (filesystem-change-evt-cancel dir-evt))
              (cons (file->bytes path)
                    evt)]))))
      stamp-prompt-tag)]))

(define (file->bytes path)
  (call-with-input-file*
   path
   (lambda (p)
     (let ([bstr (read-bytes 8192 p)])
       (if (and (bytes? bstr)
                ((bytes-length bstr) . >= . 8192))
           (apply
            bytes-append
            (cons
             bstr
             (let loop ()
               (let ([bstr (read-bytes 8192 p)])
                 (if (eof-object? bstr)
                     null
                     (cons bstr (loop)))))))
           bstr)))))

(define (no-file-stamp? a)
  (or (not a)
      (not (car a))))

(define (get-linked-collections links-path)
  ;; Use/save information in `links-cache', relying on filesystem-change events
  ;; or a copy of the file to detect when the cache is stale.
  (let/ec esc
    (define (make-handler ts)
      (lambda (exn)
        (if (exn:fail? exn)
            (let ([l (current-logger)])
              (when (log-level? l 'error)
                (log-message l 'error
                             (format
                              "error reading collection links file ~s: ~a"
                              links-path
                              (exn-message exn))
                             (current-continuation-marks))))
            (void))
        (when ts
          (hash-set! links-cache links-path (cons ts #hasheq())))
        (if (exn:fail? exn)
            (esc (make-hasheq))
            ;; re-raise the exception (which is probably a break)
            exn)))
    (call-with-exception-handler
     (make-handler #f)
     (lambda ()
       (define links-stamp+cache (hash-ref links-cache links-path '(#f . #hasheq())))
       (define a-links-stamp (car links-stamp+cache))
       (define ts (file->stamp links-path a-links-stamp))
       (cond
         [(equal? ts a-links-stamp)
          (cdr links-stamp+cache)]
         [else
          (call-with-exception-handler
           (make-handler ts)
           (lambda ()
             (call-with-default-reading-parameterization
              (lambda ()
                (define v (if (no-file-stamp? ts)
                              null
                              (call-with-input-file*
                               links-path
                               (lambda (p)
                                 (begin0
                                   (read p)
                                   (unless (eof-object? (read p))
                                     (error "expected a single S-expression")))))))
                (unless (and (list? v)
                             (andmap (lambda (p)
                                       (and (list? p)
                                            (or (= 2 (length p))
                                                (= 3 (length p)))
                                            (or (string? (car p))
                                                (eq? 'root (car p))
                                                (eq? 'static-root (car p)))
                                            (path-string? (cadr p))
                                            (or (null? (cddr p))
                                                (regexp? (caddr p)))))
                                     v))
                  (error "ill-formed content"))
                (define ht (make-hasheq))
                (define dir (let-values ([(base name dir?) (split-path links-path)])
                              base))
                (for-each (lambda (p)
                            (when (or (null? (cddr p))
                                      (regexp-match? (caddr p) (version)))
                              (let ([dir (simplify-path
                                          (path->complete-path (cadr p) dir))])
                                (cond
                                  [(eq? (car p) 'static-root)
                                   ;; multi-collection, constant content:
                                   (for-each
                                    (lambda (sub)
                                      (when (directory-exists? (build-path dir sub))
                                        (let ([k (string->symbol (path->string sub))])
                                          (hash-set! ht k (cons dir (hash-ref ht k null))))))
                                    (directory-list dir))]
                                  [(eq? (car p) 'root)
                                   ;; multi-collection, dynamic content:
                                   ;; Add directory to the #f mapping, and also
                                   ;; add to every existing table element (to keep
                                   ;; the choices in order)
                                   (unless (hash-ref ht #f #f)
                                     (hash-set! ht #f null))
                                   (hash-for-each
                                    ht
                                    (lambda (k v)
                                      (hash-set! ht k (cons dir v))))]
                                  [else
                                   ;; single collection:
                                   (let ([s (string->symbol (car p))])
                                     (hash-set! ht s (cons (box dir)
                                                           (hash-ref ht s null))))]))))
                          v)
                ;; reverse all lists:
                (hash-for-each
                 ht
                 (lambda (k v) (hash-set! ht k (reverse v))))
                ;; save table & file content:
                (hash-set! links-cache links-path (cons ts ht))
                ht))))])))))

(define (normalize-collection-reference collection collection-path)
  ;; make sure that `collection' is a top-level collection name
  (cond
    [(string? collection)
     (let ([m (regexp-match-positions #rx"/+" collection)])
       (if m
           (cond
             [(= (caar m) (sub1 (string-length collection)))
              (values (substring collection 0 (caar m)) collection-path)]
             [else
              (values (substring collection 0 (caar m))
                      (cons (substring collection (cdar m))
                            collection-path))])
           (values collection collection-path)))]
    [else
     (define-values (base name dir?) (split-path collection))
     (if (eq? base 'relative)
         (values name collection-path)
         (normalize-collection-reference base (cons name collection-path)))]))

(define (find-col-file fail collection-in collection-path-in file-name check-compiled?)
  (define-values (collection collection-path)
    (normalize-collection-reference collection-in collection-path-in))
  (define all-paths (let ([sym (string->symbol 
                                (if (path? collection)
                                    (path->string collection)
                                    collection))])
                      (let loop ([l (current-library-collection-links)])
                        (cond
                          [(null? l) null]
                          [(not (car l))
                           ;; #f is the point where we try the old parameter:
                           (append 
                            (current-library-collection-paths)
                            (loop (cdr l)))]
                          [(hash? (car l))
                           ;; A hash table maps a collection-name symbol
                           ;; to a list of paths. We need to wrap each path
                           ;; in a box, because that's how the code below
                           ;; knows that it's a single collection's directory.
                           ;; A hash table can also map #f to a list of paths
                           ;; for directories that hold collections.
                           (append
                            (map box (hash-ref (car l) sym null))
                            (hash-ref (car l) #f null)
                            (loop (cdr l)))]
                          [else
                           (let ([ht (get-linked-collections (car l))])
                             (append 
                              ;; Table values are lists of paths and (box path)s,
                              ;; where a (box path) is a collection directory
                              ;; (instead of a directory containing collections).
                              (hash-ref ht sym null)
                              (hash-ref ht #f null)
                              (loop (cdr l))))]))))
  (define (done p)
    (if file-name (build-path p file-name) p))
  (define (*build-path-rep p c)
    (if (path? p)
        (build-path p c)
        ;; box => from links table for c
        (unbox p)))
  (define (*directory-exists? orig p)
    (if (path? orig)
        (directory-exists? p)
        ;; orig is box => from links table
        #t))
  (define (to-string p) (if (path? p) (path->string p) p))
  
  (let cloop ([paths all-paths] [found-col #f])
    (if (null? paths)
        (if found-col
            (done found-col)
            (let ([rest-coll
                   (if (null? collection-path)
                       ""
                       (apply
                        string-append
                        (let loop ([cp collection-path])
                          (if (null? (cdr cp))
                              (list (to-string (car cp)))
                              (list* (to-string (car cp)) "/" (loop (cdr cp)))))))])
              (define-values (filter)
                (lambda (f l)
                  (if (null? l)
                      null
                      (if (f (car l))
                          (cons (car l) (filter f (cdr l)))
                          (filter f (cdr l))))))
              (fail
               (format "collection not found\n  collection: ~s\n  in collection directories:~a~a" 
                       (if (null? collection-path)
                           (to-string collection)
                           (string-append (to-string collection) "/" rest-coll))
                       (apply
                        string-append
                        (map (lambda (p)
                               (format "\n ~a ~a" " " p))
                             (let ([len (length all-paths)]
                                   [clen (length (current-library-collection-paths))])
                               (if ((- len clen) . < . 5)
                                   all-paths
                                   (append (current-library-collection-paths)
                                           (list (format "... [~a additional linked and package directories]"
                                                         (- len clen))))))))
                       (if (ormap box? all-paths)
                           (format "\n   sub-collection: ~s\n  in parent directories:~a"
                                   rest-coll 
                                   (apply
                                    string-append
                                    (map (lambda (p)
                                           (format "\n   ~a" (unbox p)))
                                         (filter box? all-paths))))
                           "")))))
        (let ([dir (*build-path-rep (car paths) collection)])
          (if (*directory-exists? (car paths) dir)
              (let ([cpath (apply build-path dir collection-path)])
                (if (if (null? collection-path)
                        #t
                        (directory-exists? cpath))
                    (if file-name
                        (if (or (file-exists?/maybe-compiled cpath file-name
                                                             check-compiled?)
                                (let ([alt-file-name
                                       (let* ([file-name (if (path? file-name)
                                                             (path->string file-name)
                                                             file-name)]
                                              [len (string-length file-name)])
                                         (and (len . >= . 4)
                                              (string=? ".rkt" (substring file-name (- len 4)))
                                              (string-append (substring file-name 0 (- len 4)) ".ss")))])
                                  (and alt-file-name
                                       (file-exists?/maybe-compiled cpath alt-file-name
                                                                    check-compiled?))))
                            (done cpath)
                            ;; Look further for specific file, but remember
                            ;; first found directory
                            (cloop (cdr paths) (or found-col cpath)))
                        ;; Just looking for dir; found it:
                        (done cpath))
                    ;; sub-collection not here; try next instance
                    ;; of the top-level collection
                    (cloop (cdr paths) found-col)))
              (cloop (cdr paths) found-col))))))

(define (file-exists?/maybe-compiled dir path check-compiled?)
  (or (file-exists? (build-path dir path))
      (and check-compiled?
           (let ([try-path (path-add-extension path #".zo")]
                 [modes (use-compiled-file-paths)]
                 [roots (current-compiled-file-roots)])
             (ormap (lambda (d)
                      (ormap (lambda (mode)
                               (file-exists?
                                (let ([p (build-path dir mode try-path)])
                                  (cond
                                    [(eq? d 'same) p]
                                    [(relative-path? d) (build-path p d)]
                                    [else (reroot-path p d)]))))
                             modes))
                    roots)))))

(define (find-library-collection-paths [extra-collects-dirs null] [post-collects-dirs null])
  (let ([user-too? (use-user-specific-search-paths)]
        [cons-if (lambda (f r) (if f (cons f r) r))]
        [config-table (get-config-table (find-main-config))])
    (path-list-string->path-list
     (if user-too?
         (let ([c (environment-variables-ref (current-environment-variables)
                                             #"PLTCOLLECTS")])
           (if c
               (bytes->string/locale c #\?)
               ""))
         "")
     (add-config-search
      config-table
      'collects-search-dirs
      (cons-if
       (and user-too?
            (build-path (find-system-path 'addon-dir)
                        (get-installation-name config-table)
                        "collects"))
       (let loop ([l (append
                      extra-collects-dirs
                      (list (find-system-path 'collects-dir))
                      post-collects-dirs)])
         (if (null? l)
             null
             (let* ([collects-path (car l)]
                    [v (exe-relative-path->complete-path collects-path)])
               (if v
                   (cons (simplify-path (path->complete-path v (current-directory)))
                         (loop (cdr l)))
                   (loop (cdr l)))))))))))
