#lang scheme/base

(require scribble/xref
         scheme/fasl
         scheme/path
         racket/promise
         setup/dirs
         "getinfo.rkt"
         "private/path-utils.rkt"
         "doc-db.rkt")

(provide load-collections-xref
         make-collections-xref)

(define cached-xref #f)

(define (get-dests no-user?)
  (define main-dirs
    (parameterize ([current-library-collection-paths
                    (let ([d (find-collects-dir)]) 
                      (if d (list d) null))])
      (for/hash ([k (in-list (find-relevant-directories '(scribblings) 'no-planet))])
        (values k #t))))
  (apply
   append
   (for*/list ([dir (find-relevant-directories '(scribblings) 'all-available)]
               [d (let ([info-proc (get-info/full dir)])
                    (if info-proc
                        (info-proc 'scribblings)
                        '()))])
     (unless (and (list? d) (pair? d))
       (error 'xref "bad scribblings entry: ~e" d))
     (let* ([len   (length d)]
            [flags (if (len . >= . 2) (cadr d) '())]
            [name  (if (len . >= . 4)
                       (cadddr d)
                       (path->string
                        (path-replace-suffix (file-name-from-path (car d))
                                             #"")))]
            [out-count (if (len . >= . 5)
                           (list-ref d 4)
                           1)])
       (if (not (and (len . >= . 3) (memq 'omit (caddr d))))
           (let ([d (doc-path dir name flags (hash-ref main-dirs dir #f) 
                              (if no-user? 'never 'false-if-missing))])
             (if d
                 (for*/list ([i (in-range (add1 out-count))]
                             [p (in-value (build-path d (format "out~a.sxref" i)))]
                             #:when (file-exists? p))
                   p)
                 null))
           null)))))

(define ((dest->source done-ht) dest)
  (if (hash-ref done-ht dest #f)
      (lambda () #f)
      (lambda ()
        (hash-set! done-ht dest #t)
        (with-handlers ([exn:fail? (lambda (exn)
                                     (log-warning
                                      "warning: ~a"
                                      (if (exn? exn)
                                          (exn-message exn)
                                          (format "~e" exn)))
                                     #f)])
          (cadr (call-with-input-file* dest fasl->s-exp))))))

(define (make-key->source db-path no-user?)
  (define main-db (cons (or db-path
                            (build-path (find-doc-dir) "docindex.sqlite"))
                        ;; cache for a connection:
                        (box #f)))
  (define user-db (and (not no-user?)
                       (cons (build-path (find-user-doc-dir) "docindex.sqlite")
                             ;; cache for a connection:
                             (box #f))))
  (define done-ht (make-hash)) ; tracks already-loaded documents
  (lambda (key)
    (cond
     [key
      (define (try p)
        (and p
             (let* ([maybe-db (unbox (cdr p))]
                    [db 
                     ;; Use a cached connection, or...
                     (or (and (box-cas! (cdr p) maybe-db #f)
                              maybe-db)
                         ;; ... create a new one
                         (doc-db-file->connection (car p)))])
               ((let/ec esc
                  ;; The db query:
                  (define result
                    (doc-db-key->path db key
                                      #:fail (lambda ()
                                               ;; Rollback within a connection can be slow,
                                               ;; so abandon the connection and try again:
                                               (doc-db-disconnect db)
                                               (esc (lambda () (try p))))))
                  ;; cache the connection, if none is already cached:
                  (or (box-cas! (cdr p) #f db)
                      (doc-db-disconnect db))
                  (lambda () result))))))
      (define dest (or (try main-db) (try user-db)))
      (and dest
           ((dest->source done-ht) dest))]
     [else
      ;; force all documents
      (define thunks (get-reader-thunks no-user? done-ht))
      (lambda () 
        ;; return a procedure so we can produce a list of results:
        (lambda () 
          (for/list ([thunk (in-list thunks)])
            (thunk))))])))

(define (get-reader-thunks no-user? done-ht)
  (map (dest->source done-ht)
       (filter values (get-dests no-user?))))

(define (load-collections-xref [report-loading void])
  (or cached-xref
      (begin (report-loading)
             (set! cached-xref 
                   (make-collections-xref))
             cached-xref)))

(define (make-collections-xref #:no-user? [no-user? #f]
                               #:doc-db [db-path #f])
  (if (doc-db-available?)
      (load-xref null
                 #:demand-source (make-key->source db-path no-user?))
      (load-xref (get-reader-thunks no-user? (make-hash)))))
