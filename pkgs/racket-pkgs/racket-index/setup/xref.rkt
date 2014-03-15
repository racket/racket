#lang racket/base

(require scribble/xref
         racket/fasl
         racket/path
         racket/promise
         setup/dirs
         setup/getinfo
         "private/doc-path.rkt"
         setup/doc-db)

(provide load-collections-xref
         make-collections-xref
         get-rendered-doc-directories)

(define cached-xref #f)

(define (get-rendered-doc-directories no-user? no-main?)
  (append (get-dests 'scribblings no-user? no-main? #f)
          (get-dests 'rendered-scribblings no-user? no-main? #f)))

(define (get-dests tag no-user? no-main? sxrefs?)
  (define main-dirs
    (for/hash ([k (in-list (find-relevant-directories (list tag) 'no-user))])
      (values k #t)))
  (apply
   append
   (for*/list ([dir (find-relevant-directories (list tag) 'all-available)]
               [d (let ([info-proc (get-info/full dir)])
                    (if info-proc
                        (info-proc tag)
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
                              (if no-user? 'never 'false-if-missing)
                              #:main? (not no-main?))])
             (if d
                 (if sxrefs?
                     (for*/list ([i (in-range (add1 out-count))]
                                 [p (in-value (build-path d (format "out~a.sxref" i)))]
                                 #:when (file-exists? p))
                       p)
                     (list d))
                 null))
           null)))))

(define ((dest->source done-ht quiet-fail?) dest)
  (if (hash-ref done-ht dest #f)
      (lambda () #f)
      (lambda ()
        (hash-set! done-ht dest #t)
        (with-handlers ([exn:fail? (lambda (exn)
                                     (unless quiet-fail?
                                       (log-warning
                                        "warning: ~a"
                                        (if (exn? exn)
                                            (exn-message exn)
                                            (format "~e" exn))))
                                     #f)])
          (make-data+root+doc-id
           ;; data to deserialize:
           (cadr (call-with-input-file* dest fasl->s-exp))
           ;; provide a root for deserialization:
           (path-only dest)
           ;; Use the destination directory's name as an identifier,
           ;; which allows a faster and more compact indirection
           ;; for installation-scaoped documentation:
           (let-values ([(base name dir?) (split-path dest)])
             (and (path? base)
                  (let-values ([(base name dir?) (split-path base)])
                    (and (path? name)
                         (path->string name))))))))))

(define (make-key->source db-path no-user? no-main? quiet-fail? register-shutdown!)
  (define main-db (and (not no-main?)
                       (cons (or db-path
                                 (build-path (find-doc-dir) "docindex.sqlite"))
                             ;; cache for a connection:
                             (box #f))))
  (define user-db (and (not no-user?)
                       (cons (build-path (find-user-doc-dir) "docindex.sqlite")
                             ;; cache for a connection:
                             (box #f))))
  (register-shutdown! (lambda ()
                        (define (close p)
                          (define c (unbox (cdr p)))
                          (when c
                            (if (box-cas! (cdr p) c #f)
                                (doc-db-disconnect c)
                                (close p))))
                        (when main-db (close main-db))
                        (when user-db (close user-db))))
  (define done-ht (make-hash)) ; tracks already-loaded documents
  (define forced-all? #f)
  (define (force-all)
    ;; force all documents
    (define thunks (get-reader-thunks no-user? no-main? quiet-fail? done-ht))
    (set! forced-all? #t)
    (lambda () 
      ;; return a procedure so we can produce a list of results:
      (lambda () 
        (for/list ([thunk (in-list thunks)])
          (thunk)))))
  (lambda (key)
    (cond
     [forced-all? #f]
     [key
      (define (try p)
        (and p
             (let* ([maybe-db (unbox (cdr p))]
                    [db 
                     ;; Use a cached connection, or...
                     (or (and (box-cas! (cdr p) maybe-db #f)
                              maybe-db)
                         ;; ... create a new one
                         (and (file-exists? (car p))
                              (doc-db-file->connection (car p))))])
               (and 
                db
                (let ()
                  ;; The db query:
                  (begin0
                   (doc-db-key->path db key)
                   ;; cache the connection, if none is already cached:
                   (or (box-cas! (cdr p) #f db)
                       (doc-db-disconnect db))))))))
      (define dest (or (try main-db) (try user-db)))
      (and dest
           (if (eq? dest #t)
               (force-all)
               ((dest->source done-ht quiet-fail?) dest)))]
     [else
      (unless forced-all?
        (force-all))])))

(define (get-reader-thunks no-user? no-main? quiet-fail? done-ht)
  (map (dest->source done-ht quiet-fail?)
       (filter values (append (get-dests 'scribblings no-user? no-main? #t)
                              (get-dests 'rendered-scribblings no-user? no-main? #t)))))

(define (load-collections-xref [report-loading void])
  (or cached-xref
      (begin (report-loading)
             (set! cached-xref 
                   (make-collections-xref))
             cached-xref)))

(define (make-collections-xref #:no-user? [no-user? #f]
                               #:no-main? [no-main? #f]
                               #:doc-db [db-path #f]
                               #:quiet-fail? [quiet-fail? #f]
                               #:register-shutdown! [register-shutdown! void])
  (if (doc-db-available?)
      (load-xref null
                 #:demand-source (make-key->source db-path no-user? no-main? quiet-fail?
                                                   register-shutdown!))
      (load-xref (get-reader-thunks no-user? no-main? quiet-fail? (make-hash)))))
