#lang scheme/base

(require scribble/xref
         scheme/fasl
         scheme/path
         racket/promise
         setup/dirs
         "getinfo.rkt"
         "private/path-utils.rkt"
         "doc-db.rkt")

(provide load-collections-xref)

(define cached-xref #f)

(define (get-dests)
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
           (let ([d (doc-path dir name flags (hash-ref main-dirs dir #f) 'false-if-missing)])
             (if d
                 (for*/list ([i (in-range (add1 out-count))]
                             [p (in-value (build-path d (format "out~a.sxref" i)))]
                             #:when (file-exists? p))
                   p)
                 null))
           null)))))

(define (dest->source dest)
  (lambda ()
    (with-handlers ([exn:fail? (lambda (exn)
                                 (log-error
                                  "warning: ~a"
                                  (if (exn? exn)
                                      (exn-message exn)
                                      (format "~e" exn)))
                                 #f)])
      (cadr (call-with-input-file* dest fasl->s-exp)))))

(define (dir->connection dir)
  (define p (build-path dir "docindex.sqlite"))
  (and (file-exists? p)
       (doc-db-file->connection p)))

(define main-db (delay (dir->connection (find-doc-dir))))
(define user-db (delay (dir->connection (find-user-doc-dir))))

(define (key->source key)
  (define (try p)
    (and p
         (doc-db-key->path p key)))
  (define dest (or (try (force main-db))
                   (try (force user-db))))
  (and dest
       (dest->source dest)))

(define (get-reader-thunks)
  (map dest->source
       (filter values (get-dests))))

(define (load-collections-xref [report-loading void])
  (or cached-xref
      (begin (report-loading)
             (set! cached-xref 
                   (if (doc-db-available?)
                       (load-xref null 
                                  #:demand-source key->source)
                       (load-xref (get-reader-thunks))))
             cached-xref)))
