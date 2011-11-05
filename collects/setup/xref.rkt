#lang scheme/base

(require scribble/xref
         scheme/fasl
         scheme/path
         setup/dirs
         "getinfo.rkt"
         "private/path-utils.rkt")

(provide load-collections-xref)

(define cached-xref #f)

(define (get-dests)
  (define main-dirs
    (parameterize ([current-library-collection-paths
                    (let ([d (find-collects-dir)]) 
                      (if d (list d) null))])
      (for/hash ([k (in-list (find-relevant-directories '(scribblings) 'no-planet))])
        (values k #t))))
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
                                          #"")))])
      (and (not (and (len . >= . 3) (memq 'omit (caddr d))))
           (let* ([d (doc-path dir name flags (hash-ref main-dirs dir #f) 'false-if-missing)]
                  [p (and d (build-path d "out.sxref"))])
             (and p (file-exists? p) p))))))

(define (get-reader-thunks)
  (map (lambda (dest)
         (lambda ()
           (with-handlers ([exn:fail? (lambda (exn)
                                        (fprintf (current-error-port)
                                                 "WARNING: ~a\n"
                                                 (if (exn? exn)
                                                   (exn-message exn)
                                                   (format "~e" exn)))
                                        #f)])
             (cadr (call-with-input-file* dest fasl->s-exp)))))
       (filter values (get-dests))))

(define (load-collections-xref [report-loading void])
  (or cached-xref
      (begin (report-loading)
             (set! cached-xref (load-xref (get-reader-thunks)))
             cached-xref)))
