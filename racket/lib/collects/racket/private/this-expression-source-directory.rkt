#lang racket/base
(require (for-syntax racket/base 
                     setup/collects
                     setup/main-collects)
         setup/collects
         setup/main-collects)

(provide this-expression-source-directory
         this-expression-source-file)

(define-for-syntax (this-expression-source stx dir?)
  (syntax-case stx ()
    [(_ sub)
     (let ([stx (syntax sub)])
       (let ([source-path
              (let* ([source (syntax-source stx)]
                     [source (and (path? source) source)]
                     [local (or (current-load-relative-directory) (current-directory))]
                     [x-dir (or (and source 
                                     (file-exists? source)
                                     (if dir?
                                         (let-values ([(base file dir?)
                                                       (split-path source)])
                                           (and (path? base)
                                                (path->complete-path base local)))
                                         source))
                                (if dir?
                                    local
                                    (build-path local
                                                (if source
                                                    (let-values ([(base file dir?)
                                                                  (split-path source)])
                                                      file)
                                                    "unknown"))))]
                     [dir (if dir?
                              (path->main-collects-relative x-dir)
                              (path->collects-relative x-dir))])
                (if (and (pair? dir) (eq? 'collects (car dir)))
                    (with-syntax ([d dir]
                                  [relative->path (if dir?
                                                      #'main-collects-relative->path
                                                      #'collects-relative->path)])
                      (syntax/loc stx (relative->path 'd)))
                    (with-syntax ([d (if (bytes? dir) dir (path->bytes dir))])
                      (syntax/loc stx (bytes->path d)))))])
         (let ([mpi (syntax-source-module stx)])
           (if mpi
               (with-syntax ([extract (if dir?
                                          #'extract-module-directory
                                          #'extract-module-file)])
                 (quasisyntax/loc stx
                   (or (extract
                        (quote-syntax #,(datum->syntax stx 'context stx stx)))
                       #,source-path)))
               source-path))))]
    [(_) (this-expression-source #`(x #,stx) dir?)]))

(define-syntax (this-expression-source-directory stx)
  (this-expression-source stx #t))

(define-syntax (this-expression-source-file stx)
  (this-expression-source stx #f))

(define (extract-module stx dir?)
  (let ([srcmod (let ([mpi (syntax-source-module stx)])
                  (if (module-path-index? mpi)
                    (module-path-index-resolve mpi)
                    mpi))])
    (let* ([name (resolved-module-path-name srcmod)]
           [name (if (pair? name) (car name) name)])
      (and (path? name)
           (if dir?
               (let-values ([(base name dir?) (split-path name)])
                 (and (path? base)
                      base))
               name)))))

(define (extract-module-directory stx)
  (extract-module stx #t))
(define (extract-module-file stx)
  (extract-module stx #f))
