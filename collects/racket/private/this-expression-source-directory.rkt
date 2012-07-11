#lang racket/base
(require (for-syntax racket/base 
                     setup/main-collects)
         setup/main-collects)
(provide this-expression-source-directory)
(define-syntax (this-expression-source-directory stx)
  (syntax-case stx ()
    [(_ sub)
     (let ([stx (syntax sub)])
       (let ([source-path
              (let* ([source (syntax-source stx)]
                     [source (and (path? source) source)]
                     [local (or (current-load-relative-directory) (current-directory))]
                     [dir (path->main-collects-relative
                           (or (and source (file-exists? source)
                                    (let-values ([(base file dir?)
                                                  (split-path source)])
                                      (and (path? base)
                                           (path->complete-path base local))))
                               local))])
                (if (and (pair? dir) (eq? 'collects (car dir)))
                    (with-syntax ([d dir])
                      (syntax/loc stx (main-collects-relative->path 'd)))
                    (with-syntax ([d (if (bytes? dir) dir (path->bytes dir))])
                      (syntax/loc stx (bytes->path d)))))])
         (let ([mpi (syntax-source-module stx)])
           (if mpi
               (quasisyntax/loc stx
                 (or (extract-module-directory (quote-syntax #,(datum->syntax
                                                                stx
                                                                'context
                                                                stx
                                                                stx)))
                     #,source-path))
               source-path))))]
    [(_) #`(this-expression-source-directory #,stx)]))

(define (extract-module-directory stx)
  (let ([srcmod (let ([mpi (syntax-source-module stx)])
                  (if (module-path-index? mpi)
                    (module-path-index-resolve mpi)
                    mpi))])
    (let* ([name (resolved-module-path-name srcmod)]
           [name (if (pair? name) (car name) name)])
      (and (path? name)
           (let-values ([(base name dir?) (split-path name)])
             (and (path? base)
                  base))))))

