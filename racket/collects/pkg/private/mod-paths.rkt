#lang racket/base
(require racket/set
         racket/format
         syntax/modcollapse
         "metadata.rkt"
         "get-info.rkt")

(provide pkg-directory->module-paths
         directory->module-paths)

(define (pkg-directory->module-paths dir pkg-name 
                                     #:namespace [metadata-ns (make-metadata-namespace)])
  (set->list (directory->module-paths dir pkg-name metadata-ns)))

(define (directory->module-paths dir pkg-name metadata-ns)
  (define dummy (build-path dir "dummy.rkt"))
  (define compiled (string->path-element "compiled"))
  (define single-collect (pkg-single-collection dir #:name pkg-name #:namespace metadata-ns))
  (define (try-path s f)
    (define mp
      `(lib ,(apply ~a
                    #:separator "/"
                    (let ([l (map path-element->string 
                                  (explode-path f))])
                      (if single-collect
                          (if (eq? 'relative (car l))
                              (cons single-collect (cdr l))
                              (cons single-collect l))
                          l)))))
    (if (module-path? mp)
        (set-add s (collapse-module-path mp dummy))
        s))
  (parameterize ([current-directory dir])
    (let loop ([s (set)] [f 'init] [check-zo? #f])
      (cond
       [(eq? f 'init)
        (for/fold ([s s]) ([f (directory-list)])
          (loop s f check-zo?))]
       [(directory-exists? f)
        ;; Count ".zo" files toward the set of module paths only
        ;; if an "info.rkt" in an enclosing directory says to
        ;; assume virtual sources. Otherwise, the ".zo" file will
        ;; be discarded by `raco setup'.
        (define sub-check-zo?
          (or check-zo?
              (let ([i (get-pkg-info f metadata-ns)])
                (and i
                     (i 'assume-virtual-sources (lambda () #f))))))
        (for/fold ([s s]) ([f (directory-list f #:build? #t)])
          (loop s f sub-check-zo?))]
       [(not (file-exists? f)) s]
       [else
        (define-values (base name dir?) (split-path f))
        (cond
         [(and (eq? 'relative base) (not single-collect)) s]
         [else
          (define bstr (path-element->bytes name))
          (cond
           [(or (equal? #"info.rkt" bstr)
                (equal? #"info.ss" bstr))
            ;; don't count "info.rkt" as a conflict, because
            ;; splices may need their own "info.rkt"s, and
            ;; `raco setup' can handle that
            s]
           [(regexp-match? #rx#"[.](?:rkt|ss|scrbl)$" bstr)
            (try-path s f)]
           [(and check-zo?
                 (regexp-match? #rx#"_(?:rkt|ss|scrbl)[.]zo$" (path-element->bytes name)))
            (define-values (dir-base dir-name dir?) (split-path base))
            (cond
             [(eq? 'relative dir-base) s]
             [(equal? dir-name compiled)
              (define bstr2 (regexp-replace
                             #rx#"_(?:rkt|ss|scrbl)[.]zo$"
                             (path-element->bytes name)
                             #".rkt"))
              (if (equal? #"info.rkt" bstr2)
                  s
                  (try-path s (build-path dir-base
                                          (bytes->path-element
                                           bstr2))))]
             [else s])]
           [else s])])]))))

