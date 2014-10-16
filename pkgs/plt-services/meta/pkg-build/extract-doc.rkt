#lang racket/base
(require racket/file
         racket/format
         setup/getinfo
         setup/collection-name
         file/unzip
         pkg/strip)

(provide extract-documentation)

(define (extract-documentation zip pkg dest-dir)
  (define temp-dir (make-temporary-file "docs~a" 'directory))
  (parameterize ([current-directory temp-dir])
    (unzip zip))
  (for ([d (in-directory temp-dir)])
    (cond
     [(directory-exists? d)
      (define i (get-info/full d
                               #:namespace (make-base-namespace)
                               #:bootstrap? #t))
      (when i
        (define l (i 'scribblings (lambda () null)))
        (when (list? l)
          (for ([s (in-list l)])
            (when (and (list? s)
                       (pair? s)
                       (path-string? (car s))
                       (or ((length s) . < . 4)
                           (collection-name-element? (list-ref s 3))))
              (define n (if ((length s) . < . 4)
                            (let-values ([(base name dir?) (split-path (car s))])
                              (path->string (path-replace-suffix name #"")))
                            (list-ref s 3)))
              (when (directory-exists? (build-path d "doc" n))
                (define doc-dest (build-path dest-dir (~a n "@" pkg)))
                (copy-directory/files (build-path d "doc" n)
                                      doc-dest)
                (for ([p (in-directory doc-dest)])
                  (when (regexp-match? #rx#"[.]html$" (path->bytes p))
                    (fixup-local-redirect-reference p "../local-redirect"))))))))])))
