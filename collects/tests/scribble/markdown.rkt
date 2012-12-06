#lang racket/base

;; Use text renderer to check some Scribble functionality

(require scribble/base-render (prefix-in markdown: scribble/markdown-render)
         racket/file racket/class racket/runtime-path tests/eli-tester)

(define-runtime-path source-dir "markdown-docs")
(define work-dir (build-path (find-system-path 'temp-dir)
                             "scribble-docs-tests"))

(define (build-markdown-doc src-file dest-file)
  (let* ([renderer (new (markdown:render-mixin render%) [dest-dir work-dir])]
         [docs     (list (dynamic-require src-file 'doc))]
         [fns      (list (build-path work-dir dest-file))]
         [fp       (send renderer traverse docs fns)]
         [info     (send renderer collect  docs fns fp)]
         [r-info   (send renderer resolve  docs fns info)])
    (send renderer render docs fns r-info)
    (send renderer get-undefined r-info)))

(provide markdown-tests)
(module+ main (markdown-tests))
(define (markdown-tests)
  (when (or (file-exists? work-dir) (directory-exists? work-dir))
    (delete-directory/files work-dir))
  (dynamic-wind
    (λ() (make-directory work-dir))
    (λ()
      (define files (map path-element->string (directory-list source-dir)))
      (test do
        (for ([scrbl (in-list files)]
              #:when (regexp-match? #rx"\\.scrbl$" scrbl)
              [md    (in-value (regexp-replace #rx"\\.scrbl$" scrbl ".md"))]
              #:when (member md files))
          ;; (printf "Testing ~s -> ~s\n" scrbl md)
          (define src-file (build-path source-dir scrbl))
          (define expect-file (build-path source-dir md))
          (define generated-file (build-path work-dir "gen.md"))
          (define (contents file)
            (regexp-replace #rx"\n+$" (file->string file) ""))
          (define undefineds (build-markdown-doc src-file "gen.md"))
          (for ([u (in-list undefineds)])
            (when (eq? 'tech (car u))
              (test #:failure-message
                    (format "undefined tech: ~e" u)
                    #f)))
          (test #:failure-message
                (format
                 "mismatch for: \"~a\", expected text in: \"~a\", got:\n~a"
                 scrbl md (contents generated-file))
                (string=? (contents expect-file) (contents generated-file))))))
    (λ() (delete-directory/files work-dir))))
