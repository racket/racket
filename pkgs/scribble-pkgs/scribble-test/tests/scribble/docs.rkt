#lang racket/base

;; Use text renderer to check some Scribble functionality

(require scribble/base-render
         (prefix-in text: scribble/text-render)
         (prefix-in html: scribble/html-render)
         (prefix-in latex: scribble/latex-render)
         (prefix-in markdown: scribble/markdown-render)
         racket/file 
         racket/class
         racket/runtime-path
         tests/eli-tester)

(define-runtime-path source-dir "docs")
(define work-dir (build-path (find-system-path 'temp-dir)
                             "scribble-docs-tests"))

(define (build-doc render% src-file dest-file)
  (let* ([renderer (new render% [dest-dir work-dir])]
         [docs     (list (dynamic-require src-file 'doc))]
         [fns      (list (build-path work-dir dest-file))]
         [fp       (send renderer traverse docs fns)]
         [info     (send renderer collect  docs fns fp)]
         [r-info   (send renderer resolve  docs fns info)])
    (send renderer render docs fns r-info)
    (send renderer get-undefined r-info)))

(define (build-text-doc src-file dest-file)
  (build-doc (text:render-mixin render%) src-file dest-file))

(define (build-html-doc src-file dest-file)
  (build-doc (html:render-mixin render%) src-file dest-file))

(define (build-htmls-doc src-file dest-file)
  (build-doc (html:render-multi-mixin (html:render-mixin render%)) src-file dest-file))

(define (build-latex-doc src-file dest-file)
  (build-doc (latex:render-mixin render%) src-file dest-file))

(define (build-markdown-doc src-file dest-file)
  (build-doc (markdown:render-mixin render%) src-file dest-file))

(provide docs-tests)
(module+ main (docs-tests))
(define (docs-tests)
  (when (or (file-exists? work-dir) (directory-exists? work-dir))
    (delete-directory/files work-dir))
  (dynamic-wind
    (λ() (make-directory work-dir))
    (λ()
      (define files (map path-element->string (directory-list source-dir)))
      (test do
        (for ([scrbl (in-list files)]
              #:when (regexp-match? #rx"\\.scrbl$" scrbl)
              [txt   (in-value (regexp-replace #rx"\\.scrbl$" scrbl ".txt"))]
              #:when (member txt files))
          ;; (printf "Testing ~s -> ~s\n" scrbl txt)
          (define src-file (build-path source-dir scrbl))
          (define expect-file (build-path source-dir txt))
          (define generated-file (build-path work-dir "gen.txt"))
          (define (contents file)
            (regexp-replace #rx"\n+$" (file->string file) ""))
          (define undefineds (build-text-doc src-file "gen.txt"))
          (for ([u (in-list undefineds)])
            (when (eq? 'tech (car u))
              (test #:failure-message
                    (format "undefined tech: ~e" u)
                    #f)))
          (test #:failure-message
                (format
                 "mismatch for: \"~a\", expected text in: \"~a\", got:\n~a"
                 scrbl txt (contents generated-file))
                (string=? (contents expect-file) (contents generated-file)))

          ;; Make sure the document at least renders in other modes:
          (void (build-html-doc src-file "gen.html"))
          (void (build-htmls-doc src-file "gen"))
          (void (build-latex-doc src-file "gen.tex"))
          (void (build-markdown-doc src-file "gen.md")))))
    (λ() (delete-directory/files work-dir))))
