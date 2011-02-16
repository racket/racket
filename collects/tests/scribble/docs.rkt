#lang racket/base

;; Use text renderer to check some Scribble functionality

(require scribble/base-render (prefix-in text: scribble/text-render)
         racket/file racket/class racket/runtime-path tests/eli-tester)

(define-runtime-path source-dir "docs")
(define work-dir (build-path (find-system-path 'temp-dir)
                             "scribble-docs-tests"))

(define (build-text-doc src-file dest-file)
  (let* ([renderer (new (text:render-mixin render%) [dest-dir work-dir])]
         [docs     (list (dynamic-require src-file 'doc))]
         [fns      (list (build-path work-dir dest-file))]
         [fp       (send renderer traverse docs fns)]
         [info     (send renderer collect  docs fns fp)]
         [r-info   (send renderer resolve  docs fns info)])
    (send renderer render docs fns r-info)))

(define (check-text-build src-file expect-file)
  (build-text-doc src-file "gen.txt")
  (define (contents file) (regexp-replace #rx"\n+$" (file->string file) ""))
  (string=? (contents expect-file) (contents (build-path work-dir "gen.txt"))))

(provide docs-tests)
(define (docs-tests)
  (when (or (file-exists? work-dir) (directory-exists? work-dir))
    (delete-directory/files work-dir))
  (make-directory work-dir)
  (dynamic-wind void
    (lambda ()
      (define files (map path-element->string (directory-list source-dir)))
      (for ([scrbl (in-list files)]
            #:when (regexp-match? #rx"\\.scrbl$" scrbl)
            [txt   (in-value (regexp-replace #rx"\\.scrbl$" scrbl ".txt"))]
            #:when (member txt files))
        ;; (printf "Testing ~s -> ~s\n" scrbl txt)
        (test #:failure-message
              (format "mismatch from: \"~a\" expected: \"~a\"" scrbl txt)
              (check-text-build (build-path source-dir scrbl)
                                (build-path source-dir txt)))))
    (lambda () (delete-directory/files work-dir))))
