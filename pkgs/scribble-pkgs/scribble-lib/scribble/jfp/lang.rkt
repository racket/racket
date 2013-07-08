#lang scheme/base
(require scribble/doclang
         (except-in scribble/base author)
         scribble/jfp
         setup/collects
         "../private/defaults.rkt"
         net/ftp
         racket/file
         scribble/latex-prefix
         (for-syntax scheme/base))
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/jfp)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin]))

;; No options, currently, but keep in case we want to support some:
(define-syntax (module-begin stx)
  (syntax-case* stx () (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
    [(_ id ws . body)
     ;; Skip intraline whitespace to find options:
     (and (string? (syntax-e #'ws))
          (regexp-match? #rx"^ *$" (syntax-e #'ws)))
     #'(module-begin id . body)]
    [(_ id . body)
     #'(#%module-begin id (post-process) () . body)]))

(define cls-file
  (let ([p (scribble-file "jfp/jfp1.cls")])
    (if (file-exists? (collects-relative->path p))
        p
        (downloaded-file "jfp1.cls"))))

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8
                 (format "\\documentclass{jfp1}\n~a\\usepackage{times}\n\\usepackage{qcourier}\n"
                         unicode-encoding-packages))
                (scribble-file "jfp/style.tex")
                (list cls-file)
                #f))

(unless (or (not (path? cls-file))
            (file-exists? cls-file))
  (log-error (format "File not found: ~a" cls-file))
  (define site "ftp.cambridge.org")
  (define path "pub/texarchive/journals/latex/jfp-cls")
  (define file "jfp1.cls")
  (log-error (format "Downloading via ftp://~a/~a/~a..." site path file))
  (define c (ftp-establish-connection site 21 "anonymous" "user@racket-lang.org"))
  (ftp-cd c path)
  (let-values ([(base name dir?) (split-path cls-file)])
    (make-directory* base)
    (ftp-download-file c base file))
  (ftp-close-connection c))
