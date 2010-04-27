#lang scheme/base
(require scribble/doclang
         scribble/core
         (except-in scribble/base author)
         scribble/decode
         scribble/jfp
         "../private/defaults.ss"
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

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8
                 (format "\\documentclass{jfp}\n\\usepackage{times}\n\\usepackage{qcourier}\n"))
                (scribble-file "jfp/style.tex")
                (list (scribble-file "jfp/jfp.cls"))
                #f))
