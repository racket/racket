#lang scheme/base
(require scribble/doclang
         scribble/core
         scribble/base
         scribble/decode
         scribble/sigplan
         "../private/defaults.ss"
         (for-syntax scheme/base))
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/sigplan)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case* stx (preprint) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
    [(_ id ws . body)
     ;; Skip intraline whitespace to find options:
     (and (string? (syntax-e #'ws))
          (regexp-match? #rx"^ *$" (syntax-e #'ws)))
     #'(module-begin id . body)]
    [(_ id preprint . body)
     #'(#%module-begin id (post-process #t) () . body)]
    [(_ id . body)
     #'(#%module-begin id (post-process #f) () . body)]))

(define ((post-process preprint?) doc)
  (add-sigplan-styles 
   (add-defaults doc
                 (string->bytes/utf-8
                  (format "\\documentclass~a{sigplanconf}\n\\usepackage{times}\n\\usepackage{qcourier}\n"
                          (if preprint? "[preprint]" "")))
                 (scribble-file "sigplan/style.tex")
                 (list (scribble-file "sigplan/sigplanconf.cls"))
                 #f)))

(define (add-sigplan-styles doc)
  ;; Ensure that "sigplan.tex" is used, since "style.tex"
  ;; re-defines commands.
  (struct-copy part doc [to-collect
                         (cons (terms)
                               (part-to-collect doc))]))
