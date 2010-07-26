#lang scheme/base
(require scribble/doclang
         scribble/core
         scribble/base
         scribble/decode
         scribble/sigplan
         racket/list
         "../private/defaults.ss"
         (for-syntax scheme/base))
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/sigplan)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ id . body)
     (let ([preprint? #f]
           [10pt? #f]
           [nocopyright? #f])
       (let loop ([stuff #'body])
         (syntax-case* stuff (preprint 10pt nocopyright) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
           [(ws . body)
            ;; Skip intraline whitespace to find options:
            (and (string? (syntax-e #'ws))
                 (regexp-match? #rx"^ *$" (syntax-e #'ws)))
            (loop #'body)]
           [(preprint . body)
            (set! preprint? "preprint")
            (loop #'body)]
           [(nocopyright . body)
            (set! nocopyright? "nocopyrightspace")
            (loop #'body)]
           [(10pt . body)
            (set! 10pt? "10pt")
            (loop #'body)]
           [body
            #`(#%module-begin id (post-process #,preprint? #,10pt? #,nocopyright?) () . body)])))]))

(define ((post-process . opts) doc)
  (let ([options 
         (if (ormap values opts)
             (format "[~a]" (apply string-append (add-between (filter values opts) ", ")))
             "")])
    (add-sigplan-styles 
     (add-defaults doc
                   (string->bytes/utf-8
                    (format "\\documentclass~a{sigplanconf}\n\\usepackage{times}\n\\usepackage{qcourier}\n"
                            options))
                   (scribble-file "sigplan/style.tex")
                   (list (scribble-file "sigplan/sigplanconf.cls"))
                   #f))))

(define (add-sigplan-styles doc)
  ;; Ensure that "sigplan.tex" is used, since "style.tex"
  ;; re-defines commands.
  (struct-copy part doc [to-collect
                         (cons (terms)
                               (part-to-collect doc))]))
