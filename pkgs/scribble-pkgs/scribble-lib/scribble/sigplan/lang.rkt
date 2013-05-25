#lang scheme/base
(require scribble/doclang
         scribble/core
         scribble/base
         scribble/sigplan
         scribble/latex-prefix
         racket/list
         "../private/defaults.rkt"
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
           [onecolumn? #f]
           [nocopyright? #f]
           [times? #t]
           [qcourier? #t])
       (let loop ([stuff #'body])
         (syntax-case* stuff (onecolumn preprint 10pt nocopyright notimes noqcourier) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
           [(ws . body)
            ;; Skip intraline whitespace to find options:
            (and (string? (syntax-e #'ws))
                 (regexp-match? #rx"^ *$" (syntax-e #'ws)))
            (loop #'body)]
           [(preprint . body)
            (set! preprint? "preprint")
            (loop #'body)]
           [(onecolumn . body)
            (set! onecolumn? "onecolumn")
            (loop #'body)]
           [(nocopyright . body)
            (set! nocopyright? "nocopyrightspace")
            (loop #'body)]
           [(10pt . body)
            (set! 10pt? "10pt")
            (loop #'body)]
           [(noqcourier . body)
            (set! qcourier? #f)
            (loop #'body)]
           [(notimes . body)
            (set! times? #f)
            (loop #'body)]
           [body
            #`(#%module-begin id (post-process #,times? #,qcourier? #,preprint? #,10pt? #,nocopyright? #,onecolumn?) () . body)])))]))
#|

The docs for the times.sty package suggests that it should not be used
so maybe we want to disable it permanently (or replace it with something else).

Read here for more:

  http://www.ctan.org/tex-archive/macros/latex/required/psnfss/psnfss2e.pdf

|#

(define ((post-process times? qcourier? . opts) doc)
  (let ([options
         (if (ormap values opts)
             (format "[~a]" (apply string-append (add-between (filter values opts) ", ")))
             "")])
    (add-sigplan-styles 
     (add-defaults doc
                   (string->bytes/utf-8
                    (format "\\documentclass~a{sigplanconf}\n~a~a~a"
                            options
                            unicode-encoding-packages
                            (if times? 
                                "\\usepackage{times}\n"
                                "")
                            (if qcourier? 
                                "\\usepackage{qcourier}\n"
                                "")))
                   (scribble-file "sigplan/style.tex")
                   (list (scribble-file "sigplan/sigplanconf.cls"))
                   #f))))

(define (add-sigplan-styles doc)
  ;; Ensure that "sigplan.tex" is used, since "style.tex"
  ;; re-defines commands.
  (struct-copy part doc [to-collect
                         (cons (terms)
                               (part-to-collect doc))]))
