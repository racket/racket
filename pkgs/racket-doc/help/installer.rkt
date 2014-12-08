#lang scheme/base

;; Builds different kinds of executables for different platforms.
;; The `plt-help' executable is for backward compatibity.
;; The `Racket Documentation' executable is to help Windows and
;;  Mac users who are completely lost and need something to click.

(provide post-installer)
(require launcher)

(define (post-installer path collection user?)
  (for ([mr? (case (system-type)
               [(macosx)  '(#t #f)]
               [(windows) '(#t)]
               [else      '(#f)])])
    (define-values (variants mk-launcher mk-path extras)
      (if mr?
        (values available-mred-variants
                make-mred-launcher
                mred-program-launcher-path
                (build-aux-from-path
                 (path-replace-suffix (collection-file-path "help.ico" "help") #"")))
        (values available-mzscheme-variants
                make-mzscheme-launcher
                mzscheme-program-launcher-path
                '())))
    (for ([variant (remove* '(script-3m script-cgc) (variants))])
      (parameterize ([current-launcher-variant variant])
        (mk-launcher '("-l-" "help/help")
                     (mk-path (if mr? "Racket Documentation" "plt-help") #:user? user?)
                     `([exe-name . ,(if mr? "Racket Documentation" "plt-help")]
                       [relative? . ,(not user?)]
                       [install-mode . ,(if user? 'user 'main)]
                       [start-menu? . #t]
                       ,@extras))))))
