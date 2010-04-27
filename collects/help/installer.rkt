#lang scheme/base

;; Builds different kinds of executables for different platforms.

;; proposed changes below -robby.

(provide post-installer)
(require launcher)

(define (post-installer path)
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
                 (build-path (collection-path "help") "help")))
        (values available-mzscheme-variants
                make-mzscheme-launcher
                mzscheme-program-launcher-path
                '())))
    (for ([variant (variants)])
      (parameterize ([current-launcher-variant variant])
        (mk-launcher '("-l-" "help/help")
                     (mk-path "plt-help")       ;; change to "Racket Docs"
                     `([exe-name . "plt-help"]  ;; get rid of this (in favor of 'raco docs')
                       [relative? . #t]
                       [framework-root . #f]
                       [dll-dir . #f]
                       ,@extras))))))
