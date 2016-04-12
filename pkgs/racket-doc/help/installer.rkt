#lang scheme/base
(require launcher
         setup/dirs)

;; Builds different kinds of executables for different platforms.
;; The `plt-help' executable is for backward compatibity.
;; The `Racket Documentation' executable is to help Windows and
;;  Mac users who are completely lost and need something to click.

(provide installer)

(define (installer path coll user? no-main?)
  (unless no-main?
    (do-installer path coll user? #f)
    (when (and (not user?)
               (find-config-tethered-console-bin-dir))
      (do-installer path coll #f #t)))
  (when (find-addon-tethered-console-bin-dir)
    (do-installer path coll #t #t)))

(define (do-installer path collection user? tethered?)
  (for ([mr? (case (system-type)
               [(macosx)  '(#t #f)]
               [(windows) '(#t)]
               [else      '(#f)])]
        #:when (or (not tethered?)
                   (if mr?
                       (if user?
                           (find-addon-tethered-gui-bin-dir)
                           (find-config-tethered-gui-bin-dir))
                       (if user?
                           (find-addon-tethered-console-bin-dir)
                           (find-config-tethered-console-bin-dir)))))
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
        (mk-launcher #:tether-mode (and tethered? (if user? 'addon 'config))
                     (append
                      '("-l-" "help/help"))
                     (mk-path (if mr? "Racket Documentation" "plt-help") #:user? user? #:tethered? tethered?)
                     `([exe-name . ,(if mr? "Racket Documentation" "plt-help")]
                       [relative? . ,(not user?)]
                       [install-mode . ,(if user? 'user 'main)]
                       [start-menu? . ,(not user?)]
                       ,@extras))))))
