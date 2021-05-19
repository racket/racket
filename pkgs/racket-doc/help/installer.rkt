#lang scheme/base
(require launcher
         setup/dirs
         setup/variant
         racket/file
         racket/path)

;; Builds different kinds of executables for different platforms.
;; The `plt-help' executable is for backward compatibility.
;; The `Racket Documentation' executable is to help Windows and
;;  Mac users who are completely lost and need something to click.

(provide installer)

(define (installer path coll user? no-main?)
  (cond
    [user?
     (if (find-addon-tethered-console-bin-dir)
         (do-installer path coll #t #t)
         (do-installer path coll #t #f))]
    [else
     (unless no-main?
       (if (find-config-tethered-console-bin-dir)
           (do-installer path coll #f #t)
           (do-installer path coll #f #f)))]))

(define (do-installer path collection user? tethered?)
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
    (for ([variant (filter (lambda (x) (not (script-variant? x))) (variants))])
      (parameterize ([current-launcher-variant variant])
        (define exe-path
          (mk-path (if mr? "Racket Documentation" "plt-help") #:user? user? #:tethered? tethered?))
        (unless (exists-in-another-layer? exe-path user? tethered? #:gui? mr?)
          (mk-launcher #:tether-mode (and tethered? (if user? 'addon 'config))
                       (append
                        '("-l-" "help/help"))
                       (prep-dir exe-path)
                       `([exe-name . ,(if mr? "Racket Documentation" "plt-help")]
                         [relative? . ,(not user?)]
                         [install-mode . ,(if user? 'user 'main)]
                         [start-menu? . ,(not user?)]
                         ,@extras)))))))

(define (exists-in-another-layer? exe-name user? tethered? #:gui? gui?)
  ;; for an untethered main installation, check whether the
  ;; executable exists already in an earlier layer
  (and (not user?)
       (not tethered?)
       (let-values ([(base name dir?) (split-path exe-name)])
         (for/or ([dir (in-list (if gui?
                                    (get-gui-bin-extra-search-dirs)
                                    (get-console-bin-extra-search-dirs)))])
           (file-or-directory-type (build-path dir name) #f)))))

(define (prep-dir p)
  (define dir (path-only p))
  (make-directory* dir)
  p)
