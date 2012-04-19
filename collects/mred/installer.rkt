#lang racket/base

(require launcher compiler/embed)
(provide post-installer)

;; Platforms that get a `MrEd' executable:
(define mred-exe-systems '(unix))

(define (post-installer path)
  (define variants (available-mred-variants))
  (when (memq (system-type) mred-exe-systems)
    (for ([v variants] #:when (memq v '(3m cgc)))
      (parameterize ([current-launcher-variant v])
        (create-embedding-executable
         (mred-program-launcher-path "MrEd")
         #:cmdline '("-I" "scheme/gui/init")
         #:variant v
         #:launcher? #t
         #:gracket? #t
         #:aux '((framework-root . #f)
                 (dll-dir . #f)
                 (relative? . #t))))))
  ;; add a mred-text executable that uses the -z flag (preferring a script)
  (for ([vs '((script-3m 3m) (script-cgc cgc))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (make-gracket-launcher
           '("-z")
           (mred-program-launcher-path "gracket-text")
           '([relative? . #t] [subsystem . console] [single-instance? . #f]
             ;; the following two are required to avoid using a full path,
             ;; should be removed when `relative?' will imply this
             [framework-root . #f] [dll-dir . #f]))
          (make-gracket-launcher
           '("-I" "scheme/gui/init" "-z")
           (mred-program-launcher-path "mred-text")
           '([relative? . #t] [subsystem . console] [single-instance? . #f]
             ;; the following two are required to avoid using a full path,
             ;; should be removed when `relative?' will imply this
             [framework-root . #f] [dll-dir . #f]))))))
  ;; add bin/gracket and bin/mred script under OS X
  (when (eq? 'macosx (system-type))
    (for ([v variants] #:when (memq v '(script-3m script-cgc)))
      (parameterize ([current-launcher-variant v])
        (make-gracket-launcher 
         '()
         (mred-program-launcher-path "GRacket")
         '([exe-name . "GRacket"] [relative? . #t]
           [framework-root . #f] [dll-dir . #f]))
        (make-gracket-launcher 
         '()
         (mred-program-launcher-path "MrEd")
         '([exe-name . "GRacket"] [relative? . #t]
           [framework-root . #f] [dll-dir . #f]))))))
