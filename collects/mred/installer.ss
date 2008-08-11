#lang scheme/base

(require launcher)
(provide post-installer)

(define (post-installer path)
  (define variants (available-mred-variants))
  ;; add a mred-text executable that uses the -z flag (preferring a script)
  (for ([vs '((script-3m 3m) (script-cgc cgc))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (make-mred-launcher
           '("-z")
           (mred-program-launcher-path "mred-text")
           '([relative? . #t] [subsystem . console] [single-instance? . #f]
             ;; the following two are required to avoid using a full path,
             ;; should be removed when `relative?' will imply this
             [framework-root . #f] [dll-dir . #f]))))))
  ;; add a bin/mred script under OS X
  (when (eq? 'macosx (system-type))
    (for ([v variants] #:when (memq v '(script-3m script-cgc)))
      (parameterize ([current-launcher-variant v])
        (make-mred-launcher null
                            (mred-program-launcher-path "MrEd")
                            '([exe-name . "MrEd"] [relative? . #t]
                              [framework-root . #f] [dll-dir . #f]))))))
