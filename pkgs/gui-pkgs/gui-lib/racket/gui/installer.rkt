#lang racket/base
(require launcher
         racket/path
         racket/file)

(provide post-installer)

(define (post-installer path collection user?)
  (define variants (available-mred-variants))
  ;; add a gracket-text executable that uses the -z flag (preferring a script)
  (for ([vs '((script-3m 3m) (script-cgc cgc))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (make-mred-launcher
           '("-z")
           (prep-dir
            (mred-program-launcher-path "gracket-text" #:user? user?))
           `([subsystem . console] [single-instance? . #f]
             [relative? . ,(not user?)]))))))
  ;; add a bin/gracket (in addition to lib/gracket)
  (for ([vs '((script-3m 3m) (script-cgc cgc))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (make-mred-launcher null
                              (prep-dir
                               (mred-program-launcher-path "GRacket" #:user? user?))
                              '([exe-name . "GRacket"] [relative? . ,(not user?)]
                                [exe-is-gracket . #t])))))))

(define (prep-dir p)
  (define dir (path-only p))
  (make-directory* dir)
  p)
