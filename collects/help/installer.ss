;; Builds different kinds of executables for different platforms.
#lang scheme/base

(provide post-installer)
(require launcher/launcher)

(define post-installer
  (lambda (path)
    (case (system-type)
      [(macosx)  (make-mred-exe) (make-mzscheme-exe)]
      [(windows) (make-mred-exe)]
      [else      (make-mzscheme-exe)])))

(define (make-mred-exe)
  (for ([variant (available-mred-variants)])
    (parameterize ([current-launcher-variant variant])
      (make-mred-launcher
       '("-l" "help/help")
       (mred-program-launcher-path "plt-help")
       (append '((exe-name . "plt-help") (relative? . #t))
               (build-aux-from-path
                (build-path (collection-path "help") "help")))))))

(define (make-mzscheme-exe)
  (for ([variant (available-mzscheme-variants)])
    (parameterize ([current-launcher-variant variant])
      (make-mzscheme-launcher '("-l" "help/help")
                              (mzscheme-program-launcher-path "plt-help")
                              '((exe-name . "plt-help") (relative? . #t))))))
