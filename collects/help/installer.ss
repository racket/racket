;; Builds different kinds of executables for different platforms.
(module installer mzscheme
  (provide post-installer)
  (require (lib "launcher.ss" "launcher"))

  (define post-installer
    (lambda (path)
      (case (system-type)
        [(macosx) 
         (make-mred-exe)
         (make-mzscheme-exe)]
        [(windows)
         (make-mred-exe)]
        [else
         (make-mzscheme-exe)])))

  (define (make-mred-exe)
    (for-each
     (lambda (variant)
       (parameterize ([current-launcher-variant variant])
         (make-mred-launcher '("-l" "help/help")
                             (mred-program-launcher-path "plt-help")
                             (append
                              '((exe-name . "plt-help")
                                (relative? . #t))
                              (build-aux-from-path
                               (build-path (collection-path "help") "help"))))))
     (available-mred-variants)))

  (define (make-mzscheme-exe)
    (for-each
     (lambda (variant)
       (parameterize ([current-launcher-variant variant])
         (make-mzscheme-launcher '("-l" "help/help")
                                 (mzscheme-program-launcher-path "plt-help")
                                 '((exe-name . "plt-help")
                                   (relative? . #t)))))
     (available-mzscheme-variants))))
