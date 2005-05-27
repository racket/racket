(module installer mzscheme
  (require (lib "file.ss")
           (lib "etc.ss")
           (lib "launcher.ss" "launcher"))
  (provide installer)
  
  (define (installer plthome)
    (do-installation)
    (set! do-installation void))
  
  (define (do-installation)
    (for-each install-variation (available-mred-variants)))
  
  (define (install-variation variant)
    (parameterize ([current-launcher-variant variant])
      (make-mred-launcher
       (list "-ZmvqL" "drscheme.ss" "drscheme")
       (mred-program-launcher-path "DrScheme")
       (cons
        `(exe-name . "DrScheme")
        (build-aux-from-path (build-path (collection-path "drscheme") "drscheme")))))))