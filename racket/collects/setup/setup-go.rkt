(module setup-go racket/base
  (require "setup-cmdline.rkt"
           "option.rkt"
           "setup-core.rkt"
           compiler/cm)

  (define-values (short-name x-flags x-specific-collections x-specific-planet-packages x-archives)
    (parse-cmdline (current-command-line-arguments)))

  (parameterize
   ;; Converting parse-cmdline results into parameter settings:
   ([current-target-plt-directory-getter
     (if (assq 'all-users x-flags)
         (lambda (preferred main-collects-parent-dir choices) 
           main-collects-parent-dir)
         (current-target-plt-directory-getter))]
    [trust-existing-zos (or (assq 'trust-existing-zos x-flags)
                            (trust-existing-zos))]
    [specific-collections x-specific-collections]
    [archives x-archives]
    [specific-planet-dirs x-specific-planet-packages]
    
    [setup-program-name short-name])
   
   (setup-core)))
