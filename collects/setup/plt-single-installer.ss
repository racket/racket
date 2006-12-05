(module plt-single-installer mzscheme 
  (require (lib "unit.ss")
           (lib "etc.ss")
           
	   ;; All the rest are to get the imports for setup@:
	   "option-sig.ss"
	   "setup-unit.ss"
	   "option-unit.ss"
	   (lib "launcher-sig.ss" "launcher")
	   (lib "launcher-unit.ss" "launcher")
	   (lib "dynext-sig.ss" "dynext")
	   (lib "dynext-unit.ss" "dynext")
	   (lib "sig.ss" "compiler")
	   (lib "option-unit.ss" "compiler")
	   (lib "compiler-unit.ss" "compiler"))

  (provide run-single-installer install-planet-package clean-planet-package)

  ;; run-single-installer : string (-> string) -> void
  ;; runs the instealler on the given package
  (define (run-single-installer file get-target-dir)
    (run-single-installer/internal file get-target-dir #f #f))
  
  ;; install-planet-package : path path (list string string (listof string) nat nat) -> void
  ;; unpacks and installs the given planet package into the given path
  (define (install-planet-package file directory spec)
    (run-single-installer/internal file (lambda () directory) (cons directory spec) #f))
  
  ;; clean-planet-package : path (list string string (listof string) nat nat) -> void
  ;; cleans the given planet package
  (define (clean-planet-package directory spec)
    (run-single-installer/internal #f (lambda () directory) (cons directory spec) #t))
  
  ;; run-single-installer : string (-> string) (list path string string nat nat) -> void
  ;; creates a separate thread, runs the installer in that thread,
  ;; returns when the thread completes
  (define (run-single-installer/internal file get-target-dir planet-spec clean?)
    (let ([cust (make-custodian)])
      (parameterize ([current-custodian cust]
                     [current-namespace (make-namespace)]
                     [exit-handler (lambda (v) (custodian-shutdown-all cust))])
        (let ([thd
               (thread
                (lambda ()
                  (define-unit set-options@
                    (import setup-option^ compiler^)
                    (export)
                    ;; >>>>>>>>>>>>>> <<<<<<<<<<<<<<<
                    ;; Here's where we tell setup the archive file!
                    (unless clean? 
                      (archives (list file)))
                    
                    ;; Here's where we make get a directory:
                    (current-target-directory-getter
                     get-target-dir)
                    
                    (when planet-spec
                      (specific-planet-dirs (list planet-spec)))
                    
                    (when clean?
                      (clean #t)
                      (make-zo #f)
                      (make-so #f)
                      (make-launchers #f)
                      (make-info-domain #t)
                      (call-install #f)))
                  (invoke-unit
                   (compound-unit/infer
                     (import)
                     (export)
                     (link launcher@
                           dynext:compile@
                           dynext:link@
                           dynext:file@
                           compiler:option@
                           compiler@
                           setup:option@
                           set-options@
                           setup@)))))])
          (thread-wait thd)
          (custodian-shutdown-all cust))))))
