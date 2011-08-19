#lang racket/base
(require racket/unit
         
         ;; All the rest are to get the imports for setup@:
         "option-sig.rkt"
         "setup-unit.rkt"
         "option-unit.rkt"
         launcher/launcher-sig
         launcher/launcher-unit
         dynext/dynext-sig
         dynext/dynext-unit
         compiler/sig
         compiler/option-unit
         compiler/compiler-unit)

(provide run-single-installer install-planet-package clean-planet-package reindex-user-documentation)

;; run-single-installer : string (-> string) -> void
;; runs the installer on the given package
(define (run-single-installer file get-target-dir)
  (run-single-installer/internal file get-target-dir #f #f #f))

;; install-planet-package : path path (list string string (listof string) nat nat) -> void
;; unpacks and installs the given planet package into the given path
(define (install-planet-package file directory spec)
  (run-single-installer/internal file (lambda () directory) (cons directory spec) #f #f))

;; clean-planet-package : path (list string string (listof string) nat nat) -> void
;; cleans the given planet package
(define (clean-planet-package directory spec)
  (run-single-installer/internal #f (lambda () directory) (cons directory spec) #f #t))

;; reindex-user-documentation
;; call after installing or uninstalling a set of Planet packages
(define (reindex-user-documentation)
  (run-single-installer/internal #f current-directory #f '(("scribblings/main/user")) #f))

;; run-single-installer : string (-> string) (list path string string nat nat) -> void
;; creates a separate thread, runs the installer in that thread,
;; returns when the thread completes
(define (run-single-installer/internal file get-target-dir planet-spec collections clean?)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust]
                 [current-namespace (make-base-namespace)]
                 [exit-handler (lambda (v) (custodian-shutdown-all cust))])
    (define thd
      (thread
       (lambda ()
         (define-unit set-options@
           (import setup-option^ compiler^)
           (export)
           ;; >>>>>>>>>>>>>> <<<<<<<<<<<<<<<
           ;; Here's where we tell setup the archive file:
           (unless (or clean? (not file))
             (archives (list file))
             (when planet-spec
               (archive-implies-reindex #f)))
           
           ;; Here's where we make get a directory:
           (current-target-directory-getter
            get-target-dir)
           
           (when planet-spec
             (specific-planet-dirs (list planet-spec)))
           
           (when collections
             (specific-collections collections))
           
           (when clean?
             (clean #t)
             (make-zo #f)
             (make-launchers #f)
             (make-info-domain #t)
             (call-install #f)
             (make-docs #f))
           
           (setup-program-name "raco setup")
           
           (parallel-workers 1))
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
                 setup@))))))
    (dynamic-wind
     void
     (lambda ()
       (with-handlers ([exn:break? (lambda (exn)
                                     (break-thread thd)
                                     (sleep 0.1)
                                     (raise exn))])
         (thread-wait thd)))
     (lambda () (custodian-shutdown-all cust)))))
