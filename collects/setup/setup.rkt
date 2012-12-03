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

(provide setup)

(define (setup #:file [file #f]
               #:get-target-dir [get-target-dir #f]
               #:planet-specs [planet-specs #f]
               #:collections [collections #f]
               #:make-docs? [make-docs? #t]
               #:make-user? [make-user? #t]
               #:clean? [clean? #f]
               #:jobs [parallel #f])
  (define-unit set-options@
    (import setup-option^ compiler^)
    (export)
    ;; >>>>>>>>>>>>>> <<<<<<<<<<<<<<<
    ;; Here's where we tell setup the archive file:
    (unless (or clean? (not file))
      (archives (list file))
      (when planet-specs
        (archive-implies-reindex #f)))
    
    ;; Here's where we make get a directory:
    (current-target-directory-getter
     get-target-dir)
    
    (when planet-specs
      (specific-planet-dirs planet-specs))
    
    (when collections
      (specific-collections collections))

    (unless make-user?
      (make-user #f))

    (unless make-docs?
      (make-docs #f))
    
    (when clean?
      (clean #t)
      (make-zo #f)
      (make-launchers #f)
      (make-info-domain #t)
      (call-install #f)
      (make-docs #f))
    
    (setup-program-name "raco setup")
    
    (when parallel
      (parallel-workers parallel)))
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
                 setup@))))
