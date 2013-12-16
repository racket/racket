#lang racket/base
(require "option.rkt"
         "setup-core.rkt"
         launcher/launcher
         compiler/compiler)

(provide setup)

(define (setup #:file [file #f]
               #:get-target-dir [get-target-dir #f]
               #:planet-specs [planet-specs #f]
               #:collections [collections #f]
               #:make-docs? [make-docs? #t]
               #:make-doc-index? [make-doc-index? #f]
               #:make-user? [make-user? #t]
               #:clean? [clean? #f]
               #:tidy? [tidy? #f]
               #:avoid-main? [avoid-main? #f]
               #:jobs [parallel #f])
  (parameterize 
   (;; Here's where we tell setup the archive file:
    [archives (if (or clean? (not file)) (archives) (list file))]
    [archive-implies-reindex (if (and planet-specs (and (not clean?) file))
                                 #f
                                 (archive-implies-reindex))]
    
    ;; Here's where we make get a directory:
    [current-target-directory-getter get-target-dir]
    
    [specific-planet-dirs (if planet-specs planet-specs (specific-planet-dirs))]
    
    [specific-collections (if collections collections (specific-collections))]
    
    [make-only (if (or planet-specs collections) #t (make-only))]
    
    [make-user (if make-user? (make-user) #f)]
    
    [make-docs (if (and make-docs? (not clean?)) (make-docs) #f)]
    
    [make-doc-index (if make-doc-index? #t (make-doc-index))]

    [make-tidy (if tidy? #t (make-tidy))]

    [avoid-main-installation (if avoid-main? #t (avoid-main-installation))]
    
    [clean (if clean? #t (clean))]
    [make-zo (if clean? #f (make-zo))]
    [make-launchers (if clean? #f (make-launchers))] 
    [make-info-domain (if clean? #t (make-info-domain))]
    [call-install (if clean? #f (call-install))]
    
    [setup-program-name "raco setup"]
    
    [parallel-workers (if parallel parallel (parallel-workers))])

   (let/ec esc
     (parameterize ([exit-handler
                     (lambda (v) (esc (if (and (integer? v)
                                               (<= 1 v 255))
                                          #f
                                          #t)))])
       (setup-core)
       #t))))
