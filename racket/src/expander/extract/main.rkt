#lang racket/base
(require "link.rkt"
         "linklet-info.rkt"
         "linklet.rkt"
         "get-linklet.rkt"
         "needed.rkt"
         "export.rkt"
         "check-and-report.rkt"
         "flatten.rkt"
         "gc-defn.rkt"
         "simplify-defn.rkt"
         "prune-name.rkt"
         "decompile.rkt"
         "save-and-report.rkt"
         "global.rkt"
         "underscore.rkt"
         "symbol.rkt"
         "../run/status.rkt"
         "../common/set.rkt"
         racket/pretty)

(provide extract)

;; Gather all of the linklets need to run phase 0 of the specified
;; module while keeping the module's variables that are provided from
;; phase 0. In other words, keep enogh to produce any value or effect
;; that `dynamic-require` would produce.
(define (extract start-mod-path cache
                 #:print-extracted-to print-extracted-to
                 #:as-c? as-c?
                 #:as-decompiled? as-decompiled?
                 #:as-bytecode? as-bytecode?
                 #:local-rename? local-rename?
                 #:no-global? no-global?
                 #:global-ok global-ok
                 ;; Table of symbol -> (listof knot-spec),
                 ;; to redirect a remaining import back to
                 ;; an implementation that is defined in the
                 ;; flattened code; a knot-spec as a module-path
                 ;; redirect to there, or as 'ignored avoids both
                 ;; a knot and complaining
                 #:instance-knot-ties instance-knot-ties
                 ;; Table of symbol -> string
                 ;; to replace (hash-ref (or (primitive-table '<table>) ...) '<id> #f)
                 ;; with a direct reference to <prefix><id>
                 #:primitive-table-directs primitive-table-directs
                 ;; Override linklet compiler's simple inference
                 ;; of side-effects to remove a module from the
                 ;; flattened form if it's not otherwise referenced:
                 #:side-effect-free-modules side-effect-free-modules
                 ;; A list of symbols that should not be defined in the
                 ;; flattened, GCed form:
                 #:disallows disallows)
  ;; Located modules:
  (define compiled-modules (make-hash))

  ;; All linklets that find we based on module `requires` from the
  ;; starting module
  (define seen (make-hash)) ; link -> linklet-info

  ;; The subset of `seen` that have that non-empty linklets
  (define linklets (make-hash)) ; link -> linklet-info
  ;; The same linklets are referenced this list, but kept in reverse
  ;; order of instantiation:
  (define linklets-in-order (box null))

  ;; Which linklets (as represented by a "link") are actually needed to run
  ;; the code, which includes anything referenced by the starting
  ;; point's exports and any imported linklet that has a side effect:
  (define needed (make-hash)) ; link -> value for reason

  ;; Use the host Racket's module name resolver to normalize the
  ;; starting module path:
  (define start-name
    (resolved-module-path-name
     (module-path-index-resolve
      (module-path-index-join start-mod-path #f))))

  ;; We always start at phase 0
  (define start-link (link start-name 0))
  
  ;; Start with the given link, and follow dependencies
  (get-linklets! start-link
                 #:cache cache
                 #:compiled-modules compiled-modules
                 #:seen seen
                 #:linklets linklets
                 #:linklets-in-order linklets-in-order
                 #:side-effect-free-modules side-effect-free-modules)
  
  ;; Compute which linklets are actually used as imports
  (needed! start-link 'start
           #:seen seen
           #:needed needed)
  
  ;; We also want the starting name's re-exports:
  (for ([ex-lnk (in-list (linklet-info-re-exports (hash-ref seen start-link)))])
    (needed! ex-lnk `(re-export ,start-link)
             #:seen seen
             #:needed needed))

  ;; Anything that shows up in `codes` with a side effect also counts
  (for ([(lnk li) (in-hash linklets)])
    (when (linklet-info-side-effects? li)
      (needed! lnk 'side-effect
               #:seen seen
               #:needed needed)))
  
  ;; Check for bootstrap obstacles, and report what we've found
  (define check-later-vars
    (check-and-record-report! #:compiled-modules compiled-modules
                              #:linklets linklets
                              #:linklets-in-order linklets-in-order
                              #:needed needed
                              #:instance-knot-ties instance-knot-ties))
  
  ;; If we're in source mode, we can generate a single linklet
  ;; that combines all the ones we found
  (when (linklets-are-source-mode? linklets)
    ;; Get variables to be exported by a flattened linklet; all of the
    ;; module provides must refer to instance variables
    (define exports
      (get-module-export-variables start-link
                                   #:compiled-modules compiled-modules
                                   #:cache cache))

    ;; Generate the flattened linklet
    (define-values (variable-names flattened-linklet-expr)
      (flatten! start-link
                #:linklets linklets
                #:linklets-in-order linklets-in-order
                #:needed needed
                #:exports exports
                #:instance-knot-ties instance-knot-ties
                #:primitive-table-directs primitive-table-directs
                #:check-later-vars check-later-vars))
    
    (define simplified-expr
      (simplify-definitions flattened-linklet-expr))

    ;; Remove unreferenced definitions
    (define gced-linklet-expr
      (garbage-collect-definitions simplified-expr
                                   #:disallows disallows))

    (log-status "Checking that references outside the runtime were removed by simplification...")
    (define really-used-names (all-used-symbols gced-linklet-expr))
    (define complained? #f)
    (for ([(var complains) (in-hash check-later-vars)])
      (when (set-member? really-used-names (hash-ref variable-names var))
        (for ([complain (in-list (reverse complains))])
          (complain (lambda (var)
                      (set-member? really-used-names (hash-ref variable-names var))))
          (set! complained? #t))))
    (when complained?
      (exit 1))

    (when no-global?
      (check-global gced-linklet-expr global-ok))

    ;; Avoid gratuitous differences due to names generated during
    ;; expansion...
    (define re-renamed-linklet-expr
      (if local-rename?
          ;; ... and allow the same name to be used in different non-shadowing
          ;; local contextx
          (collapse-underscore-numbers gced-linklet-expr)
          ;; ... but use a distinct symbol for every binder's name
          (simplify-underscore-numbers gced-linklet-expr)))

    ;; Prune any explicit function names (using a `quote` pattern in
    ;; the body) when they still match a name that would be inferred
    (define pruned-linklet-expr
      (prune-names re-renamed-linklet-expr))

    (cond
     [(or as-decompiled? as-bytecode?)
      (compile-and-decompile pruned-linklet-expr print-extracted-to #:as-bytecode? as-bytecode?)]
     [else
      (save-and-report-flattened! pruned-linklet-expr print-extracted-to
                                  #:as-c? as-c?)])))
