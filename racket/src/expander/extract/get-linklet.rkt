#lang racket/base
(require "../common/set.rkt"
         "../common/phase.rkt"
         "../run/status.rkt"
         "../host/linklet.rkt"
         "../compile/module-use.rkt"
         "../syntax/binding.rkt"
         "../namespace/provided.rkt"
         "link.rkt"
         "linklet-info.rkt"
         "linklet.rkt"
         "module.rkt")

(provide get-linklets!)

(define (get-linklets! lnk
                       #:cache cache
                       #:compiled-modules compiled-modules
                       #:seen seen
                       #:linklets linklets
                       #:linklets-in-order linklets-in-order
                       #:side-effect-free-modules side-effect-free-modules)
  (let get-linklets! ([lnk lnk] [first? #t])
    (define name (link-name lnk))
    (define phase (link-phase lnk))
    (define root-name (if (pair? name) (car name) name)) ; strip away submodule path
    (unless (or (symbol? root-name) ; skip pre-defined modules
                (hash-ref seen lnk #f))
      ;; Seeing this module+phase combination for the first time
      (log-status "Getting ~s at ~s" name phase)
      (define comp-mod (get-compiled-module name root-name
                                            #:compiled-modules compiled-modules
                                            #:cache cache))

      ;; Extract the relevant linklet (i.e., at a given phase)
      ;; from the compiled module
      (define h (compiled-module-phase-to-linklet comp-mod))
      (define linklet
        (hash-ref h phase #f))

      ;; Extract other metadata at the module level:
      (define reqs (instance-variable-value (compiled-module-declaration comp-mod) 'requires))
      (define provs (instance-variable-value (compiled-module-declaration comp-mod) 'provides))

      ;; Extract phase-specific (i.e., linklet-specific) info on variables:
      (define vars (if linklet
                       (list->set (linklet-export-variables linklet))
                       null))
      ;; Extract phase-specific info on imports (for reporting bootstrap issues):
      (define in-vars (if linklet
                          (skip-abi-imports (linklet-import-variables linklet))
                          null))
      ;; Extract phase-specific info on side effects:
      (define side-effects? (and (not (hash-ref side-effect-free-modules name #f))
                                 (member phase (hash-ref h 'side-effects '()))
                                 #t))
      ;; Extract phase-specific mapping of the linklet arguments to modules
      (define uses
        (hash-ref (instance-variable-value (compiled-module-declaration comp-mod) 'phase-to-link-modules)
                  phase
                  null))
      
      (define dependencies
        (for*/list ([phase+reqs (in-list reqs)]
                    [req (in-list (cdr phase+reqs))])
          ;; we want whatever required module will have at this module's `phase`
          (define at-phase (phase- phase (car phase+reqs)))
          (link (module-path-index->module-name req name)
                at-phase)))

      ;; Get linklets implied by the module's `require` (although some
      ;; of those may turn out to be dead code)
      (for ([dependency (in-list dependencies)])
        (get-linklets! dependency #f))
      
      ;; Imports are the subset of the transitive closure of `require`
      ;; that are used by this linklet's implementation
      (define imports
        (for/list ([mu (in-list uses)])
          (link (module-path-index->module-name (module-use-module mu) name)
                (module-use-phase mu))))
      (when (and (pair? imports)
                 (not linklet))
        (error "no implementation, but uses arguments?" name phase))

      ;; Re-exports are the subset of the transitive closure of
      ;; `require` that have variables that are re-exported from this
      ;; linklet; relevant only for the starting point
      (define re-exports
        (and first?
             (set->list
              (for*/set ([(sym binding/p) (in-hash (hash-ref provs phase #hasheq()))]
                         [(binding) (in-value (provided-as-binding binding/p))]
                         [l (in-value
                             (link (module-path-index->module-name (module-binding-module binding) name)
                                   (module-binding-phase binding)))]
                         [re-li (in-value (hash-ref linklets l #f))]
                         #:when (and re-li
                                     (set-member? (linklet-info-variables re-li) (module-binding-sym binding))))
                l))))

      (define li (linklet-info linklet imports re-exports vars in-vars side-effects?))

      (hash-set! seen lnk li)
      
      (when linklet
        (hash-set! linklets lnk li)
        (set-box! linklets-in-order (cons lnk (unbox linklets-in-order)))))))
