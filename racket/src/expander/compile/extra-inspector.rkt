#lang racket/base
(require "../common/set.rkt"
         "module-use.rkt"
         "../host/linklet.rkt")

(provide extra-inspectors-allow?

         module-uses-add-extra-inspectorsss
         module-uses-strip-extra-inspectorsss
         module-uses-extract-extra-inspectorsss
         module-use*-declaration-inspector!

         module-use+extra-inspectors
         module-use-merge-extra-inspectorss!)

;; Compilation leaves a linklet with some "or" inspectors that apply
;; to the whole linklet plus (potentially) some "and" inspectors for
;; each invdidual binding. Cross-module optimization can move this
;; or+and combination to the "and" part of a different module, so we
;; use functions in general

(define (extra-inspectors-allow? extra-inspectors guard-insp)
  (cond
    [(not extra-inspectors) #f]
    [(set? extra-inspectors)
     (for/and ([extra-insp (in-set extra-inspectors)])
       (inspector-superior? extra-insp guard-insp))]
    [(procedure? extra-inspectors)
     (extra-inspectors guard-insp)]
    [else
     (error 'extra-inspectors-allow?
            "unknown representation of extra inspectors: ~e"
            extra-inspectors)]))

(define (extra-inspectors-merge extra-inspectors-1 extra-inspectors-2)
  (cond
    [(or (not extra-inspectors-1)
         (not extra-inspectors-2))
     #f]
    [(and (set? extra-inspectors-1)
          (set? extra-inspectors-2))
     (set-union extra-inspectors-1 extra-inspectors-2)]
    [else
     (lambda (guard-insp)
       (and (extra-inspectors-allow? extra-inspectors-1 guard-insp)
            (extra-inspectors-allow? extra-inspectors-2 guard-insp)))]))

;; ----------------------------------------

;; While compiling a linklet, we start out with parallel lists of
;; module uses and extra inspectors, but it's more convenient to
;; manage inlining if we put those together. We may need to merge
;; extra-inspector sets while preserving `eq?` identity of the
;; `module-use*`, so that field is mutable.
(struct module-use* module-use ([extra-inspectorss #:mutable]
                                [self-inspector #:mutable]))

;; Parallel lists into one list
(define (module-uses-add-extra-inspectorsss mus extra-inspectorsss)
  (cond
    [extra-inspectorsss
     (for/list ([mu (in-list mus)]
                [extra-inspectorss (in-list extra-inspectorsss)])
       (module-use* (module-use-module mu)
                    (module-use-phase mu)
                    extra-inspectorss
                    #f))]
    [else
     (for/list ([mu (in-list mus)])
       (module-use* (module-use-module mu)
                    (module-use-phase mu)
                    #f
                    #f))]))

;; Split the list back into one of the parallel lists
(define (module-uses-strip-extra-inspectorsss mu*s)
  (for/list ([mu* (in-list mu*s)])
    (module-use (module-use-module mu*)
                (module-use-phase mu*))))

;; Split the list back into the other parallel list --- but also check
;; for inlining-introduced references that must have formerly been
;; module-internal references (i.e., referenecs that are not already
;; recorded as imports)
(define (module-uses-extract-extra-inspectorsss mu*s linklet check-inlined-reference? skip-n)
  (cond
    [(not check-inlined-reference?)
     (for/list ([mu* (in-list mu*s)])
       (module-use*-extra-inspectorss mu*))]
    [else
     (for/list ([mu* (in-list mu*s)]
                [imports (in-list (list-tail (linklet-import-variables linklet) skip-n))])
       (define extra-inspectorss (module-use*-extra-inspectorss mu*))
       (for/fold ([extra-inspectorss extra-inspectorss]) ([import (in-list imports)])
         (cond
           [(eq? (hash-ref extra-inspectorss import '#:not-recorded) '#:not-recorded)
            (hash-set extra-inspectorss import (set (module-use*-self-inspector mu*)))]
           [else extra-inspectorss])))]))

(define (module-use*-declaration-inspector! mu* insp)
  (set-module-use*-self-inspector! mu* insp))

;; ----------------------------------------

(define (module-use+extra-inspectors mpi phase imports inspector extra-inspector extra-inspectorss)
  ;; If `inspector` or `extra-inspector` is not subsumed by the
  ;; current inspector, then propagate it by adding to each imported
  ;; variable's set of "or" inspectors
  (define now-inspector (current-code-inspector))
  (define add-insp? (and inspector (inspector-superior? inspector now-inspector)))
  (define add-extra-insp? (and extra-inspector (inspector-superior? extra-inspector now-inspector)))
  (define new-extra-inspectorss
    (cond
      [(or add-insp? add-extra-insp?)
       (for/hash ([import (in-list imports)])
         (values import
                 (let ([extra-inspectors (and extra-inspectorss
                                              (hash-ref extra-inspectorss import #f))])
                   (lambda (guard-insp)
                     (or (and add-insp? (inspector-superior? inspector guard-insp))
                         (and add-extra-insp? (inspector-superior? extra-inspector guard-insp))
                         (extra-inspectors-allow? extra-inspectors guard-insp))))))]
      [else
       ;; Make sure every import is mapped, because w may need to distinguish
       ;; between "not accessed" and "accessed without extra inspectors"
       (for/fold ([extra-inspectorss (or extra-inspectorss (seteq))]) ([import (in-list imports)])
         (if (hash-ref extra-inspectorss import #f)
             extra-inspectorss
             (hash-set extra-inspectorss import #f)))]))
  (module-use* mpi phase new-extra-inspectorss #f))

;; Merge inspectors from potentially different paths through imported linklets
(define (module-use-merge-extra-inspectorss! existing-mu* mu*)
  (define existing-extra-inspectorss (module-use*-extra-inspectorss existing-mu*))
  (define extra-inspectorss (module-use*-extra-inspectorss mu*))
  (define new-extra-inspectorss
    (cond
      [(not existing-extra-inspectorss) extra-inspectorss]
      [(not extra-inspectorss) existing-extra-inspectorss]
      [else
       (for/fold ([new-extra-inspectorss existing-extra-inspectorss]) ([(sym extra-inspectors) (in-hash extra-inspectorss)])
         (hash-set new-extra-inspectorss
                   sym
                   (extra-inspectors-merge extra-inspectors
                                           (hash-ref new-extra-inspectorss sym (seteq)))))]))
  (set-module-use*-extra-inspectorss! existing-mu* new-extra-inspectorss))
