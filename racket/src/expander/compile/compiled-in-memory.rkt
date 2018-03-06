#lang racket/base

;; A `compiled-in-memory` structure holds the result of compilation.
;; It's produced by `compile-top` or `compile-module` and consumed by
;; `eval-compiled-in-memory`. The marshaled form is just the linklet
;; directory, which has the same essential information, but loses sharing
;; with anything else currently in memory. The marshaled form also loses
;; extra inspectors.
(provide (struct-out compiled-in-memory))

(struct compiled-in-memory (linklet-directory ;; includes content of `{pre,post}-compiled-tops`; may be just a bundle
                            ;; Shortcuts, instead of using the metadata linklet:
                            original-self
                            requires
                            provides
                            phase-to-link-module-uses
                            ;; Maybe provide more capability than the module's declaration inspector:
                            compile-time-inspector
                            ;; For each phase (that has a linklet), optionally report
                            ;; a list of lists; the outer list matches the order of imports
                            ;; into the linklet, and each inner list matches the order of
                            ;; variables from that imported linklet; each member of the
                            ;; inner list is #f or an extra inspector that has been carried
                            ;; over from the originally compiled reference
                            phase-to-link-extra-inspectorsss ; phase -> list of hash tables to "extra inspectors"
                            ;; For using existing values directly, instead of unmarshaling:
                            mpis
                            syntax-literals
                            ;; Shortcuts for associated code (submodules or sequence of top levels)
                            pre-compiled-in-memorys
                            post-compiled-in-memorys
                            ;; Namespace scopes from top-level compilation, so syntax objects
                            ;; can be adjusted for a target namespace:
                            namespace-scopes
                            ;; To track whether a form in a top-level sequence can be discarded:
                            purely-functional?)
  #:property prop:custom-write
  (lambda (cim port mode)
    (write (compiled-in-memory-linklet-directory cim) port)))
