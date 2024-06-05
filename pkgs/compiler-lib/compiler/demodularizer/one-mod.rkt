#lang racket/base

(provide (struct-out one-mod))

;; A (sub)module without its own submodules
(struct one-mod (order ; integer that represents `require` order, lower is first
                 excluded?
                 rel-mpi ; used if the module is a singleton pane and changed to excluded
                 zo
                 decl
                 phase-uses  ; phase-level -> (list (cons path/submod phase-level) ...) for linklet imports
                 reqs    ; phase-shift -> (list path/submod ...) preserving original order
                 exports ; phase-level -> ext-name -> int-name for linklet exports
                 min-phase max-phase ; reachable phases via transitive requires
                 provides ; phase-level -> sym -> provided
                 stx-vec stx-mpi portal-stxes
                 pre-submodules
                 post-submodules))
