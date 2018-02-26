#lang racket/base
(require "../expand/root-expand-context.rkt"
         "../expand/require.rkt"
         "../expand/def-id.rkt"
         "../expand/env.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/module-binding.rkt"
         "../common/module-path.rkt"
         "../common/phase.rkt"
         "../host/linklet.rkt")

(provide make-create-root-expand-context-from-module)

;; Reconstructs a `root-expand-context` for a module based on its
;; metadata, specifically its requires and the exports of its
;; linklets. Reconstructing that way works as long as there are no
;; transformer definitions, since transformer definitions are not
;; visible outside a linklet. Typically, also, we can only do this
;; when the module contained no syntax literals, which would likely
;; contain information that is inconsistent with this reconstruction.
(define (make-create-root-expand-context-from-module requires evaled-ld-h)
  (lambda (ns phase-shift original-self self)
    (define root-ctx (make-root-expand-context))
    (define s (add-scopes empty-syntax (root-expand-context-module-scopes root-ctx)))

    ;; Add bindings for `require`s
    (for ([(phase+reqs) (in-list requires)])
      (define phase (car phase+reqs))
      (for ([req (in-list (cdr phase+reqs))])
        (define mpi (module-path-index-shift req original-self self))
        (perform-require! mpi s self
                          s ns
                          #:phase-shift (phase+ phase phase-shift)
                          #:run-phase phase-shift
                          #:who 'module)))
    
    ;; Add bindings for `define`s, including registering symbols used
    ;; by those definitions (some of which might be macro-introduced)
    (define defined-syms (root-expand-context-defined-syms root-ctx))
    (for ([(phase linklet) (in-hash evaled-ld-h)])
      (for ([sym (in-list (linklet-export-variables linklet))])
        ;; Note that sym might be an unreadable symbol, in which case
        ;; the binding should be unreachable, but we need to reserve
        ;; the symbol to avoid conflicts
        (define id (datum->syntax s sym))
        (add-binding! id (make-module-binding self phase sym) phase)
        (add-defined-sym! defined-syms phase sym id)))
    
    root-ctx))
