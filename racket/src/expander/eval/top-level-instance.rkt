#lang racket/base
(require "../syntax/to-list.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../common/phase.rkt"
         "../namespace/namespace.rkt"
         "../expand/root-expand-context.rkt"
         "../compile/reserved-symbol.rkt"
         "../syntax/module-binding.rkt"
         "../host/linklet.rkt"
         "../expand/env.rkt"
         "../expand/require.rkt"
         "../expand/require+provide.rkt")

;; Run-time support for evaluating top-level forms
(provide top-level-instance)

(define top-level-instance
  (make-instance
   'top-level #f 'constant
   
   top-level-bind!-id
   (lambda (id mpi orig-phase phase-shift ns sym trans? trans-val)
     (define phase (phase+ orig-phase phase-shift))
     (define b (make-module-binding mpi phase sym
                                    #:frame-id (root-expand-context-frame-id
                                                (namespace-get-root-expand-ctx ns))))
     (add-binding! id b phase)
     (cond
      [trans?
       (when trans-val
         (maybe-install-free=id! trans-val id phase))]
      [else
       (namespace-unset-transformer! ns phase sym)]))
   
   top-level-require!-id
   (lambda (stx ns)
     (define reqs (cdr (syntax->list stx)))
     (parse-and-perform-requires! #:run? #t
                                  #:visit? #f
                                  reqs
                                  #f ; no syntax errors should happen
                                  ns
                                  (namespace-phase ns)
                                  (make-requires+provides #f)
                                  #:who 'require
                                  ;; We don't need to check for conflicts
                                  ;; or adjust the requires+provides:
                                  #:initial-require? #t))))
