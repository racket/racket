#lang racket/base
(require "../common/set.rkt"
         "../syntax/srcloc.rkt"
         "../common/phase.rkt"
         (except-in "../syntax/scope.rkt"
                    syntax-e
                    bound-identifier=?
                    syntax-shift-phase-level)
         "../namespace/namespace.rkt"
         (except-in "../syntax/binding.rkt"
                    free-identifier=?
                    identifier-binding
                    identifier-binding-symbol)
         "../namespace/core.rkt"
         "../expand/set-bang-trans.rkt"
         "../expand/rename-trans.rkt"
         "../expand/liberal-def-ctx.rkt"
         "../expand/syntax-local.rkt"
         "../expand/definition-context.rkt"
         "../expand/local-expand.rkt"
         "../syntax/api.rkt"
         "../syntax/api-taint.rkt"
         "../syntax/error.rkt"
         "../read/api.rkt"
         "../common/module-path.rkt"
         "../namespace/variable-reference.rkt"
         "../expand/allowed-context.rkt"
         "../expand/missing-module.rkt")

(provide primitive-ids)

;; Register core primitives:
(define-syntax-rule (add-core-primitives! #:table primitive-ids id ...)
  (begin
    (define primitive-ids (seteq 'id ...))
    (void
     (begin
       (add-core-primitive! 'id id)
       ...))))

(add-core-primitives! #:table primitive-ids
                      
                      syntax?
                      syntax-e
                      syntax->datum
                      datum->syntax

                      bound-identifier=?
                      free-identifier=?
                      free-transformer-identifier=?
                      free-template-identifier=?
                      free-label-identifier=?
                      identifier-binding
                      identifier-transformer-binding
                      identifier-template-binding
                      identifier-label-binding
                      identifier-binding-symbol
                      identifier-prune-lexical-context
                      syntax-debug-info
                      syntax-track-origin
                      syntax-shift-phase-level
                      syntax-source-module
                      identifier-prune-to-source-module
                      
                      syntax-source
                      syntax-line
                      syntax-column
                      syntax-position
                      syntax-span
                      syntax->list
                      syntax-property
                      syntax-property-remove
                      syntax-property-preserved?
                      syntax-property-symbol-keys
                      syntax-original?
                      
                      syntax-tainted?
                      syntax-arm
                      syntax-disarm
                      syntax-rearm
                      syntax-taint

                      syntax-binding-set
                      syntax-binding-set?
                      syntax-binding-set-extend
                      syntax-binding-set->syntax

                      raise-syntax-error
                      struct:exn:fail:syntax
                      exn:fail:syntax
                      make-exn:fail:syntax
                      exn:fail:syntax?
                      exn:fail:syntax-exprs
                      struct:exn:fail:syntax:unbound
                      exn:fail:syntax:unbound
                      make-exn:fail:syntax:unbound
                      exn:fail:syntax:unbound?
                      
                      current-module-path-for-load
                      prop:exn:missing-module
                      exn:missing-module?
                      exn:missing-module-accessor
                      struct:exn:fail:filesystem:missing-module
                      exn:fail:filesystem:missing-module
                      make-exn:fail:filesystem:missing-module
                      exn:fail:filesystem:missing-module?
                      exn:fail:filesystem:missing-module-path
                      struct:exn:fail:syntax:missing-module
                      exn:fail:syntax:missing-module
                      make-exn:fail:syntax:missing-module
                      exn:fail:syntax:missing-module?
                      exn:fail:syntax:missing-module-path
                      
                      syntax-transforming?
                      syntax-transforming-with-lifts?
                      syntax-transforming-module-expression?
                      syntax-local-transforming-module-provides?
                      
                      syntax-local-context
                      syntax-local-introduce
                      syntax-local-identifier-as-binding
                      syntax-local-phase-level
                      syntax-local-name

                      make-syntax-introducer
                      make-interned-syntax-introducer
                      make-syntax-delta-introducer
                      syntax-local-make-delta-introducer
                      
                      syntax-local-value
                      syntax-local-value/immediate
                      
                      syntax-local-lift-expression
                      syntax-local-lift-values-expression
                      syntax-local-lift-context
                      
                      syntax-local-lift-module
                      
                      syntax-local-lift-require
                      syntax-local-lift-provide
                      syntax-local-lift-module-end-declaration
                      
                      syntax-local-module-defined-identifiers
                      syntax-local-module-required-identifiers
                      syntax-local-module-exports
                      syntax-local-submodules
                      
                      syntax-local-get-shadower
                      
                      local-expand
                      local-expand/capture-lifts
                      local-transformer-expand
                      local-transformer-expand/capture-lifts
                      syntax-local-expand-expression

                      internal-definition-context?
                      syntax-local-make-definition-context
                      syntax-local-bind-syntaxes
                      internal-definition-context-binding-identifiers
                      internal-definition-context-introduce
                      internal-definition-context-seal
                      identifier-remove-from-definition-context
                      
                      make-set!-transformer
                      prop:set!-transformer
                      set!-transformer?
                      set!-transformer-procedure

                      rename-transformer?
                      prop:rename-transformer
                      make-rename-transformer
                      rename-transformer-target

                      prop:liberal-define-context
                      liberal-define-context?
                      
                      prop:expansion-contexts
                      
                      module-path?

                      resolved-module-path?
                      make-resolved-module-path
                      resolved-module-path-name
                      
                      module-path-index?
                      module-path-index-resolve
                      module-path-index-join
                      module-path-index-split
                      module-path-index-submodule

                      current-module-name-resolver
                      current-module-declare-name
                      current-module-declare-source
                      
                      current-namespace
                      namespace-module-registry
                      namespace?
                      
                      variable-reference->empty-namespace
                      variable-reference->namespace
                      variable-reference->resolved-module-path
                      variable-reference->module-path-index
                      variable-reference->module-source
                      variable-reference->phase
                      variable-reference->module-base-phase
                      variable-reference->module-declaration-inspector

                      read-syntax
                      read-syntax/recursive)
