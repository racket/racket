#lang racket/base
(require "../eval/main.rkt"
         "../eval/dynamic-require.rkt"
         "../eval/reflect.rkt"
         "../eval/load.rkt"
         "../eval/parameter.rkt"
         "../eval/collection.rkt"
         (prefix-in wrapper: "../eval/api.rkt")
         "../compile/recompile.rkt"
         "../namespace/namespace.rkt"
         "../namespace/api.rkt"
         "../namespace/attach.rkt"
         "../namespace/api-module.rkt")

(provide main-primitives)

(define main-primitives
  (hasheq 'eval wrapper:eval
          'eval-syntax wrapper:eval-syntax
          'compile wrapper:compile
          'compile-syntax wrapper:compile-syntax
          'expand wrapper:expand
          'expand-syntax wrapper:expand-syntax
          'expand-once wrapper:expand-once
          'expand-syntax-once wrapper:expand-syntax-once
          'expand-to-top-form wrapper:expand-to-top-form
          'expand-syntax-to-top-form wrapper:expand-syntax-to-top-form
          'dynamic-require dynamic-require
          'dynamic-require-for-syntax dynamic-require-for-syntax
          'load load
          'load-extension load-extension
          'load/use-compiled load/use-compiled

          'current-eval current-eval
          'current-compile current-compile
          'current-load current-load
          'current-load/use-compiled current-load/use-compiled

          'collection-path collection-path
          'collection-file-path collection-file-path
          'find-library-collection-paths find-library-collection-paths
          'find-library-collection-links find-library-collection-links

          'current-library-collection-paths current-library-collection-paths
          'current-library-collection-links current-library-collection-links
          'use-compiled-file-paths use-compiled-file-paths
          'current-compiled-file-roots current-compiled-file-roots
          'use-compiled-file-check use-compiled-file-check
          'use-collection-link-paths use-collection-link-paths
          'use-user-specific-search-paths use-user-specific-search-paths

          'compiled-expression? compiled-expression?
          'compiled-module-expression? compiled-module-expression?
          'module-compiled-name module-compiled-name
          'module-compiled-submodules module-compiled-submodules
          'module-compiled-language-info module-compiled-language-info
          'module-compiled-imports module-compiled-imports
          'module-compiled-exports module-compiled-exports
          'module-compiled-indirect-exports module-compiled-indirect-exports

          'compiled-expression-recompile compiled-expression-recompile

          'make-empty-namespace make-empty-namespace

          'namespace-attach-module namespace-attach-module
          'namespace-attach-module-declaration namespace-attach-module-declaration

          'namespace-symbol->identifier namespace-symbol->identifier
          'namespace-module-identifier namespace-module-identifier
          'namespace-syntax-introduce namespace-syntax-introduce
          'namespace-require namespace-require
          'namespace-require/copy namespace-require/copy
          'namespace-require/constant namespace-require/constant
          'namespace-require/expansion-time namespace-require/expansion-time
          'namespace-variable-value namespace-variable-value
          'namespace-set-variable-value! namespace-set-variable-value!
          'namespace-undefine-variable!	namespace-undefine-variable!
          'namespace-mapped-symbols namespace-mapped-symbols 
          'namespace-base-phase namespace-base-phase          
          
          'module-declared? module-declared?
          'module-predefined? module-predefined?
          'module->language-info module->language-info
          'module->imports module->imports
          'module->exports module->exports
          'module->indirect-exports module->indirect-exports
          'module-compiled-cross-phase-persistent? module-compiled-cross-phase-persistent?
          'module-provide-protected? module-provide-protected?
          'module->namespace module->namespace
          'namespace-unprotect-module namespace-unprotect-module))
