#lang racket/base
(require "common/set.rkt"
         "common/module-path.rkt"
         "namespace/namespace.rkt"
         "eval/main.rkt"
         (only-in "eval/api.rkt"
                  [eval eval-top-level])
         "eval/dynamic-require.rkt"
         "eval/reflect.rkt"
         "eval/load.rkt"
         "eval/collection.rkt"
         "eval/parameter.rkt"
         "read/api.rkt"
         "read/primitive-parameter.rkt"
         "namespace/api.rkt"
         "namespace/attach.rkt"
         "namespace/api-module.rkt"
         "namespace/core.rkt"
         "namespace/primitive-module.rkt"
         "expand/missing-module.rkt"
         (only-in "compile/correlate.rkt"
                  compile-keep-source-locations!)
         "boot/kernel.rkt"
         "boot/read-primitive.rkt"
         "boot/main-primitive.rkt"
         "boot/utils-primitive.rkt"
         "boot/expobs-primitive.rkt"
         "boot/place-primitive.rkt"
         "boot/linklet-primitive.rkt"
         "boot/runtime-primitive.rkt"
         "boot/handler.rkt"
         "syntax/api.rkt"
         (only-in "compile/recompile.rkt"
                  compiled-expression-recompile)
         (only-in racket/private/config find-main-config)
         (only-in "syntax/cache.rkt" cache-place-init!)
         (only-in "syntax/scope.rkt" scope-place-init!)
         (only-in "eval/module-cache.rkt" module-cache-place-init!)
         (only-in "common/performance.rkt" performance-place-init!))

;; All bindings provided by this module must correspond to variables
;; (as opposed to syntax). Provided functions must not accept keyword
;; arguments, both because keyword support involves syntax bindings
;; and because an embedding context won't be able to supply keyword
;; arguments.

(provide boot ; installs handlers: eval, module name resolver, etc.
         seal

         ;; These are direct functions, not ones that use handlers:
         expand
         compile
         eval
         read

         ;; Uses handlers:
         eval-top-level

         load
         load/use-compiled
         load-extension

         current-eval
         current-compile
         current-load
         current-load/use-compiled

         find-library-collection-paths
         find-library-collection-links
         find-main-config

         current-library-collection-paths
         current-library-collection-links
         use-compiled-file-paths
         current-compiled-file-roots
         use-compiled-file-check
         use-collection-link-paths
         use-user-specific-search-paths
         
         syntax?
         read-syntax
         datum->syntax syntax->datum
         identifier-binding
         datum->kernel-syntax
         maybe-syntax->datum ; for reader callbacks via a readtable, etc.
         
         make-namespace
         current-namespace
         namespace->instance
         
         namespace-syntax-introduce
         namespace-datum-introduce
         namespace-require
         dynamic-require         
         module-declared?
         module-predefined?
         module->language-info
         maybe-raise-missing-module
         
         namespace-module-identifier
         namespace-attach-module
         namespace-attach-module-declaration
         namespace-mapped-symbols
         namespace-variable-value
         
         module-path-index?
         module-path-index-join
         resolved-module-path?
         module-path?

         path-list-string->path-list ; for startup

         declare-primitive-module! ; to support "extensions"

         embedded-load ; for -k

         compile-keep-source-locations! ; to enable if the back end wants them

         expander-place-init!

         ;; The remaining functions are provided for basic testing
         ;; (such as "demo.rkt")

         syntax? syntax-e
         identifier?
         syntax-property
         syntax-debug-info
         module-compiled-exports
         module-compiled-indirect-exports
         read-accept-compiled

         syntax-shift-phase-level
         bound-identifier=?

         compiled-expression-recompile)

;; ----------------------------------------

;; Register core forms:
(require "expand/expr.rkt"
         "expand/module.rkt"
         "expand/top.rkt")

;; Register core primitives:
(require "boot/core-primitive.rkt")

;; ----------------------------------------
;; Initial namespace

(define (namespace-init!)
  (define ns (make-namespace))
  (void
   (begin
     (declare-core-module! ns)
     (declare-hash-based-module! '#%read read-primitives #:namespace ns)
     (declare-hash-based-module! '#%main main-primitives #:namespace ns)
     (declare-hash-based-module! '#%utils utils-primitives #:namespace ns)
     (declare-hash-based-module! '#%place-struct place-struct-primitives #:namespace ns
                                 ;; Treat place creation as "unsafe", since the new place starts with
                                 ;; permissive guards that can access unsafe features that affect
                                 ;; existing places
                                 #:protected '(dynamic-place))
     (declare-hash-based-module! '#%boot boot-primitives #:namespace ns)
     (let ([linklet-primitives
            ;; Remove symbols that are in the '#%linklet primitive table
            ;; but provided by `#%kernel`:
            (hash-remove (hash-remove linklet-primitives
                                      'variable-reference?)
                         'variable-reference-constant?)])
       (declare-hash-based-module! '#%linklet-primitive linklet-primitives #:namespace ns
                                   #:primitive? #t
                                   #:register-builtin? #t)
       (declare-hash-based-module! '#%linklet-expander linklet-expander-primitives #:namespace ns)
       (declare-reexporting-module! '#%linklet (list '#%linklet-primitive
                                                     '#%linklet-expander)
                                    #:namespace ns))
     (declare-hash-based-module! '#%expobs expobs-primitives #:namespace ns
                                 #:protected? #t)
     (declare-kernel-module! ns
                             #:main-ids (for/set ([name (in-hash-keys main-primitives)])
                                          name)
                             #:read-ids (for/set ([name (in-hash-keys read-primitives)])
                                          name))
     (for ([name (in-list runtime-instances)]
           #:unless (eq? name '#%kernel))
       (copy-runtime-module! name
                             #:namespace ns
                             #:protected? (or (eq? name '#%foreign)
                                              (eq? name '#%futures)
                                              (eq? name '#%unsafe))))
     (declare-reexporting-module! '#%builtin (list* '#%place-struct
                                                    '#%utils
                                                    '#%boot
                                                    '#%expobs
                                                    '#%linklet
                                                    runtime-instances)
                                  #:namespace ns
                                  #:reexport? #f)
     (current-namespace ns)

     (dynamic-require ''#%kernel 0))))

(namespace-init!)

(define (datum->kernel-syntax s)
  (datum->syntax core-stx s))

(define (expander-place-init!)
  (scope-place-init!)
  (cache-place-init!)
  (core-place-init!)
  (module-path-place-init!)
  (module-cache-place-init!)
  (collection-place-init!)
  (performance-place-init!)
  (namespace-init!))
