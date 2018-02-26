#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../common/module-path.rkt")

(provide runtime-stx
         runtime-module-name

         runtime-instances)

;; Runtime primitives are implemented in the runtime system (and not
;; shadowed by the expander's primitives). They're re-exported by
;; '#%kernel, but originally exported by a '#%runtime module. The
;; expander needs to generate references to some '#%runtime` bindings.

(define runtime-scope (new-multi-scope))
(define runtime-stx (add-scope empty-syntax runtime-scope))

(define runtime-module-name (make-resolved-module-path '#%runtime))
(define runtime-mpi (module-path-index-join ''#%runtime #f))

(define (add-runtime-primitive! sym)
  (add-binding-in-scopes! (syntax-scope-set runtime-stx 0)
                          sym
                          (make-module-binding runtime-mpi 0 sym)))

;; This is only a subset that we need to have bound;
;; the rest are added in "kernel.rkt"
(void
 (begin
   (add-runtime-primitive! 'values)
   (add-runtime-primitive! 'cons)
   (add-runtime-primitive! 'list)
   (add-runtime-primitive! 'make-struct-type)
   (add-runtime-primitive! 'make-struct-type-property)
   (add-runtime-primitive! 'gensym)
   (add-runtime-primitive! 'string->uninterned-symbol)))

;; Instances that are built into the runtime system, but
;; not including '#%linklet
(define runtime-instances
  '(#%kernel
    #%paramz
    #%foreign
    #%unsafe
    #%flfxnum
    #%extfl
    #%network
    #%place
    #%futures))
