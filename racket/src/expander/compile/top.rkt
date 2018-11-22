#lang racket/base
(require "serialize.rkt"
         "../host/linklet.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../namespace/namespace.rkt"
         "../expand/root-expand-context.rkt"
         "../expand/parsed.rkt"
         "../compile/reserved-symbol.rkt"
         "../common/performance.rkt"
         "../eval/top-level-instance.rkt"
         "compiled-in-memory.rkt"
         "linklet.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "instance.rkt"
         "eager-instance.rkt"
         "expr.rkt"
         "form.rkt"
         "multi-top.rkt"
         "namespace-scope.rkt"
         "side-effect.rkt"
         "correlated-linklet.rkt")

(provide compile-single
         compile-top)

;; Compile a stand-alone expression, such as the right-hand side of a
;; `define-syntaxes` in a module
(define (compile-single p cctx)
  (compile-top p cctx
               #:serializable? #f
               #:single-expression? #t))

;; Compile a single form, which can be a `define-values` form, a
;; `define-syntaxes` form, or an expression (where `begin` is treated
;; as an expression form). If `serializable?` is false, don't bother
;; generating the linklet for serialized data, because it won't be
;; used.
(define (compile-top p cctx
                     #:serializable? [serializable? #t]
                     #:single-expression? [single-expression? #f]
                     #:to-correlated-linklet? [to-correlated-linklet? #f])
  (performance-region
   ['compile (if single-expression? 'transformer 'top)]

   (define phase (compile-context-phase cctx))

   (define mpis (make-module-path-index-table))
   (define purely-functional? #t)

   ;; Compile the body forms, similar to compiling the body of a module
   (define-values (body-linklets
                   min-phase
                   max-phase
                   phase-to-link-module-uses
                   phase-to-link-module-uses-expr
                   phase-to-link-extra-inspectorss
                   syntax-literals
                   no-root-context-pos)
     (compile-forms (list p) cctx mpis
                    #:body-imports (if single-expression?
                                       `([]
                                         [,syntax-literals-id]
                                         [])
                                       `([,top-level-bind!-id
                                          ,top-level-require!-id]
                                         [,mpi-vector-id
                                          ,syntax-literals-id]
                                         ,instance-imports))
                    #:body-import-instances (list top-level-instance
                                                  empty-top-syntax-literal-instance
                                                  empty-instance-instance)
                    #:serializable? serializable?
                    #:to-correlated-linklet? to-correlated-linklet?
                    #:definition-callback (lambda () (set! purely-functional? #f))
                    #:compiled-expression-callback
                    (lambda (e expected-results phase required-reference?)
                      (when (and purely-functional?
                                 (any-side-effects? e expected-results #:ready-variable? required-reference?))
                        (set! purely-functional? #f)))
                    #:other-form-callback (lambda (s cctx)
                                            (set! purely-functional? #f)
                                            (compile-top-level-require s cctx))
                    #:cross-linklet-inlining? (not single-expression?)))

   (define (add-metadata ht)
     (let* ([ht (hash-set ht 'original-phase phase)]
            [ht (hash-set ht 'max-phase max-phase)])
       ht))
   
   (define bundle
     (hash->linklet-bundle
      (add-metadata
       (cond
        [serializable?
         ;; To support seialization, construct a linklet that will
         ;; deserialize module path indexes, syntax objects, etc.
         (define syntax-literals-expr
           (performance-region
            ['compile 'top 'serialize]
            (generate-eager-syntax-literals! 
             syntax-literals
             mpis
             phase
             (compile-context-self cctx)
             (compile-context-namespace cctx))))

         (define link-linklet
           ((lambda (s)
              (if to-correlated-linklet?
                  (make-correlated-linklet s #f)
                  (performance-region
                   ['compile 'top 'linklet]
                   (define-values (linklet new-keys)
                     (compile-linklet s
                                      #f
                                      (vector deserialize-instance
                                              empty-eager-instance-instance)
                                      (lambda (inst) (values inst #f))))
                   linklet)))
            `(linklet
              ;; imports
              (,deserialize-imports
               ,eager-instance-imports)
              ;; exports
              (,mpi-vector-id
               ,deserialized-syntax-vector-id
               phase-to-link-modules
               ,syntax-literals-id)
              (define-values (,mpi-vector-id)
                ,(generate-module-path-index-deserialize mpis))
              (define-values (,deserialized-syntax-vector-id) 
                (make-vector ,(add1 phase) #f))
              (define-values (phase-to-link-modules) ,phase-to-link-module-uses-expr)
              (define-values (,syntax-literals-id) ,syntax-literals-expr))))
         
         (hash-set body-linklets 'link link-linklet)]
        [else
         ;; Will combine the linking unit with non-serialized link info
         body-linklets]))))
   
   ;; If the compiled code is executed directly, it must be in its
   ;; original phase, and we'll share the original values
   (compiled-in-memory (hash->linklet-directory (hasheq #f bundle))
                       #f ; self
                       #f ; requires
                       #f ; provides
                       phase-to-link-module-uses
                       (current-code-inspector)
                       phase-to-link-extra-inspectorss
                       (mpis-as-vector mpis)
                       (syntax-literals-as-vector syntax-literals)
                       null
                       null
                       (extract-namespace-scopes (compile-context-namespace cctx))
                       purely-functional?)))

;; Callback for compiling a sequence of expressions: handle `require`
;; (which is handled separately for modules)
(define (compile-top-level-require p cctx)
  (define phase (compile-context-phase cctx))
  (cond
   [(parsed-require? p)
    (define form-stx (compile-quote-syntax (syntax-disarm (parsed-s p)) cctx))
    `(,top-level-require!-id ,form-stx ,ns-id)]
   [else #f]))
