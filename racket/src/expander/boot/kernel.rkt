#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../namespace/core.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/provided.rkt"
         "../syntax/binding.rkt"
         "core-primitive.rkt"
         "../common/module-path.rkt"
         "../expand/require+provide.rkt"
         "../host/linklet.rkt"
         "../compile/built-in-symbol.rkt")

;; The '#%kernel module combines '#%core, '#%runtime, and '#%main

(provide declare-kernel-module!
         copy-runtime-module!
         declare-hash-based-module!
         declare-reexporting-module!)

(define (declare-kernel-module! ns
                                #:main-ids main-ids
                                #:read-ids read-ids)
  (copy-runtime-module! '#%kernel
                        #:to '#%runtime
                        #:skip (set-union primitive-ids
                                          (set-union main-ids
                                                     read-ids))
                        #:extras (hasheq 'variable-reference? variable-reference?
                                         'variable-reference-constant? variable-reference-constant?
                                         'variable-reference-from-unsafe? variable-reference-from-unsafe?)
                        #:namespace ns)
  (declare-reexporting-module! '#%kernel '(#%core #%runtime #%main #%read)
                               #:namespace ns))
 
(define (copy-runtime-module! name
                              #:to [to-name name]
                              #:namespace ns
                              #:skip [skip-syms (seteq)]
                              #:alts [alts #hasheq()]
                              #:extras [extras #hasheq()]
                              #:primitive? [primitive? #t]
                              #:protected? [protected? #f])
  (define prims (primitive-table name))
  (for ([sym (in-hash-keys prims)])
    (register-built-in-symbol! sym))
  (define ht (for/hasheq ([(sym val) (in-hash prims)]
                          #:unless (set-member? skip-syms sym))
               (values sym
                       (or (hash-ref alts sym #f) val))))
  (define ht+extras (for/fold ([ht ht]) ([(k v) (in-hash extras)])
                      (hash-set ht k v)))
  (declare-hash-based-module! to-name ht+extras
                              #:namespace ns
                              #:primitive? primitive?
                              #:protected? protected?))

(define (declare-hash-based-module! name ht
                                    #:namespace ns
                                    #:primitive? [primitive? #f]
                                    #:protected? [protected? #f]
                                    #:protected [protected-syms null]
                                    #:register-builtin? [register-builtin? #f])
  (define mpi (module-path-index-join (list 'quote name) #f))
  (declare-module!
   ns
   (make-module #:cross-phase-persistent? #t
                #:primitive? primitive?
                #:predefined? #t
                #:no-protected? (not protected?)
                #:self mpi
                #:provides
                (hasheqv 0 (for/hash ([sym (in-hash-keys ht)])
                             (when register-builtin?
                               (register-built-in-symbol! sym))
                             (define binding (make-module-binding mpi 0 sym))
                             (values sym
                                     (if (or protected?
                                             (member sym protected-syms))
                                         (provided binding #t #f)
                                         binding))))
                #:instantiate-phase-callback
                (lambda (data-box ns phase-shift phase-level self bulk-binding-registry insp)
                  (when (= 0 phase-level)
                    (for ([(sym val) (in-hash ht)])
                      (namespace-set-variable! ns 0 sym val #t)))))
   (module-path-index-resolve mpi)))

(define (declare-reexporting-module! name require-names
                                     #:reexport? [reexport? #t]
                                     #:namespace ns)
  (define mpi (module-path-index-join (list 'quote name) #f))
  (define require-mpis (for/list ([require-name (in-list require-names)])
                         (module-path-index-join (list 'quote require-name) #f)))
  (declare-module!
   ns
   (make-module #:cross-phase-persistent? #t
                #:predefined? #t
                #:self mpi
                #:requires (list (cons 0 require-mpis))
                #:provides
                (if reexport?
                    (hasheqv 0
                             (for*/hash ([require-mpi (in-list require-mpis)]
                                         [m (in-value (namespace->module
                                                       ns
                                                       (module-path-index-resolve require-mpi)))]
                                         [(sym binding) (in-hash
                                                         (hash-ref
                                                          (shift-provides-module-path-index
                                                           (module-provides m)
                                                           (module-self m)
                                                           require-mpi)
                                                          0))])
                               (values sym binding)))
                    #hasheqv())
                #:instantiate-phase-callback void)
   (module-path-index-resolve mpi)))
