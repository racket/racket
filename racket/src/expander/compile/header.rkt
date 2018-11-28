#lang racket/base
(require "../common/set.rkt"
         "../syntax/scope.rkt"
         "module-use.rkt"
         "../common/module-path.rkt"
         "context.rkt"
         "built-in-symbol.rkt"
         "reserved-symbol.rkt"
         "namespace-scope.rkt"
         "serialize.rkt")

(provide (struct-out header)
         make-header
         
         make-syntax-literals
         syntax-literals-empty?
         syntax-literals-count
         add-syntax-literal!
         add-syntax-literals!
         generate-eager-syntax-literals!
         generate-eager-syntax-literal-lookup
         generate-lazy-syntax-literals!
         generate-lazy-syntax-literals-data!
         generate-lazy-syntax-literal-lookup
         syntax-literals-as-vector

         header-empty-syntax-literals?
         
         local-key->symbol
         select-fresh

         register-required-variable-use!
         register-as-defined!
         registered-as-required?
         generate-links+imports)

;; A compilation header accumulates information about syntax literals
;; and about referenced required and defined variables. This
;; information is accumulated while compiling expressions, and then
;; header information is extracted into deserialization code that
;; reconstructs syntax literals, module path indexes, and so on. The
;; header also keeps track of which variable references correspond to
;; which linklet imports, and it keeps track of compile-time
;; inspectors that may grant access to some of those imports.

(struct syntax-literals ([stxes #:mutable]
                         [count #:mutable]))

(struct header (module-path-indexes        ; module-path-index -> linklet import position
                binding-sym-to-define-sym  ; sym -> sym; avoid conflicts with primitives
                [binding-syms-in-order #:mutable] ; list of sym
                require-var-to-import-sym  ; variable-use -> sym
                import-sym-to-extra-inspectors ; sym -> set of inspectors
                [require-vars-in-order #:mutable] ; list of variable-use
                define-and-import-syms     ; hash of sym -> 'defined/'imported, to select distinct symbols
                syntax-literals))          ; syntax-literals

(struct variable-use (module-use sym)
        #:transparent) ; for hashing

(define (make-syntax-literals)
  (syntax-literals null 0))

(define (make-header mpis syntax-literals)
  (header mpis
          (make-hasheq)        ; binding-sym-to-define-sym
          null                 ; binding-syms-in-order
          (make-variable-uses) ; require-var-to-import-sym
          (make-hasheq)        ; import-sym-to-extra-inspectors
          null                 ; require-vars-in-order
          (make-hasheq)        ; define-and-import-syms
          syntax-literals))

(define (make-variable-uses)
  (make-hash))

(define (add-syntax-literal! header-or-literals q)
  (define sl (if (header? header-or-literals)
                 (header-syntax-literals header-or-literals)
                 header-or-literals))
  (define pos (syntax-literals-count sl))
  (set-syntax-literals-count! sl (add1 pos))
  (set-syntax-literals-stxes! sl (cons q (syntax-literals-stxes sl)))
  pos)

;; Return a position in a larger vector where the given vector will
;; start; for convenience, pair that position with the size of the
;; vector
(define (add-syntax-literals! sl vec)
  (define pos (syntax-literals-count sl))
  (for ([e (in-vector vec)])
    (add-syntax-literal! sl e))
  (cons pos (vector-length vec)))

(define (syntax-literals-empty? sl)
  (null? (syntax-literals-stxes sl)))

;; Generate on-demand shifting (not shared among module instances)
;; using `deserialize-syntax-literal-data` (shared among module
;; instances); the result defines `syntax-literals-id` and
;; `get-syntax-literal!-id`
(define (generate-lazy-syntax-literals! sl mpis self
                                        #:skip-deserialize? [skip-deserialize? #f])
  `((define-values (,syntax-literals-id)
      (make-vector ,(syntax-literals-count sl) #f))
    (define-values (,get-syntax-literal!-id)
      (lambda (pos)
        (let-values ([(ready-stx) (unsafe-vector*-ref ,syntax-literals-id pos)])
          (if ready-stx
              ready-stx
              (begin
                ,@(if skip-deserialize?
                      null
                      `((if (unsafe-vector*-ref ,deserialized-syntax-vector-id 0)
                            (void)
                            (,deserialize-syntax-id ,bulk-binding-registry-id))))
                (let-values ([(stx)
                              (syntax-module-path-index-shift
                               (syntax-shift-phase-level
                                (unsafe-vector*-ref ,deserialized-syntax-vector-id pos)
                                ,phase-shift-id)
                               ,(add-module-path-index! mpis self)
                               ,self-id
                               ,inspector-id)])
                  (begin
                    (vector-cas! ,syntax-literals-id pos #f stx)
                    (unsafe-vector*-ref ,syntax-literals-id pos))))))))))

;; Generate on-demand deserialization (shared across instances); the
;; result defines `deserialize-syntax-id`
(define (generate-lazy-syntax-literals-data! sl mpis)
  (cond
   [(syntax-literals-empty? sl)
    `((define-values (,deserialize-syntax-id) #f))]
   [else
    `((define-values (,deserialize-syntax-id)
        ;; Put deserialization under a `lambda` so that it's loaded
        ;; from bytecode on demand, and in a function that can be
        ;; discarded via `set!` after deserialization. Since this
        ;; deserialized form is shared via the module cache across
        ;; module instances and even module declarations, it must not
        ;; depend on anything namespace-, declaration-, or
        ;; instance-specific. As an exception, however, a bulk-binding
        ;; registry can be namespace- or declaration-specific
        ;; declaration on the grounds that all declarations should
        ;; provide the same information for bulk bindings.
        (lambda (,bulk-binding-registry-id)
          (begin
            (vector-copy!
             ,deserialized-syntax-vector-id
             '0
             (let-values ([(,inspector-id) #f])
               ,(generate-deserialize (vector->immutable-vector
                                       (list->vector
                                        (reverse (syntax-literals-stxes sl))))
                                      mpis)))
            (set! ,deserialize-syntax-id #f)))))]))

(define (generate-lazy-syntax-literal-lookup pos)
  `(,get-syntax-literal!-id ,pos))

;; Generate immediate deserialization and shifting of a set of syntax
;; objects across multiple phases; the result is an expression for a
;; vector (indexed by syntax-literal position).
(define (generate-eager-syntax-literals! sl mpis base-phase self ns)
  (cond
   [(syntax-literals-empty? sl)
    ;; Avoid serializing unneeded namespace scope:
    #f]
   [else
    `(let-values ([(ns+stxss) ,(generate-deserialize (cons
                                                      ;; Prefix with namespace scope:
                                                      (encode-namespace-scopes ns)
                                                      (reverse
                                                       (syntax-literals-stxes sl)))
                                                     mpis)])
      (let-values ([(ns-scope-s) (car ns+stxss)])
        (list->vector
         (map (lambda (stx)
                (swap-top-level-scopes
                 (syntax-module-path-index-shift
                  (syntax-shift-phase-level
                   stx
                   (- ,base-phase ,dest-phase-id))
                  ,(add-module-path-index! mpis self)
                  ,self-id)
                 ns-scope-s ,ns-id))
              (cdr ns+stxss)))))]))

(define (generate-eager-syntax-literal-lookup pos)
  `(unsafe-vector*-ref ,syntax-literals-id ,pos))

;; Genereate a vector for a set of syntax objects; the result is a
;; vector like the one generated in expression from by
;; `generate-eager-syntax-literals!`, where no shifts are needed
(define (syntax-literals-as-vector sl)
  (list->vector
   (reverse (syntax-literals-stxes sl))))

(define (header-empty-syntax-literals? h)
  (syntax-literals-empty? (header-syntax-literals h)))

;; ----------------------------------------

;; Pick a symbol to represent a local binding, given the binding's key
(define (local-key->symbol key)
  ;; A local-binding key is already an distinct uninterned symbol
  ;; (with a deterministic label)
  key)

;; Select a symbol not yet used in the header or as a built-in name
(define (select-fresh sym header)
  (if (symbol-conflicts? sym header)
      (let loop ([pos 1])
        (define new-sym (string->symbol (string-append (number->string pos)
                                                       "/"
                                                       (symbol->string sym))))
        (if (symbol-conflicts? new-sym header)
            (loop (add1 pos))
            new-sym))
      sym))

(define (symbol-conflicts? sym header)
  (or (built-in-symbol? sym)
      (hash-ref (header-define-and-import-syms header) sym #f)))

;; ----------------------------------------

(define (register-required-variable-use! header mpi phase sym extra-inspector
                                         #:defined? [defined? #f])
  (define key (variable-use (module-use mpi phase) sym))
  (define variable-uses (header-require-var-to-import-sym header))
  (define prev-var-sym (hash-ref variable-uses key #f))
  (define var-sym
    (or prev-var-sym
        (let ([sym (select-fresh (variable-use-sym key) header)])
          (hash-set! variable-uses key sym)
          (set-header-require-vars-in-order! header
                                             (cons key
                                                   (header-require-vars-in-order header)))
          (hash-set! (header-define-and-import-syms header) sym (if defined? 'defined 'required))
          sym)))
  (when (and extra-inspector
             ;; Only track extra inspectors if all references have an inspector;
             ;; otherwise, the one without an extra inspector has the least access
             (not prev-var-sym))
    (define extra-inspectors (header-import-sym-to-extra-inspectors header))
    (hash-update! extra-inspectors var-sym (lambda (s) (set-add s extra-inspector)) #hasheq()))
  var-sym)

(define (register-as-defined! header def-sym)
  (hash-set! (header-define-and-import-syms header) def-sym 'defined))

(define (registered-as-required? header var-sym)
  (eq? 'required (hash-ref  (header-define-and-import-syms header) var-sym #f)))

;; Returns:
;;  link-names : a list of sym
;;  link-requires : a list of module path indexes
;;  imports : a list of S-expressions for imports; refers to `link-names`
;;  extra-inspectorsss : a list of hash of symbol to (or/c #f (set/c inspector?))
;;  def-decls : a list of S-expressions for forward-reference declarations
(define (generate-links+imports header phase cctx cross-linklet-inlining?)
  ;; Find each distinct module+phase, where `link-mod-uses` is in a
  ;; determinsitic order
  (define-values (mod-use-ht link-mod-uses)
    (for/fold ([ht #hash()] [link-mod-uses null]) ([(vu) (in-list (header-require-vars-in-order header))])
      (define mu (variable-use-module-use vu))
      (if (or (hash-ref ht mu #f)
              (eq? (module-use-module mu)
                   (compile-context-self cctx))
              (top-level-module-path-index? (module-use-module mu)))
          (values ht link-mod-uses)
          (values (hash-set ht mu #t)
                  (cons mu link-mod-uses)))))

  (values
   ;; Module-uses list:
   link-mod-uses
   ;; Imports, using the same order as module-uses list:
   (for/list ([mu (in-list link-mod-uses)])
     (for/list ([vu (in-list (header-require-vars-in-order header))]
                #:when (equal? mu (variable-use-module-use vu)))
       (define var-sym (hash-ref (header-require-var-to-import-sym header) vu))
       (define ex-sym (variable-use-sym vu))
       (if (eq? var-sym ex-sym)
           var-sym
           `[,ex-sym ,var-sym])))
   ;; Extra inspectorsss, in parallel to imports
   (for/list ([mu (in-list link-mod-uses)])
     (define extra-inspectorss
       (for*/hash ([vu (in-list (header-require-vars-in-order header))]
                   #:when (equal? mu (variable-use-module-use vu))
                   [var-sym (in-value (hash-ref (header-require-var-to-import-sym header) vu))]
                   [extra-inspectors (in-value (hash-ref (header-import-sym-to-extra-inspectors header) var-sym #f))]
                   #:when (or extra-inspectors
                              ;; For inlining purposes, keep track of all referenced,
                              ;; since formerly unreferenced will mean inlined
                              cross-linklet-inlining?))
         (values var-sym extra-inspectors)))
     (and (hash-count extra-inspectorss)
          extra-inspectorss))
   ;; Declarations (for non-module contexts)
   (for/list ([vu (in-list (header-require-vars-in-order header))]
              #:when (let ([mod (module-use-module (variable-use-module-use vu))])
                       (or (eq? mod (compile-context-self cctx))
                           (top-level-module-path-index? mod))))
     (define var-sym (hash-ref (header-require-var-to-import-sym header) vu))
     (define ex-sym (variable-use-sym vu))
     (if (eq? var-sym ex-sym)
         var-sym
         `(,var-sym ,ex-sym)))))

;; Get a reasonably nice name from a module-path-index
(define (extract-name mpi)
  (define-values (p base) (module-path-index-split mpi))
  (cond
   [(symbol? p) p]
   [(path? p) (let-values ([(base name dir?) (split-path p)])
                (path-replace-extension name #""))]
   [(string? p) (path-replace-extension p #"")]
   [(and (pair? p) (eq? (car p) 'quote))
    (cadr p)]
   [(and (pair? p) (eq? (car p) 'file))
    (let-values ([(base name dir?) (split-path (cadr p))])
      (path-replace-extension name #""))]
   [(and (pair? p) (eq? (car p) 'lib))
    (path-replace-extension (cadr p) #"")]
   [else 'module]))

