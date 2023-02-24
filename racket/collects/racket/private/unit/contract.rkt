#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/name
                     syntax/parse/pre
                     "exptime/import-export.rkt"
                     "exptime/signature.rkt"
                     "contract-syntax.rkt")
         racket/contract/base
         racket/contract/combinator
         racket/contract/region
         racket/unsafe/undefined
         "keywords.rkt"
         "runtime.rkt"
         "unit-core.rkt"
         "util.rkt")

(provide (for-syntax unit/c/core) unit/c)

(begin-for-syntax
  (define (build-contracted-defs sig-ids tag-syms binding-idss ctcss
                                 #:import? import?
                                 #:blame-expr blame-expr
                                 #:indy-party-expr indy-party-expr
                                 #:uncontracted-intro uncontracted-intro
                                 #:contracted-intro contracted-intro
                                 #:indy-intro indy-intro)
    (for/list ([sig-id (in-list sig-ids)]
               [tag-sym (in-list tag-syms)]
               [binding-ids (in-list binding-idss)]
               [ctcs (in-list ctcss)]
               #:unless (null? binding-ids))
      (define sig-blame-id (generate-temporary sig-id))
      #`(begin
          (define #,sig-blame-id (add-unit/c-signature-context #,blame-expr '#,sig-id '#,tag-sym '#,import?))
          #,@(for/list ([binding-id (in-list binding-ids)]
                        [ctc (in-list ctcs)])
               (cond
                 [import?
                  (define val-thunk-id (generate-temporary binding-id))
                  (define indy-thunk-id (generate-temporary binding-id))
                  #`(begin
                      (define-values [#,val-thunk-id #,indy-thunk-id]
                        (build-unit/c-import-wrappers '#,binding-id
                                                      (λ () (values #,(uncontracted-intro binding-id)
                                                                    #,(indy-intro ctc)))
                                                      #,sig-blame-id
                                                      #,indy-party-expr))
                      (define-syntaxes [#,(contracted-intro binding-id)
                                        #,(indy-intro binding-id)]
                        (make-unit/c-import-bindings (quote-syntax #,val-thunk-id)
                                                     (quote-syntax #,indy-thunk-id))))]
                 [else
                  #`(define-values [#,(contracted-intro binding-id)
                                    #,(indy-intro binding-id)]
                      (wrap-unit/c-export '#,binding-id
                                          #,(uncontracted-intro binding-id)
                                          #,(indy-intro ctc)
                                          #,sig-blame-id
                                          #,indy-party-expr))])))))

  (define (unit/c/core name stx)
    (syntax-parse stx
      #:context (add-inferred-name (current-syntax-context) name)
      [(:import-clause/c
        :export-clause/c
        d:opt-init-depends
        {~optional b:body-clause/c})
       
       (define (process-sig sig-stx sig-var-ids xs cs)
         (let ([dup (check-duplicate-identifier xs)])
           (when dup
             (raise-stx-err (format "duplicate identifier found for signature ~a"
                                    (syntax->datum sig-stx))
                            dup)))
         (for ([x (in-list xs)]
               [c (in-list cs)])
           (unless (memf (λ (i) (bound-identifier=? x i)) sig-var-ids)
             (raise-stx-err (format "identifier not member of signature ~a" 
                                    (syntax-e sig-stx))
                            x))))

       (check-duplicate-sigs (map cons (attribute i.s.tag-sym) (attribute i.s.info))
                             (attribute i.s)
                             (map cons (attribute d.tag-sym) (attribute d.info))
                             (attribute d.dep))
       (check-duplicate-subs (map cons (attribute e.s.tag-sym) (attribute e.s.info))
                             (attribute e.s))
       
       (for-each process-sig
                 (attribute i.s)
                 (attribute i.s.var.int-id)
                 (attribute i.x)
                 (attribute i.c))
       (for-each process-sig
                 (attribute e.s)
                 (attribute e.s.var.int-id)
                 (attribute e.x)
                 (attribute e.c))

       (define/syntax-parse [imports/exports/deps ...]
         #'[(vector-immutable (cons 'i.s.info.self-id (vector-immutable i.s.key ...)) ...)
            (vector-immutable (cons 'e.s.info.self-id (vector-immutable e.s.key ...)) ...)
            (list d.self-key ...)])

       (define uncontracted-intro (make-syntax-introducer #t))
       (define contracted-intro (make-syntax-introducer #t))
       (define indy-intro (make-syntax-introducer #t))

       ;; Builds import/export specs that rename the given ids using the
       ;; given syntax introducer.
       (define (build-renamed-specs orig-specs idss intro)
         (for/list ([orig-spec (in-list orig-specs)]
                    [ids (in-list idss)])
           (rename-ie-spec orig-spec (for/list ([id (in-list ids)])
                                       (cons (intro id) id)))))

       (define-values [unit-expr x y z]
         (build-unit
          #:contract-region? #f ; contract violations in the body should blame the indy party
          #:suppress-generated-exports? #t ; see Note [Suppress generated exports on reexport] in "unit-core.rkt"
          #`[(import #,@(build-renamed-specs (attribute i.s) (attribute i.x) uncontracted-intro))
             (export #,@(build-renamed-specs (attribute e.s) (attribute e.x) contracted-intro))
             {~? {~@ . d}}

             #,@(build-contracted-defs
                 (attribute i.s.sig-id)
                 (attribute i.s.tag-sym)
                 (attribute i.x)
                 (attribute i.c)
                 #:import? #t
                 #:blame-expr #'import-blame
                 #:indy-party-expr #'indy-party
                 #:uncontracted-intro uncontracted-intro
                 #:contracted-intro contracted-intro
                 #:indy-intro indy-intro)

             (define-values/invoke-unit unit-tmp
               (import #,@(build-renamed-specs (attribute i.s) (attribute i.x) contracted-intro))
               (export #,@(build-renamed-specs (attribute e.s) (attribute e.x) uncontracted-intro))
               (values . results))

             #,@(build-contracted-defs
                 (attribute e.s.sig-id)
                 (attribute e.s.tag-sym)
                 (attribute e.x)
                 (attribute e.c)
                 #:import? #f
                 #:blame-expr #'export-blame
                 #:indy-party-expr #'indy-party
                 #:uncontracted-intro uncontracted-intro
                 #:contracted-intro contracted-intro
                 #:indy-intro indy-intro)

             #,(if (attribute b)
                   #`(unit/c-check-invoke-values
                      body-blame
                      (list #,@(map indy-intro (attribute b.ctc)))
                      results)
                   #'(apply values results))]))
       
       (quasisyntax/loc stx
         (let ([indy-party (current-contract-region)])
           (make-contract
            #:name
            (list 'unit/c
                  (cons 'import 
                        (list (cons 'i.s
                                    (map list (list 'i.x ...) 
                                         (build-compound-type-name 'i.c ...)))
                              ...))
                  (cons 'export 
                        (list (cons 'e.s
                                    (map list (list 'e.x ...)
                                         (build-compound-type-name 'e.c ...)))
                              ...))
                  (cons 'init-depend
                        (list 'd.dep ...))
                  {~? b.name})
            #:projection
            (λ (blame)
              (define export-blame blame)
              (define import-blame (blame-swap blame))
              #,@(if (attribute b)
                     (list #'(define body-blame (add-unit/c-body-context blame)))
                     '())
              (λ (unit-tmp)
                (unit/c-first-order-check 
                 unit-tmp
                 imports/exports/deps ...
                 blame)
                #,unit-expr))
            #:first-order
            (λ (v)
              (unit/c-first-order-check 
               v
               imports/exports/deps ...
               #f)))))])))

(define-syntax/err-param (unit/c stx)
  (syntax-case stx ()
    [(_ . sstx)
     (let ([name (syntax-local-infer-name stx)])
       (unit/c/core name #'sstx))]))

(define (unit/c-first-order-check val expected-imports expected-exports expected-deps blame)
  (let/ec return
    (define (failed str . args)
      (if blame
          (apply raise-blame-error blame val str args)
          (return #f)))
    (define (check-sig-subset sub-sig super-sig import?)
      (define t (make-hash))
      (let loop ([i (sub1 (vector-length sub-sig))])
        (when (>= i 0)
          (let ([v (cdr (vector-ref sub-sig i))])
            (let loop ([j (sub1 (vector-length v))])
              (when (>= j 0)
                (let ([vj (vector-ref v j)])
                  (hash-set! t vj
                             (if (hash-ref t vj #f)
                                 'amb
                                 #t)))
                (loop (sub1 j)))))
          (loop (sub1 i))))
      (let loop ([i (sub1 (vector-length super-sig))])
        (when (>= i 0)
          (let* ([v0 (vector-ref (cdr (vector-ref super-sig i)) 0)]
                 [r (hash-ref t v0 #f)])
            (when (not r)
              (let ([sub-name (car (vector-ref super-sig i))])
                (define tag-part (vector-ref (cdr (vector-ref super-sig i)) 0))
                (define tag (and (pair? tag-part) (car tag-part)))
                (failed
                 (string-append
                  (if import?
                      (format "contract does not list import ~a" sub-name)
                      (format "unit must export signature ~a" sub-name))
                  (if tag
                      (format " with tag ~a" tag)
                      ""))))))
          (loop (sub1 i)))))
    ;; check that the dependencies of the given unit are consistent with the
    ;; dependencies specified by the contract. Ensures that the given dependencies
    ;; are a subset of the expected dependencies otherwise raises a contract error.
    (define (check-dependencies expected given imports)
      (define t (for*/hash ([i (in-vector imports)]
                            [v (in-value (cdr i))]
                            [im (in-value (vector-ref v 0))]
                            #:when (member im expected)
                            [vj (in-vector v)])
                  (values vj #t)))
      ;; use the imports to get the name and tag of dependency
      (define (get-name dep-tag)
        (define tag-table
          (for/hash ([e (in-vector imports)])
            (define name (car e))
            (define v (vector-ref (cdr e) 0))
            (define tag (if (pair? v) (cdr v) v))
            (values tag name)))
        (hash-ref tag-table dep-tag #f))
     (for ([dep (in-list given)])
       (unless (hash-ref t dep #f)
         (define tag (and (pair? dep) (car dep)))
         (define sig-tag (or (and (pair? dep) (cdr dep)) dep))
         (failed
               (string-append
                (format "contract does not list initialization dependency ~a"
                        (get-name sig-tag))
                (if tag
                    (format " with tag ~a" tag)
                    ""))))))
    (unless (unit? val)
      (failed "not a unit"))
    (check-sig-subset expected-imports (unit-import-sigs val) #t)
    (check-sig-subset (unit-export-sigs val) expected-exports #f)
    (check-dependencies expected-deps (unit-deps val) expected-imports)
    #t))

(define (add-unit/c-signature-context blame name tag import?)
  (blame-add-context blame (format "signature ~a ~a~a by"
                                   name
                                   (if import? "imported" "exported")
                                   (if tag (format " with tag ~a" tag) ""))))

(define (add-unit/c-member-context blame name)
  (blame-add-context blame
                     (format "the ~a member of" name)
                     #:important (symbol->string name)))

(define (add-unit/c-body-context blame)
  (blame-add-context blame "the body of"))

(define (unit/c-check-invoke-values blame ctcs args)
  (define len (length ctcs))
  (define args-len (length args))
  (unless (= len args-len)
    (raise-blame-error blame
                       (blame-value blame)
                       (format "expected ~a values, returned ~a" len args-len)))
  (apply values 
         (map 
          (lambda (ctc arg) (((contract-projection ctc) blame) arg))
          ctcs args)))

;; build-unit/c-import-wrappers : symbol?
;;                                (-> (values any/c contract?))
;;                                blame?
;;                                any/c
;;                             -> (values (-> any/c) (-> any/c))
;;
;; Returns a pair of thunks that lazily apply the standard and indy
;; contract projections for an imported value. The laziness is important,
;; as the imports may not yet be initialized; see Note [Preserve import laziness]
;; in "runtime.rkt" for the details.
(define (build-unit/c-import-wrappers member-name get-val+ctc sig-blame indy-party)
  (define (force!)
    (define-values [val ctc] (get-val+ctc))
    (with-contract-continuation-mark sig-blame
      (define proj (contract-projection (coerce-contract 'unit/c ctc)))

      (define blame (add-unit/c-member-context sig-blame member-name))
      (define wrapped-val ((proj blame) val))
      (set-box! thunk-box (λ () wrapped-val))

      (define indy-blame (blame-replace-negative blame indy-party))
      (define indy-val ((proj indy-blame) val))
      (set-box! indy-thunk-box (λ () indy-val))))

  (define (make-forcer-box)
    (define b
      (box (λ ()
             ;; If evaluation of the value or contract somehow depends on the
             ;; contracted value, raise a use-before-initialization error to
             ;; avoid infinitely looping.
             (set-box! b (λ () (check-not-unsafe-undefined unsafe-undefined member-name)))
             (force!)
             ((unbox b)))))
    b)

  (define thunk-box (make-forcer-box))
  (define indy-thunk-box (make-forcer-box))

  (values (λ () ((unbox thunk-box)))
          (λ () ((unbox indy-thunk-box)))))

(define (wrap-unit/c-export member-name val ctc sig-blame indy-party)
  (define proj (contract-projection (coerce-contract 'unit/c ctc)))
  (define blame (add-unit/c-member-context sig-blame member-name))
  (define indy-blame (blame-replace-negative blame indy-party))
  (values ((proj blame) val)
          ((proj indy-blame) val)))

(begin-for-syntax
  ;; To avoid evaluating imports too early, it’s important that we make
  ;; the wrapped imported bindings look like a unit import, so other forms
  ;; know it’s safe to defer evaluation. See Note [Preserve import laziness]
  ;; in "runtime.rkt" for details.
  (define (make-unit/c-import-binding thunk-id)
    (make-import-binding
     (λ (stx) (quasisyntax/loc stx (#,thunk-id)))))
  (define (make-unit/c-import-bindings thunk-id indy-thunk-id)
    (values (make-unit/c-import-binding thunk-id)
            (make-unit/c-import-binding indy-thunk-id))))
