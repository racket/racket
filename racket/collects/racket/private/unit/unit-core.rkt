#lang racket/base

;; This module implements the core unit forms, without support for inferred linking.

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/context
                     syntax/id-table
                     syntax/intdef
                     syntax/kerncase
                     syntax/name
                     syntax/stx
                     "exptime/import-export.rkt"
                     "exptime/signature.rkt"
                     "exptime/syntax.rkt")
         racket/contract/base
         racket/contract/region
         racket/list
         racket/stxparam
         racket/unsafe/undefined
         syntax/location
         syntax/parse/define
         "keywords.rkt"
         "runtime.rkt"
         "util.rkt")

(provide (for-syntax build-unit
                     build-unit-from-context
                     build-unit/new-import-export
                     build-compound-unit)
         unit
         unit-from-context
         unit/new-import-export
         compound-unit
         invoke-unit/core
         invoke-unit
         define-values/invoke-unit/core
         define-values/invoke-unit)

;; -----------------------------------------------------------------------------
;; `unit`

(begin-for-syntax
  (define (localify exp def-ctx)
    (internal-definition-context-introduce def-ctx exp 'add))

  (define (make-import-make-unboxing var renamings loc ctc)
    (if ctc
        (with-syntax ([ctc-stx (syntax-property ctc 'inferred-name var)])
          (quasisyntax/loc (current-syntax-context)
            (lambda (stx)
              (with-syntax ([app (datum->syntax (quote-syntax here)
                                                (list (quote-syntax #,loc))
                                                stx)])
                (syntax (let ([v/c app])
                          (if (pair? v/c)
                              (contract (let-syntax #,renamings ctc-stx) (car v/c) (cdr v/c)
                                        (current-contract-region)
                                        (quote #,var) (quote-srcloc #,var))
                              (error 'unit "contracted import ~a used before definition"
                                     (quote #,(syntax->datum var))))))))))
        (quasisyntax/loc (current-syntax-context)
          (lambda (stx)
            (datum->syntax (quote-syntax here)
                           (list (quote-syntax #,loc))
                           stx)))))

  (define (make-id-mappers . make-unbox-stxes)
    (apply values (map make-id-mapper make-unbox-stxes)))

  (define (make-id-mapper make-unbox-stx)
    (make-set!-transformer
     (lambda (sstx)
       (syntax-case sstx (set!)
         [x
          (identifier? #'x) 
          (make-unbox-stx sstx)]
         [(set! . x)
          (raise-syntax-error
           'unit
           "cannot set! imported or exported variables"
           sstx)]
         [(_ . x)
          (datum->syntax
           sstx
           (cons (make-unbox-stx sstx) #'x)
           sstx)]))))

  ;; build-unit : syntax-object -> 
  ;;             (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a unit expression.  stx must be
  ;; such that it passes check-unit-syntax.
  ;; The two additional values are the identifiers of the unit's import and export
  ;; signatures
  (define (build-unit stx)
    (syntax-parse stx
      #:context (current-syntax-context)
      #:literals [import export init-depend]
      [[(import in:tagged-import-spec ...)
        (export out:tagged-export-spec ...)
        deps:opt-init-depends
        . body]

       (check-duplicate-sigs (map cons (attribute in.tag-sym) (attribute in.info))
                             (attribute in)
                             (map cons (attribute deps.tag-sym) (attribute deps.info))
                             (attribute deps.dep))
       (check-duplicate-subs (map cons (attribute out.tag-sym) (attribute out.info))
                             (attribute out))
       (check-unit-ie-sigs (attribute in.value) (attribute out.value))

       ;; INTRODUCED FORMS AND MACROS:
       ;; We need to distinguish the original body from any
       ;; forms that are introduced from signatures 
       ;; (via `define-values`, etc., in a signature body).
       ;; The `intro` mark should be added to everything except
       ;; the introduced parts, which we implement by adding the
       ;; mark to the introduced parts and then flipping it
       ;; everywhere.
       (define intro (make-syntax-introducer #t))

       (define/syntax-parse name (syntax-local-infer-name (current-syntax-context)))
       (define/syntax-parse ([in-loc ...] ...) (map generate-temporaries (attribute in.var.int-id)))
       (define/syntax-parse ([out-loc ...] ...) (map generate-temporaries (attribute out.var.int-id)))
       (define/syntax-parse [in-count ...] (for/list ([vars (in-list (attribute in.var.int-id))])
                                             (length vars)))

       (define/syntax-parse [in-var-mapper-bind ...]
         (for/list ([int-vars (in-list (attribute in.var.int-id))]
                    [ext-vars (in-list (attribute in.var.ext-id))]
                    [ctcs (in-list (attribute in.ctc))]
                    [locs (in-list (attribute in-loc))])
           (define renamings (for/list ([ext-var (in-list ext-vars)]
                                        [int-var (in-list int-vars)])
                               #`[#,ext-var (make-rename-transformer (quote-syntax #,int-var))]))
           #`[#,int-vars
              (make-id-mappers
               #,@(for/list ([int-var (in-list int-vars)]
                             [ctc (in-list ctcs)]
                             [loc (in-list locs)])
                    (make-import-make-unboxing int-var renamings loc ctc)))]))

       (define/syntax-parse ([rename-bind [stx-bind ...] [val-bind ...]] ...)
         (map (build-val+macro-defs intro) (attribute in.value)))
       (define/syntax-parse ([[post-rhs ...] [ctc ...]] ...)
         (map build-post-val-defs+ctcs (attribute out.value)))

       (define unit-expr
         (quasisyntax/loc (current-syntax-context)
           (make-unit
            'name
            (vector-immutable (cons 'in.info.self-id (vector-immutable in.key ...)) ...)
            (vector-immutable (cons 'out.info.self-id (vector-immutable out.key ...)) ...)
            (list deps.self-key ...)
            (syntax-parameterize ([current-contract-region (lambda (stx) #'(quote (unit name)))])
              (lambda ()
                (let ([out-loc (box unsafe-undefined)] ... ...)
                  (values
                   #,(quasisyntax/loc (current-syntax-context)
                       (lambda (import-table)
                         (let-values ([(in-loc ...) (vector->values (hash-ref import-table in.self-key) 0 'in-count)] ...)
                           (letrec-syntaxes (in-var-mapper-bind ...)
                             (letrec-syntaxes+values (rename-bind ... stx-bind ... ...)
                               (val-bind ... ...)
                               (unit-body #,(current-syntax-context)
                                          [in.var.int-id ... ...]
                                          [out.var.int-id ... ...]
                                          [out-loc ... ...]
                                          [ctc ... ...]
                                          (begin . body)
                                          (define-values [out.post-def.id ...] post-rhs) ... ...))))))
                   (unit-export ([out.key ...]
                                 (vector-immutable (λ () (check-not-unsafe-undefined (unbox out-loc) 'out.var.ext-id))
                                                   ...))
                                ...))))))))

       (values (syntax-parse-track-literals (syntax-protect (intro unit-expr)))
               (map cons (attribute in.tag-sym) (attribute in.sig-id))
               (map cons (attribute out.tag-sym) (attribute out.sig-id))
               (map cons (attribute deps.tag-sym) (attribute deps.sig-id)))])))

(begin-for-syntax
  ;; (make-var-info bool bool identifier (U #f syntax-object))
  (define-struct var-info (syntax? [exported? #:mutable] id [ctc #:mutable])))

(define-syntax (unit-body stx)
  (syntax-case stx ()
    ((_ err-stx ivars evars elocs ectcs body ...)
     (parameterize ((current-syntax-context #'err-stx))
       (let* ([expand-context (generate-expand-context)]
              [def-ctx (syntax-local-make-definition-context)]
              [stop-list
               (append
                (kernel-form-identifier-list)
                (syntax->list #'ivars))]
              [definition?
                (lambda (id)
                  (and (identifier? id)
                       (or (free-identifier=? id (quote-syntax define-values))
                           (free-identifier=? id (quote-syntax define-syntaxes)))))]
              [expanded-body
               (let expand-all ((defns&exprs (syntax->list #'(body ...))))
                 ;; Expand the body enough
                 (apply
                  append
                  (map
                   (lambda (defn-or-expr)
                     (let ([defn-or-expr
                             (local-expand
                              defn-or-expr
                              expand-context
                              stop-list
                              def-ctx)])
                       (define (track e)
                         (syntax-case defn-or-expr ()
                           [(id . _) (syntax-track-origin e defn-or-expr #'id)]))
                       (syntax-case defn-or-expr (begin define-values define-syntaxes)
                         [(begin . l)
                          (let ([l (parameterize ((current-syntax-context defn-or-expr))
                                     (checked-syntax->list #'l))])
                            (expand-all (map track l)))]
                         [(define-syntaxes (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (with-syntax ([rhs (local-transformer-expand
                                              #'rhs
                                              'expression
                                              null)])
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
                            (with-syntax ([(id ...) (map (lambda (id) (syntax-local-identifier-as-binding id def-ctx))
                                                    (syntax->list #'(id ...)))])
                              (list (track #'(define-syntaxes (id ...) rhs)))))]
                         [(define-values (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (begin
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f def-ctx)
                            (with-syntax ([(id ...) (map (lambda (id) (syntax-local-identifier-as-binding id def-ctx))
                                                         (syntax->list #'(id ...)))])
                              (list (track #'(define-values (id ...) rhs)))))]
                         [else (list defn-or-expr)])))
                   defns&exprs)))]
              [ends-in-defn?
               (syntax-case expanded-body ()
                 [(_ ... (x . _))
                  (and (identifier? #'x)
                       (member #'x
                               (list #'define-values #'define-syntaxes #'define-syntax)
                               free-identifier=?))]
                 [_ #f])]
              ;; Get all the defined names, sorting out variable definitions
              ;; from syntax definitions.
              [defined-names-table
                (let ((table (make-bound-id-table)))
                  (for-each
                   (lambda (defn-or-expr)
                     (syntax-case defn-or-expr ()
                       ((dv . rest)
                        (definition? #'dv)
                        (begin
                          (check-def-syntax defn-or-expr)
                          (syntax-case #'rest ()
                            [((id ...) expr)
                             (for-each 
                              (lambda (id)
                                (when (bound-id-table-ref table id #f)
                                  (raise-stx-err "variable defined twice" id))
                                (bound-id-table-set!
                                 table id 
                                 (make-var-info (free-identifier=? #'dv (quote-syntax define-syntaxes))
                                                #f
                                                id
                                                #f)))
                              (syntax->list #'(id ...)))]
                            [_ (void)])))
                       [_ (void)]))
                   expanded-body)
                  table)])
         
         ;; Mark exported names and
         ;; check that all exported names are defined (as var):
         (for-each
          (lambda (name loc ctc)
            (let ([v (bound-id-table-ref defined-names-table name #f)])
              (unless v
                (raise-stx-err (format "undefined export ~a" (syntax-e name))))
              (when (var-info-syntax? v)
                (raise-stx-err "cannot export syntax from a unit" name))
              (set-var-info-exported?! v loc)
              (when (pair? (syntax-e ctc))
                (set-var-info-ctc! v (localify (cdr (syntax-e ctc)) def-ctx)))))
          (syntax->list (localify #'evars def-ctx))
          (syntax->list #'elocs)
          (syntax->list #'ectcs))
         
         ;; Check that none of the imports are defined
         (for-each
          (lambda (i)
            (let ((defid (bound-id-table-ref defined-names-table i #f)))
              (when defid
                (raise-stx-err
                 "definition for imported identifier"
                 (var-info-id defid)))))
          (syntax->list (localify #'ivars def-ctx)))

         ;; Handles redirection of exported definitions and collects
         ;; positive-blaming `contract` expressions
         (define (process-defn-or-expr defn-or-expr)
           (syntax-case defn-or-expr (define-values)
             [(define-values (id ...) body)
              (let* ([ids (syntax->list #'(id ...))]
                     [tmps (generate-temporaries ids)]
                     [do-one
                      (λ (id tmp)
                        (let ([var-info (bound-id-table-ref defined-names-table id)])
                          (cond
                            [(var-info-exported? var-info)
                             =>
                             (λ (export-loc)
                               (let ([ctc (var-info-ctc var-info)])
                                 (values
                                  #`(begin
                                      #,(quasisyntax/loc defn-or-expr
                                          (set-box! #,export-loc
                                                    #,(if ctc
                                                          #`(cons #,tmp (current-contract-region))
                                                          tmp)))
                                      #,(quasisyntax/loc defn-or-expr
                                          (define-syntax #,id
                                            (make-id-mapper (lambda (stx) (quote-syntax #,tmp))))))
                                  (and ctc
                                       #`(contract #,ctc #,tmp
                                                   (current-contract-region)
                                                   'cant-happen
                                                   (quote #,id)
                                                   (quote-srcloc #,id))))))]
                            [else (values (quasisyntax/loc defn-or-expr
                                            (define-syntax #,id
                                              (make-rename-transformer (quote-syntax #,tmp))))
                                          #f)])))])
                (define-values (defns-and-exprs ctc-exprs)
                  (for/lists [defns-and-exprs ctc-exprs]
                             ([id (in-list ids)]
                              [tmp (in-list tmps)])
                    (do-one id tmp)))
                (list (cons (syntax-track-origin
                             (quasisyntax/loc defn-or-expr
                               (define-values #,tmps
                                 #,(if (and (pair? ids) (null? (cdr ids)))
                                       (syntax-property #'body 'inferred-name (car ids))
                                       #'body)))
                             defn-or-expr
                             (syntax-case defn-or-expr () [(d-v . _) #'d-v]))
                            defns-and-exprs)
                      (filter values ctc-exprs)))]
             [else (list (list defn-or-expr) '())]))

         (internal-definition-context-track
          def-ctx
          (if (null? expanded-body)
              #'(void)
              (with-syntax ([([(defn-or-expr ...) (ctc-expr ...)] ...)
                             (map process-defn-or-expr expanded-body)])
                (if ends-in-defn?
                    #'(let ()
                        defn-or-expr ... ...
                        ctc-expr ... ...
                        (void))
                    (with-syntax ([(defn-or-expr ... last-defn-or-expr) #'(defn-or-expr ... ...)])
                      #'(let ()
                          defn-or-expr ...
                          (begin0
                            last-defn-or-expr
                            ctc-expr ... ...))))))))))))

(define-syntax-parser unit
  [(_ . body)
   (define-values [u x y z] (build-unit #'body))
   u])

;; -----------------------------------------------------------------------------
;; `unit-from-context`

;; build-unit-from-context : syntax-object -> 
;;                           (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a unit-from-context expression.  stx must be
;; such that it passes check-ufc-syntax.
;; The two additional values are the identifiers of the unit's import and export
;; signatures
(define-for-syntax (build-unit-from-context stx)
  (syntax-parse stx
    #:context (current-syntax-context)
    [(out:tagged-export-spec)
     ;; Remove any bindings that will be generated via post- definitions
     (define int+ext-ids
       (let ([ht (make-bound-id-table)])
         (for* ([post-ids (in-list (attribute out.post-def.id))]
                [post-id (in-list post-ids)])
           (bound-id-table-set! ht post-id #t))
         (for/list ([int-id (in-list (attribute out.var.int-id))]
                    [ext-id (in-list (attribute out.var.int-id))]
                    #:unless (bound-id-table-ref ht int-id #f))
           (cons int-id ext-id))))

     (define (push-rename-in spec renames)
       (syntax-case spec (bind-at tag)
         [(bind-at u spec)
          #`(bind-at u #,(push-rename-in #'spec renames))]
         [(tag t spec)
          #`(tag t #,(push-rename-in #'spec renames))]
         [_ #`(rename #,spec . #,renames)]))

     (define/syntax-parse ([int-id . ext-id] ...) int+ext-ids)
     (define/syntax-parse [def-name ...] (generate-temporaries (map car int+ext-ids)))
     (values
      (syntax-protect
       (quasisyntax/loc (current-syntax-context)
         (unit (import) (export #,(push-rename-in #'out #'((def-name int-id) ...)))
               (define def-name int-id)
               ...)))
      '()
      (list (cons (attribute out.tag-sym) (attribute out.sig-id)))
      '())]))

(define-syntax-parser unit-from-context
  [(_ . body)
   (define-values [u x y z] (build-unit-from-context #'body))
   u])

;; -----------------------------------------------------------------------------
;; `unit/new-import-export`

(begin-for-syntax
  (define (redirect-imports/exports table-stx sigs tags target-sigs target-tags #:import? import?)
    (define def-table (make-bound-id-table))
    (define ctc-table (make-bound-id-table))
    (define sig-table (make-bound-id-table))

    (for ([sig (in-list sigs)]
          [tag (in-list tags)])
      (for ([index (in-naturals)]
            [int/ext-name (in-list (signature-vars sig))]
            [ctc (in-list (signature-ctcs sig))])
        (define int-name (car int/ext-name))
        (define v #`(hash-ref #,table-stx #,(siginfo->key-expr (signature-siginfo sig) tag)))
        (bound-id-table-set! def-table
                             int-name
                             #`(check-not-unsafe-undefined (vector-ref #,v #,index)
                                                           '#,int-name))
        (bound-id-table-set! ctc-table int-name ctc)
        (bound-id-table-set! sig-table int-name sig)))

    (define/syntax-parse ([eloc ...] ...)
      (for/list ([target-sig (in-list target-sigs)])
        (for/list ([target-int/ext-name (in-list (signature-vars target-sig))]
                   [target-ctc (in-list (signature-ctcs target-sig))])
          (define var (car target-int/ext-name))
          (define vref (bound-id-table-ref
                        def-table
                        var
                        (lambda ()
                          (raise-stx-err
                           (format (if import?
                                       "identifier ~a is not present in new imports"
                                       "identifier ~a is not present in old exports")
                                   (syntax-e (car target-int/ext-name)))))))
          (define ctc (bound-id-table-ref ctc-table var))
          (define rename-bindings (get-member-bindings def-table
                                                       (bound-id-table-ref sig-table var)
                                                       #'(current-contract-region)
                                                       #t))
          (define/syntax-parse ctc-stx (if ctc (syntax-property
                                                #`(letrec-syntax #,rename-bindings #,ctc)
                                                'inferred-name var)
                                           ctc))
          (if target-ctc
              #`(λ ()
                  (cons #,(if ctc
                              #`(let ([old-v/c (#,vref)])
                                  (contract ctc-stx (car old-v/c) 
                                            (cdr old-v/c) (current-contract-region)
                                            (quote #,var) (quote-srcloc #,var)))
                              #`(#,vref))
                        (current-contract-region)))
              (if ctc
                  #`(λ ()
                      (let ([old-v/c (#,vref)])
                        (contract ctc-stx (car old-v/c) 
                                  (cdr old-v/c) (current-contract-region)
                                  (quote #,var) (quote-srcloc #,var))))
                  vref)))))

    (define/syntax-parse ([export-keys ...] ...)
      (for/list ([target-sig (in-list target-sigs)]
                 [target-tag (in-list target-tags)])
        (siginfo->key-exprs (signature-siginfo target-sig) target-tag)))

    #'(unit-export ((export-keys ...)
                    (vector-immutable eloc ...))
                   ...))

  ;; build-unit/new-import-export : syntax-object -> 
  ;;             (values syntax-object (listof identifier) (listof identifier))
  ;; constructs the code for a unit expression that changes the import and export signatures
  ;; of another.  stx must be such that it passes check-unit-syntax.
  ;; The two additional values are the identifiers of the unit's import and export
  ;; signatures
  (define (build-unit/new-import-export stx)
    (define/syntax-parse who (stx-car (current-syntax-context)))
    (syntax-parse stx
      #:context (current-syntax-context)
      #:literals [import export init-depend]
      [[(import in:tagged-import-spec ...)
        (export out:tagged-export-spec ...)
        deps:opt-init-depends
        {~describe "unit specification"
                   ([sub-out:tagged-import-spec ...]
                    sub-expr:expr
                    sub-in:tagged-export-spec ...)}]

       (check-duplicate-sigs (map cons (attribute in.tag-sym) (attribute in.info))
                             (attribute in)
                             (map cons (attribute deps.tag-sym) (attribute deps.info))
                             (attribute deps.dep))
       (check-duplicate-subs (map cons (attribute out.tag-sym) (attribute out.info))
                             (attribute out))
       (check-unit-ie-sigs (attribute in.value) (attribute out.value))

       (define/syntax-parse name (syntax-local-infer-name (current-syntax-context)))
       (define unit-expr
         (quasisyntax/loc (current-syntax-context)
           (let ([sub-tmp sub-expr])
             (check-unit sub-tmp 'who)
             (check-sigs
              sub-tmp
              (vector-immutable (cons 'sub-in.info.self-id (vector-immutable sub-in.key ...)) ...)
              (vector-immutable (cons 'sub-out.info.self-id (vector-immutable sub-out.key ...)) ...)
              'who)
             (make-unit
              'name
              (vector-immutable (cons 'in.info.self-id (vector-immutable in.key ...)) ...)
              (vector-immutable (cons 'out.info.self-id (vector-immutable out.key ...)) ...)
              (list deps.self-key ...)
              (syntax-parameterize ([current-contract-region (lambda (stx) #'(quote (unit name)))])
                (lambda ()
                  (let-values ([(sub-fn export-table) ((unit-go sub-tmp))])
                    (values (lambda (import-table)
                              (sub-fn #,(redirect-imports/exports
                                         #:import? #t
                                         #'import-table
                                         (attribute in.value)
                                         (attribute in.tag-sym)
                                         (attribute sub-in.value)
                                         (attribute sub-in.tag-sym))))
                            #,(redirect-imports/exports
                               #:import? #f
                               #'export-table
                               (attribute sub-out.value)
                               (attribute sub-out.tag-sym)
                               (attribute out.value)
                               (attribute out.tag-sym))))))))))

       (values (syntax-parse-track-literals (syntax-protect unit-expr))
               (map cons (attribute in.tag-sym) (attribute in.sig-id))
               (map cons (attribute out.tag-sym) (attribute out.sig-id))
               (map cons (attribute deps.tag-sym) (attribute deps.sig-id)))])))

(define-syntax-parser unit/new-import-export
  [(_ . body)
   (define-values [u x y z] (build-unit/new-import-export #'body))
   u])

;; -----------------------------------------------------------------------------
;; `compound-unit`

(begin-for-syntax
  (define-syntax-class tagged-link-id
    #:description "tagged link identifier"
    #:attributes [tag-id tag-sym link-id]
    #:commit
    #:literals [tag]
    (pattern (tag ~! tag-id:id link-id:id)
      #:attr tag-sym (syntax-e #'tag-id))
    (pattern link-id:id
      #:attr tag-id #f
      #:attr tag-sym #f))

  (define-syntax-class link-binding
    #:description "link binding"
    #:auto-nested-attributes
    #:commit
    #:datum-literals [:]
    (pattern [link-id:id : sig:tagged-signature-id]))

  (define-syntax-class linkage-decl
    #:description "linkage decl"
    #:auto-nested-attributes
    #:commit
    (pattern [(export:link-binding ...) unit-expr:expr import:tagged-link-id ...]))

  ;; In compound-unit, link-ids are bound by both the `import` clause and by
  ;; the exports section of a linkage-decl in a `link` clause. Information about
  ;; each link-id is stored in a link-id-binding record.
  (struct link-id-binding
    (order-index  ; The initialization order index of this link-id (0 for link-ids
                  ;   bound by imports and >0 for those bound by subunits).
     sig-id       ;\ The signature and siginfo of this link-id.
     siginfo      ;/
     access-expr) ; An expression that extracts the right signature vector
                  ;   from the appropriate unit table.
    #:transparent)

  ;; Returns #t if this link-id is bound by an import, #f if it is bound by a subunit.
  (define (link-id-binding-import? binding)
    (zero? (link-id-binding-order-index binding)))

  (define (link-id-binding->key-exprs binding tag)
    (siginfo->key-exprs (link-id-binding-siginfo binding) tag))

  ;; This utility function returns a list of natural numbers for use as a syntax
  ;; property needed to support units in Typed Racket.
  ;; Each number in the list is an index into a unit's list of imports signifying
  ;; that the import at that index is also an init-dependency of the unit.
  (define (build-init-depend-property init-depends imports)
    (define (sig=? s1 s2)
      (and (eq? (syntax-e (car s1)) (car s2))
           (free-identifier=? (cdr s1) (cdr s2))))
    (for/list ([import (in-list imports)]
               [index (in-naturals)]
               #:when (member import init-depends sig=?))
      index))

  ;; build-compound-unit : (->* [syntax?] [static-dep-info]
  ;;                            (values syntax?
  ;;                                    (listof identifier)
  ;;                                    (listof identifier)
  ;;                                    (listof identifier)))
  ;; Constructs the code for a compound-unit expression. The input syntax should
  ;; match the body of the compound-unit form (i.e. leaving off the initial
  ;; compound-unit identifier). The three additional return values are the
  ;; identifiers of the compound-unit's import and export signatures, plus
  ;; identifiers for initialization dependencies.
  (define (build-compound-unit stx [static-dep-info '()])
    (define/syntax-parse who:id (syntax-e (stx-car (current-syntax-context))))
    (define link-id-ctx (syntax-local-make-definition-context))
    (syntax-parse (internal-definition-context-add-scopes link-id-ctx stx)
      #:context (current-syntax-context)
      #:literals [import export link]
      [({~alt {~once (import in:link-binding ...)
                     #:too-few "missing import clause"
                     #:too-many "multiple import clauses"}
              {~once (export out:tagged-link-id ...)
                     #:too-few "missing export clause"
                     #:too-many "multiple export clauses"}
              {~once (link sub:linkage-decl ...)
                     #:too-few "missing link clause"
                     #:too-many "multiple link clauses"}}
        ...)
       #:fail-when (check-duplicate-identifier (append (attribute in.link-id)
                                                       (append* (attribute sub.export.link-id))))
                   "duplicate linking identifier definition"

       ;; Step 0: Bind some temporaries for use in the generated code.
       (define/syntax-parse deps-id (generate-temporary 'deps))
       (define/syntax-parse import-table-id (generate-temporary 'import-table))
       (define/syntax-parse [sub-unit-id ...] (generate-temporaries (attribute sub.unit-expr)))
       (define/syntax-parse [sub-export-table-id ...] (generate-temporaries (attribute sub.unit-expr)))

       ;; Step 1: Bind each `link-id` to a `link-id-binding` record so we can query it.
       (for ([order-index (in-naturals)]
             [link-ids (in-list (cons (attribute in.link-id)
                                      (attribute sub.export.link-id)))]
             [sig-ids (in-list (cons (attribute in.sig.sig-id)
                                     (attribute sub.export.sig.sig-id)))]
             [siginfos (in-list (cons (attribute in.sig.info)
                                      (attribute sub.export.sig.info)))]
             [key-exprs (in-list (cons (attribute in.sig.self-key)
                                       (attribute sub.export.sig.self-key)))]
             [table-id (in-list (cons (attribute import-table-id)
                                      (attribute sub-export-table-id)))])
         (for ([link-id (in-list link-ids)]
               [sig-id (in-list sig-ids)]
               [siginfo (in-list siginfos)]
               [key-expr (in-list key-exprs)])
           (syntax-local-bind-syntaxes
            (list link-id)
            #`(quote #,(link-id-binding order-index
                                        sig-id
                                        siginfo
                                        #`(hash-ref #,table-id #,key-expr)))
            link-id-ctx)))

       (define (lookup-link link-id)
         (define v (syntax-local-value link-id (λ () #f) link-id-ctx))
         (unless (link-id-binding? v)
           (wrong-syntax link-id "unknown linking identifier"))
         v)

       ;; Step 2: Do some simple compile-time checks.
       (for ([out-link-id (in-list (attribute out.link-id))])
         (define binding (lookup-link out-link-id))
         (when (link-id-binding-import? (lookup-link out-link-id))
           (wrong-syntax out-link-id "cannot directly export an import")))

       (define (do-check-duplicate-subs source-stxs link-bindings tag-syms)
         (check-duplicate-subs
          (for/list ([link-binding (in-list link-bindings)]
                     [tag-sym (in-list tag-syms)])
            (cons tag-sym (link-id-binding-siginfo link-binding)))
          source-stxs))

       (do-check-duplicate-subs (attribute out)
                                (map lookup-link (attribute out.link-id))
                                (attribute out.tag-sym))

       ;; Step 3: Resolve import/export linkages.
       (define (get-import-output-linkage link-ids tag-syms)
         (for/list ([link-id (in-list link-ids)]
                    [tag-sym (in-list tag-syms)])
           (define binding (lookup-link link-id))
           (list* (car (siginfo-names (link-id-binding-siginfo binding)))
                  (link-id-binding-access-expr binding)
                  (link-id-binding->key-exprs binding tag-sym))))

       (define/syntax-parse ([in-name-id _ in-key-expr ...] ...)
         (get-import-output-linkage (attribute in.link-id) (attribute in.sig.tag-sym)))
       (define/syntax-parse ([out-name-id out-access-expr out-key-expr ...] ...)
         (get-import-output-linkage (attribute out.link-id) (attribute out.tag-sym)))

       ;; Step 4: Resolve sub-unit imports linkages. The `check-sub-expr`s are
       ;; evaluated when the compound-unit form is evaluated and check that the
       ;; linkages are valid, and the `sub-import-table-expr`s build the import
       ;; tables passed to the unit when it is invoked.
       (define/syntax-parse [(check-sub-expr . sub-import-table-expr) ...]
         (for/list ([order-index (in-naturals 1)] ; imports have index 0, so start at 1
                    [unit-expr (in-list (attribute sub.unit-expr))]
                    [unit-id (in-list (attribute sub-unit-id))]
                    [in-stxs (in-list (attribute sub.import))]
                    [in-link-ids (in-list (attribute sub.import.link-id))]
                    [in-tags (in-list (attribute sub.import.tag-sym))]
                    [out-link-ids (in-list (attribute sub.export.link-id))]
                    [out-tags (in-list (attribute sub.export.sig.tag-sym))])
           (define in-bindings (map lookup-link in-link-ids))
           (define out-bindings (map lookup-link out-link-ids))
           (do-check-duplicate-subs in-stxs in-bindings in-tags)

           (define (get-names+keys bindings tags)
             (map (λ (binding tag)
                    (cons (car (siginfo-names (link-id-binding-siginfo binding)))
                          (link-id-binding->key-exprs binding tag)))
                  bindings
                  tags))
           (define/syntax-parse ([in-sig-name in-key-expr ...] ...) (get-names+keys in-bindings in-tags))
           (define/syntax-parse ([out-sig-name out-key-expr ...] ...) (get-names+keys out-bindings out-tags))

           ;; Analyze this sub-unit’s position in the unit initialization order
           ;; to construct the appropriate init-dep checks.
           (define/syntax-parse ([import-dep-expr ...]
                                 [(forward-dep-key-expr . forward-dep-names-expr) ...])
             (for/fold ([import-deps '()]
                        [forward-deps '()]
                        #:result (list import-deps forward-deps))
                       ([in-link-id (in-list in-link-ids)]
                        [in-binding (in-list in-bindings)]
                        [in-tag (in-list in-tags)])
               (cond
                 [(link-id-binding-import? in-binding)
                  (values (append (link-id-binding->key-exprs in-binding in-tag)
                                  import-deps)
                          forward-deps)]
                 [(>= (link-id-binding-order-index in-binding) order-index)
                  (values import-deps
                          (for/fold ([forward-deps forward-deps])
                                    ([dep-key-expr (in-list (link-id-binding->key-exprs in-binding in-tag))]
                                     [sig-name (in-list (siginfo-names (link-id-binding-siginfo in-binding)))])
                            (cons (cons dep-key-expr #`(cons '#,sig-name '#,in-link-id))
                                  forward-deps)))]
                 [else
                  (values import-deps forward-deps)])))

           (cons
            #`(begin
                ;; check that the unit expression is actually a unit
                #,(quasisyntax/loc unit-expr
                    (check-unit #,unit-id 'who))
                ;; check that the unit imports/exports the right signatures
                #,(quasisyntax/loc unit-expr
                    (check-sigs #,unit-id
                                (vector-immutable (cons 'in-sig-name
                                                        (vector-immutable in-key-expr ...))
                                                  ...)
                                (vector-immutable (cons 'out-sig-name
                                                        (vector-immutable out-key-expr ...))
                                                  ...)
                                'who))
                ;; check that the unit’s init-depends constraints are satisfied
                #,(quasisyntax/loc unit-expr
                    (check-deps (hash {~@ forward-dep-key-expr forward-dep-names-expr} ...)
                                #,unit-id
                                'who))
                ;; record any of the unit’s init-depends on imports
                (let ([import-deps (hash {~@ import-dep-expr #t} ...)])
                  (for-each (lambda (dep)
                              (when (hash-has-key? import-deps dep)
                                (set! deps-id (cons dep deps-id))))
                            (unit-deps #,unit-id))))

            (let ()
              (define/syntax-parse (([key-expr . access-expr] ...) ...)
                (for/list ([binding (in-list in-bindings)]
                           [key-exprs (in-list (attribute in-key-expr))])
                  (define access-expr (link-id-binding-access-expr binding))
                  (map (λ (key-expr) (cons key-expr access-expr)) key-exprs)))
              #`(hash {~@ key-expr access-expr} ... ...)))))

       ;; Step 6: Assemble the generated expression.
       (define compound-unit-expr
         (quasisyntax/loc this-syntax
           (let ([deps-id '()]
                 [sub-unit-id sub.unit-expr] ...)
             check-sub-expr ...
             (make-unit
              '#,(syntax-local-infer-name (current-syntax-context))
              (vector-immutable (cons 'in-name-id (vector-immutable in-key-expr ...)) ...)
              (vector-immutable (cons 'out-name-id (vector-immutable out-key-expr ...)) ...)
              (remove-duplicates deps-id)
              (lambda ()
                (let-values ([(sub-unit-id sub-export-table-id) ((unit-go sub-unit-id))] ...)
                  (values (lambda (import-table-id)
                            (void) ; just in case there are no sub-units
                            (sub-unit-id sub-import-table-expr) ...)
                          (unit-export ([out-key-expr ...] out-access-expr) ...))))))))

       ;; Step 7: Build static information.
       ;; TODO: These values come in a strange format. Really, it would make
       ;; sense for them to match the results of `unit-static-signatures` from
       ;; racket/unit-exptime, but instead they have an extra layer of syntax
       ;; wrapping around the `car` of each pair. It would be nice to fix this.
       (define static-imports (map syntax-e (syntax->list #'(({~? in.sig.tag-id #f} . in.sig.sig-id) ...))))
       (define/syntax-parse [out-sig-id ...] (for/list ([out-link-id (in-list (attribute out.link-id))])
                                               (link-id-binding-sig-id (lookup-link out-link-id))))
       (define static-exports (map syntax-e (syntax->list #'(({~? out.tag-id #f} . out-sig-id) ...))))

       ;; We’re done!
       (values (syntax-parse-track-literals
                (syntax-protect
                 (syntax-property
                  compound-unit-expr
                  'unit:inferred-init-depends
                  (build-init-depend-property static-dep-info static-imports))))
               static-imports
               static-exports
               static-dep-info)])))

(define-syntax-parser compound-unit
  [(_ . body)
   (define-values [u x y z] (build-compound-unit #'body))
   u])

;; -----------------------------------------------------------------------------
;; `invoke-unit`

(define (invoke-unit/core unit)
  (check-unit unit 'invoke-unit)
  (check-no-imports unit 'invoke-unit)
  (let-values ([(f exports) ((unit-go unit))])
    (f #f)))

(define-syntax-parser define-values/invoke-unit/core
  #:track-literals
  [(_ unit-expr:expr out:tagged-import-spec ...)
   (let ([dup-id (check-duplicate-identifier (append-map signature-ie-int-names (attribute out.value)))])
     (when dup-id
       (raise-stx-err (format "duplicate binding for ~.s" (syntax-e dup-id)))))

   (define tmarker (make-syntax-introducer))
   (define tmp-bindings (map (λ (s) (map tmarker (map car (signature-vars s)))) (attribute out.value)))
   (define def-table (make-bound-id-table))
   (for ([sig (in-list (attribute out.value))]
         [new-xs (in-list tmp-bindings)])
     (for ([old (in-list (map car (signature-vars sig)))]
           [new (in-list new-xs)])
       (bound-id-table-set! def-table old new)))

   (define/syntax-parse ([tmp-binding ...] ...) tmp-bindings)
   (define/syntax-parse [out-vec ...] (generate-temporaries (attribute out)))
   (define/syntax-parse ([rename-bind [stx-bind ...] [val-bind ...]] ...)
     (map (build-val+macro-defs values) (attribute out.value)))

   (define/syntax-parse ([out-code ...] ...)
     (for/list ([os (in-list (attribute out.value))]
                [ov (in-list (attribute out-vec))])
       (for/list ([i (in-range (length (signature-vars os)))])
         #`(vector-ref #,ov #,i))))

   (define/syntax-parse ((wrap-code ...) ...)
     (for/list ([os (in-list (attribute out.value))]
                [ov (in-list (attribute out-vec))]
                [tbs (in-list (attribute tmp-binding))])
       (define rename-bindings 
         (get-member-bindings def-table os #'(quote-module-name) #t))
       (for/list ([tb (in-list tbs)]
                  [i (in-naturals)]
                  [v (in-list (map car (signature-vars os)))]
                  [c (in-list (signature-ctcs os))])
         (if c
             (with-syntax ([ctc-stx
                            (syntax-property
                             #`(letrec-syntax #,rename-bindings #,c)
                             'inferred-name v)])
               #`(let ([v/c (#,tb)])
                   (contract ctc-stx (car v/c) (cdr v/c)
                             (current-contract-region)
                             (quote #,v) (quote-srcloc #,v))))
             #`(#,tb)))))
   
   #`(begin
       (define-values (tmp-binding ... ...)
         #,(syntax/loc #'unit-expr
             (let ((unit-tmp unit-expr))
               (check-unit unit-tmp 'define-values/invoke-unit)
               (check-sigs unit-tmp
                           (vector-immutable)
                           (vector-immutable (cons 'out.info.self-id
                                                   (vector-immutable out.key ...)) ...)
                           'define-values/invoke-unit)
               (let-values ([(unit-fn export-table) ((unit-go unit-tmp))])
                 (let ([out-vec (hash-ref export-table out.self-key)] ...)
                   (unit-fn #f)
                   (values out-code ... ...))))))
       (define-values (out.var.int-id ... ...)
         (values wrap-code ... ...))
       (define-syntaxes . rename-bind) ...
       (define-syntaxes . stx-bind) ... ...
       (define-values . val-bind) ... ...)])

(define-for-syntax (temp-id-with-tags id i)
  (let loop ([i i] [at #f])
    (syntax-case i (tag bind-at)
      [(tag t sig)
       (list id #`(tag t #,id) (let ([l #'(tag t sig)])
                                 (if at
                                     #`(bind-at #,at #,l)
                                     l)))]
      [(bind-at u i)
       (loop #'i #'u)]
      [_else
       (list id id (if at
                       #`(bind-at #,at #,i)
                       i))])))

(define-syntax-parser invoke-unit
  #:track-literals
  #:literals [import]
  [(_ unit:expr)
   (syntax/loc this-syntax
     (invoke-unit/core unit))]
  [(_ unit:expr (import in:tagged-import-spec ...))
   (define/syntax-parse [u ...] (generate-temporaries (attribute in)))
   (define/syntax-parse ([U Ul isig] ...) (map temp-id-with-tags
                                               (generate-temporaries (attribute in))
                                               (attribute in)))
   (quasisyntax/loc this-syntax
     (let ()
       (define u (with-loc+name #,this-syntax
                   (unit-from-context in)))
       ...
       (with-loc #,this-syntax
         (invoke-unit/core
          (with-loc+name #,this-syntax
            (compound-unit
              (import) (export)
              (link [((U : in.sig-id)) u] ... [() unit Ul ...])))))))])

(define-syntax-parser define-values/invoke-unit
  #:track-literals
  #:literals [import export]
  [(_ {~describe "unit expression" u:expr}
      {~describe "import clause" (import)}
      {~describe "export clause" (export e ...)})
   (quasisyntax/loc this-syntax
     (define-values/invoke-unit/core u e ...))]
  [(_ {~describe "unit expression" u:expr}
      {~describe "import clause" (import i:tagged-import-spec ...)}
      {~describe "export clause" (export e:tagged-export-spec ...)})
   (with-syntax ((((EU EUl e) ...) (map temp-id-with-tags
                                        (generate-temporaries #'(e ...))
                                        (syntax->list #'(e ...))))
                 (((IU IUl i) ...) (map temp-id-with-tags
                                        (generate-temporaries #'(i ...))
                                        (syntax->list #'(i ...))))
                 ((iu ...) (generate-temporaries #'(i ...))))
     (quasisyntax/loc this-syntax
       (begin
         (define iu (with-loc+name #,this-syntax
                      (unit-from-context i)))
         ...
         (with-loc #,this-syntax
           (define-values/invoke-unit/core
             (with-loc+name #,this-syntax
               (compound-unit
                 (import) (export EUl ...)
                 (link [((IU : i.tagged-sig-id)) iu] ... [((EU : e.tagged-sig-id) ...) u IUl ...])))
             e ...)))))])
