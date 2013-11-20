#lang racket/base

#|

This file defines two sorts of primitives. All of them are provided into any module using the typed racket language.

1. macros for defining type annotated code.
   this includes: lambda:, define:, etc
   potentially, these macros should be replacements for the mzscheme ones in the user program
   however, it's nice to be able to use both when the sugar is more limited
   for example: (define ((f x) y) (+ x y))

2. macros for defining 'magic' code
   examples: define-typed-struct, require/typed
   these expand into (ignored) mzscheme code, and declarations that a typechecker understands
   in order to protect the declarations, they are wrapped in `#%app void' so that local-expand of the module body
   will not expand them on the first pass of the macro expander (when the stop list is ignored)

3. contracted versions of built-in racket values such as parameters and prompt tags
   that are defined in "base-contracted.rkt"

|#


(provide (except-out (all-defined-out) dtsi* dtsi/exec* let-internal: define-for-variants define-for*-variants 
                     with-handlers: for/annotation for*/annotation define-for/acc:-variants base-for/flvector: base-for/vector
                     -lambda -define)
         ;; provide the contracted bindings as primitives
         (all-from-out "base-contracted.rkt")
         (all-from-out "top-interaction.rkt")
         :
         (rename-out [define-typed-struct define-struct:]
                     [lambda: λ:]
                     [-lambda lambda]
                     [-lambda λ]
                     [-define define]
                     [with-handlers: with-handlers]
                     [define-typed-struct/exec define-struct/exec:]
                     [for/annotation for]
                     [for*/annotation for*]))

(module struct-extraction racket/base
  (provide extract-struct-info/checked)
  (require syntax/parse racket/struct-info)
  (define (extract-struct-info/checked id)
    (syntax-parse id
     [(~var id (static struct-info? "identifier bound to a structure type"))
      (extract-struct-info (syntax-local-value #'id))])))

(require "../utils/require-contract.rkt"
         "colon.rkt"
         "../typecheck/internal-forms.rkt"
         (rename-in racket/contract/base [-> c->] [case-> c:case->])
         ;; contracted bindings to replace built-in ones
         (except-in "base-contracted.rkt" initialize-contracted)
         "top-interaction.rkt"
         "base-types.rkt"
         "base-types-extra.rkt"
         'struct-extraction
         racket/flonum ; for for/flvector and for*/flvector
         (for-syntax
          racket/lazy-require
          syntax/parse
          syntax/stx
          racket/list
          racket/syntax
          unstable/sequence
          unstable/syntax
          racket/base
          racket/struct-info
          syntax/struct
          "annotate-classes.rkt"
          "../utils/tc-utils.rkt"
          "../private/syntax-properties.rkt"
          "../types/utils.rkt"
          "for-clauses.rkt"
          'struct-extraction)
         "../types/numeric-predicates.rkt"
         racket/unsafe/ops
         racket/vector)
(provide index?) ; useful for assert, and racket doesn't have it

;; Lazily loaded b/c they're only used sometimes, so we save a lot
;; of loading by not having them when they are unneeded
(begin-for-syntax 
  (lazy-require ["../rep/type-rep.rkt" (make-Opaque Error?)]
                [syntax/define (normalize-definition)]
                [typed-racket/private/parse-type (parse-type)]
                [typed-racket/private/type-contract (type->contract type->contract-fail)]
                [typed-racket/env/type-name-env (register-type-name)]))

(define-for-syntax (ignore stx) (ignore-property stx #t))

(begin-for-syntax
  (define-syntax-class opt-parent
    #:attributes (nm parent)
    (pattern nm:id #:with parent #'#f)
    (pattern (nm:id parent:id))))


(define-syntaxes (require/typed-legacy require/typed)
 (let ()
  (define-syntax-class opt-rename
    #:attributes (nm spec)
    (pattern nm:id
             #:with spec #'nm)
    (pattern (orig-nm:id internal-nm:id)
             #:with spec #'(orig-nm internal-nm)
             #:with nm #'internal-nm))

  (define-syntax-class simple-clause
    #:attributes (nm ty)
    (pattern [nm:opt-rename ty]))

  (define-splicing-syntax-class (opt-constructor legacy struct-name)
   #:attributes (value)
   (pattern (~seq) #:attr value (if legacy
                                    #`(#:extra-constructor-name #,(format-id struct-name "make-~a" struct-name))
                                    #'()))
   (pattern (~seq (~and key (~or #:extra-constructor-name #:constructor-name)) name:id) #:attr value #'(key name)))

  (define-syntax-class (struct-clause legacy)
    ;#:literals (struct)
    #:attributes (nm (body 1) (constructor-parts 1))
    (pattern [(~or (~datum struct) #:struct) nm:opt-parent (body ...) (~var constructor (opt-constructor legacy #'nm.nm))]
             #:with (constructor-parts ...) #'constructor.value))

  (define-syntax-class opaque-clause
    ;#:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [(~or (~datum opaque) #:opaque) ty:id pred:id]
             #:with opt #'())
    (pattern [(~or (~datum opaque) #:opaque) opaque ty:id pred:id #:name-exists]
             #:with opt #'(#:name-exists)))

  (define-syntax-class (clause legacy lib)
   #:attributes (spec)
   (pattern oc:opaque-clause #:attr spec
     #`(require/opaque-type oc.ty oc.pred #,lib . oc.opt))
   (pattern (~var strc (struct-clause legacy)) #:attr spec
     #`(require-typed-struct strc.nm (strc.body ...) strc.constructor-parts ... #,lib))
   (pattern sc:simple-clause #:attr spec
     #`(require/typed #:internal sc.nm sc.ty #,lib)))


  (define ((r/t-maker legacy) stx)
    (syntax-parse stx
      [(_ lib:expr (~var c (clause legacy #'lib)) ...)
       (when (zero? (syntax-length #'(c ...)))
         (raise-syntax-error #f "at least one specification is required" stx))
       #`(begin c.spec ...)]
      [(_ #:internal nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
       (define/with-syntax cnt*
         (if (eq? (syntax-local-context) 'top-level)
             ;; if we're at the top-level, we can generate the contract
             ;; immediately, but otherwise the contract will be fixed up
             ;; by the module type-checking pass later
             (let ([typ (parse-type #'ty)])
               (ignore
                (type->contract
                 typ
                 ;; this is for a `require/typed', so the value is not
                 ;; from the typed side
                 #:typed-side #f
                 (type->contract-fail typ #'ty))))
             ;; in the fix-up case, the contract is just an identifier
             ;; that is defined below
             (generate-temporary #'nm.nm)))
       (define/with-syntax hidden (generate-temporary #'nm.nm))
       (define/with-syntax sm (if (attribute parent)
                                  #'(#:struct-maker parent)
                                  #'()))
       (define property
         (if (attribute parent)
             contract-def/maker-property
             contract-def-property))
       (quasisyntax/loc stx
         (begin
           ;; define `cnt*` to be fixed up later by the module
           ;; type-checking (not defined at top-level since it
           ;; doesn't work with local expansion)
           #,@(ignore (if (eq? (syntax-local-context) 'top-level)
                          #'()
                          #`(#,(property #'(define cnt* #f) #'ty))))
           #,(internal #'(require/typed-internal hidden ty . sm))
           #,(ignore #'(require/contract nm.spec hidden cnt* lib))))]))
  (values (r/t-maker #t) (r/t-maker #f))))

(define-syntax (require/typed/provide stx)
  (unless (memq (syntax-local-context) '(module module-begin))
    (raise-syntax-error 'require/typed/provide
                        "can only be used at module top-level"))
  (syntax-parse stx
    [(_ lib) #'(begin)]
    [(_ lib [r:id t] other-clause ...)
     #'(begin (require/typed lib [r t])
              (provide r)
              (require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:struct name:id ([f:id (~datum :) t] ...)
                                   option ...])
        other-clause ...)
     #'(begin (require/typed lib clause)
              (provide (struct-out name))
              (require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:struct (name:id parent:id)
                                   ([f:id (~datum :) t] ...)
                                   option ...])
        other-clause ...)
     #'(begin (require/typed lib clause)
              (provide (struct-out name))
              (require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:opaque t:id pred:id])
        other-clause ...)
     #'(begin (require/typed lib clause)
              (provide t pred)
              (require/typed/provide lib other-clause ...))]))

(define-syntax require-typed-struct/provide
  (syntax-rules ()
    [(_ (nm par) . rest)
     (begin (require-typed-struct (nm par) . rest)
            (provide (struct-out nm)))]
    [(_ nm . rest)
     (begin (require-typed-struct nm . rest)
            (provide (struct-out nm)))]))

;; Conversion of types to contracts
;;  define-predicate
;;  make-predicate
;;  cast

(define-syntax (define-predicate stx)
  (syntax-parse stx
    [(_ name:id ty:expr)
     #`(begin
         #,(ignore-property (if (eq? (syntax-local-context) 'top-level)
                                #'(define name (procedure-rename (make-predicate ty) 'name))
                                (flat-contract-def-property  #'(define name #f) #'ty))
                            #t)
         ;; not a require, this is just the unchecked declaration syntax
         #,(internal #'(require/typed-internal name (Any -> Boolean : ty))))]))

(define-syntax (make-predicate stx)
  (syntax-parse stx
    [(_ ty:expr)
     (if (syntax-transforming-module-expression?)
       (let ((name (syntax-local-lift-expression
                     (flat-contract-def-property #'#f #'ty))))
         (define (check-valid-type _)
           (define type (parse-type #'ty))
           (define vars (fv type))
           ;; If there was an error don't create another one
           (unless (or (Error? type) (null? vars))
             (tc-error/delayed
               "Type ~a could not be converted to a predicate because it contains free variables."
               type)))

         #`(ann
             #,(external-check-property (ignore-some-property name #t) check-valid-type)
             (Any -> Boolean : ty)))
       (let ([typ (parse-type #'ty)])
         (if (Error? typ)
             ;; This code should never get run, typechecking will have an error earlier
             #`(error 'make-predicate "Couldn't parse type")
             #`(#%expression
                (ann
                 #,(ignore-some-property
                    (type->contract
                     typ
                     ;; must be a flat contract
                     #:kind 'flat
                     ;; the value is not from the typed side
                     #:typed-side #f
                     (type->contract-fail typ #'ty #:ctc-str "predicate"))
                    #t)
                 (Any -> Boolean : ty))))))]))

(define-syntax (cast stx)
  (syntax-parse stx
    [(_ v:expr ty:expr)
     (define (apply-contract ctc-expr)
       #`(#%expression
           (ann
             #,(ignore-some-property
                 #`(let-values (((val) #,(with-type-property #'(ann v Any) #t)))
                     (contract
                       #,ctc-expr
                       val
                       'cast
                       'typed-world
                       val
                       (quote-syntax #,stx)))
                 #t)
             ty)))

     (cond [(not (unbox typed-context?)) ; no-check, don't check
            #'v]
           [(syntax-transforming-module-expression?)
            (let ((ctc (syntax-local-lift-expression
                        (contract-def-property #'#f #'ty))))
              (define (check-valid-type _)
                (define type (parse-type #'ty))
                (define vars (fv type))
                ;; If there was an error don't create another one
                (unless (or (Error? type) (null? vars))
                  (tc-error/delayed
                   "Type ~a could not be converted to a contract because it contains free variables."
                   type)))
              (external-check-property (apply-contract ctc) check-valid-type))]
           [else
            (let ([typ (parse-type #'ty)])
              (if (Error? typ)
                  ;; This code should never get run, typechecking will have an error earlier
                  #`(error 'cast "Couldn't parse type")
                  (apply-contract
                   (type->contract
                    typ
                    ;; the value is not from the typed side
                    #:typed-side #f
                    (type->contract-fail typ #'ty)))))])]))

(define-syntax (require/opaque-type stx)
  (define-syntax-class name-exists-kw
    (pattern #:name-exists))
  (syntax-parse stx
    [(_ ty:id pred:id lib (~optional ne:name-exists-kw) ...)
     (register-type-name #'ty (make-Opaque #'pred))
     (with-syntax ([hidden (generate-temporary #'pred)])
       (quasisyntax/loc stx
         (begin
           #,(ignore-property #'(define pred-cnt (any/c . c-> . boolean?)) #t)
           #,(internal #'(require/typed-internal hidden (Any -> Boolean : (Opaque pred))))
           #,(if (attribute ne)
                 (internal (syntax/loc stx (define-type-alias-internal ty (Opaque pred))))
                 (syntax/loc stx (define-type-alias ty (Opaque pred))))
           #,(ignore-property #'(require/contract pred hidden pred-cnt lib) #t))))]))

(begin-for-syntax
  (define-syntax-class type-variables
    #:attributes ((vars 1))
    #:description "a sequence of type variables"
    (pattern (vars:id ...)
      #:fail-when (check-duplicate-identifier (syntax->list #'(vars ...)))
                  "duplicate type variable declaration")))

(define-syntax (plambda: stx)
  (syntax-parse stx
    [(plambda: tvars:type-variables formals . body)
     (quasisyntax/loc stx
       (#%expression
        #,(plambda-property
            (syntax/loc stx (lambda: formals . body))
            #'(tvars.vars ...))))]))

(define-syntax (pcase-lambda: stx)
  (syntax-parse stx
    [(pcase-lambda: tvars:type-variables cl ...)
     (quasisyntax/loc stx
       (#%expression
        #,(plambda-property
            (syntax/loc stx (case-lambda: cl ...))
            #'(tvars.vars ...))))]))

(define-syntax (popt-lambda: stx)
  (syntax-parse stx
    [(popt-lambda: tvars:type-variables formals . body)
     (quasisyntax/loc stx
       (#%expression
        #,(plambda-property
            (syntax/loc stx (opt-lambda: formals . body))
            #'(tvars.vars ...))))]))

(define-syntax (pdefine: stx)
  (syntax-parse stx #:literals (:)
    [(pdefine: tvars:type-variables (nm:id . formals:annotated-formals) : ret-ty . body)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars.vars ...) (formals.arg-ty ... -> ret-ty)))])
       (syntax/loc stx
         (begin
          (: nm : type)
          (define (nm . formals.ann-formals) . body))))]))

(define-syntax (ann stx)
  (syntax-parse stx #:literals (:)
    [(_ (~or (~seq arg : ty) (~seq arg ty)))
     (type-ascription-property #'arg #'ty)]))

(define-syntax (inst stx)
  (syntax-parse stx #:literals (:)
    [(_ arg : . tys)
     (syntax/loc stx (inst arg . tys))]
    [(_ arg tys ... ty ddd b:id)
     #:when (eq? (syntax-e #'ddd) '...)
     (type-inst-property (syntax/loc #'arg (#%expression arg)) #'(tys ... (ty . b)))]
    [(_ arg tys ...)
     (type-inst-property (syntax/loc #'arg (#%expression arg)) #'(tys ...))]))

(define-syntax (define: stx)
  (syntax-parse stx #:literals (:)
    [(define: (nm:id . formals:annotated-formals) (~describe "return type annotation" (~seq : ret-ty)) body ...)
     (with-syntax ([arrty (syntax/loc stx (formals.arg-ty ... -> ret-ty))])
       (syntax/loc stx
         (define: nm : arrty
           (lambda: formals body ...))))]
    [(define: nm:id ~! (~describe ":" :) (~describe "type" ty) body)
     (identifier? #'nm)
     (with-syntax ([new-nm (type-label-property #'nm #'ty)])
       (syntax/loc stx (define new-nm body)))]
    [(define: tvars:type-variables nm:id : ty body)
     (with-syntax ([type (syntax/loc #'ty (All (tvars.vars ...) ty))])
       (syntax/loc stx
         (begin
           (: nm : type)
           (define nm body))))]
    [(define: tvars:type-variables (nm:id . formals:annotated-formals) : ret-ty body ...)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars.vars ...) (formals.arg-ty ... -> ret-ty)))])
       (syntax/loc stx
         (begin
          (: nm : type)
          (define (nm . formals.ann-formals) body ...))))]))

(define-syntax (lambda: stx)
  (syntax-parse stx
    [(lambda: formals:annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntax (case-lambda: stx)
  (syntax-parse stx
    [(case-lambda: [formals:annotated-formals . body] ...)
     (syntax/loc stx (case-lambda [formals.ann-formals . body] ...))]))

(define-syntax (opt-lambda: stx)
  (syntax-parse stx
    [(opt-lambda: formals:opt-lambda-annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntaxes (let-internal: let*: letrec:)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:optionally-annotated-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let) (mk #'let*) (mk #'letrec))))

(define-syntaxes (let-values: let*-values: letrec-values:)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:optionally-annotated-values-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let-values) (mk #'let*-values) (mk #'letrec-values))))

(define-syntax (let: stx)
  (syntax-parse stx #:literals (:)
    [(let: nm:id ~! ; named let:
           (~and (~seq (~optional (~seq : ret-ty))
                       (bs:optionally-annotated-binding ...) body ...)
                 (~seq rest ...)))
     (quasisyntax/loc stx
       (#,(syntax-parse #'(rest ...)
            #:literals (:)
            [(: ret-ty (bs:annotated-binding ...) . body)
             (quasisyntax/loc stx
               (letrec: ([nm : (bs.ty ... -> ret-ty)
                             #,(quasisyntax/loc stx
                                 (lambda (bs.ann-name ...) . #,(syntax/loc stx body)))])
                        #,(quasisyntax/loc stx nm)))]
            [(: ret-ty (bs:optionally-annotated-binding ...) body ... bod)
             (quasisyntax/loc stx
               (letrec ([nm #,(quasisyntax/loc stx
                                (lambda (bs.ann-name ...) body ... (ann #,(syntax/loc stx bod) ret-ty)))])
                 #,(quasisyntax/loc stx nm)))]
            [((bs:optionally-annotated-binding ...) . body)
             (quasisyntax/loc stx
               (letrec ([nm #,(quasisyntax/loc stx
                                (lambda (bs.ann-name ...) . #,(syntax/loc stx body)))])
                 #,(quasisyntax/loc stx nm)))])
        bs.rhs ...))]
    [(let: . rest)
     (syntax/loc stx (let-internal: . rest))]))

(define-syntax (plet: stx)
  (syntax-parse stx #:literals (:)
    [(_ (A:id ...) ([bn:optionally-annotated-name e] ...) . body)
     (syntax/loc stx
       ((plambda: (A ...) (bn ...) . body) e ...))]))

(define-syntax (define-type-alias stx)
  (syntax-parse stx
    [(_ tname:id rest (~optional (~and omit #:omit-define-syntaxes)
                                 #:defaults
                                 ([omit #f])))
     #`(begin
         #,(if (not (attribute omit))
               (ignore #'(define-syntax tname
                           (lambda (stx)
                             (raise-syntax-error
                              'type-check
                              "type name used out of context"
                              stx
                              (and (stx-pair? stx) (stx-car stx))))))
               #'(begin))
         #,(internal (syntax/loc stx (define-type-alias-internal tname rest))))]
    [(_ (tname:id args:id ...) rest)
     (syntax/loc stx (define-type-alias tname (All (args ...) rest)))]))

(define-syntax (with-handlers: stx)
  (syntax-parse stx
    [(_ ([pred? action] ...) . body)
     (with-syntax ([(pred?* ...)
                    (for/list ([s (in-syntax #'(pred? ...))])
                      (with-type-property #`(ann #,s : (Any -> Any)) #t))]
                   [(action* ...)
                    (for/list ([s (in-syntax #'(action ...))])
                      (exn-handler-property s #t))]
                   [body* (exn-body-property #'(let-values () . body) #t)])
       (with-handlers-property #'(with-handlers ([pred?* action*] ...) body*) #t))]))

(begin-for-syntax
  (define-syntax-class dtsi-struct-name
    #:description "struct name (with optional super-struct name)"
    #:attributes (name super value)
    (pattern ((~var name (static struct-info? "struct name")) super:id)
             #:attr value (attribute name.value))
    (pattern (~var name (static struct-info? "struct name"))
             #:attr value (attribute name.value)
             #:with super #f)))

(define-syntax (define-typed-struct/exec stx)
  (syntax-parse stx #:literals (:)
    [(_ nm ((~describe "field specification" [fld:optionally-annotated-name]) ...) [proc : proc-ty])
     (with-syntax*
      ([proc* (with-type-property #'(ann proc : proc-ty) #t)]
       [d-s (ignore-some-property (syntax/loc stx (define-struct nm (fld.name ...)
                                               #:property prop:procedure proc*))
                                  #t)]
       [dtsi (quasisyntax/loc stx (dtsi/exec* () nm (fld ...) proc-ty))])
      #'(begin d-s dtsi))]))

(define-syntaxes (dtsi* dtsi/exec*)
  (let ()
    (define (mk internal-id)
      (lambda (stx)
        (syntax-parse stx
          [(_ () nm:dtsi-struct-name . rest)
           (internal (quasisyntax/loc stx
                       (#,internal-id
                        #,(struct-info-property #'nm (attribute nm.value)) . rest)))]
          [(_ (vars:id ...) nm:dtsi-struct-name . rest)
           (internal (quasisyntax/loc stx
                       (#,internal-id (vars ...)
                                      #,(struct-info-property #'nm (attribute nm.value)) . rest)))])))
    (values (mk #'define-typed-struct-internal)
            (mk #'define-typed-struct/exec-internal))))

;; Syntax classes and helpers for `struct:`
(begin-for-syntax
  (define-syntax-class fld-spec
    #:literals (:)
    #:description "[field-name : type]"
    (pattern [fld:id : ty]))

  (define-syntax-class struct-name
    #:description "struct name (with optional super-struct name)"
    #:attributes (name super)
    (pattern (name:id super:id))
    (pattern name:id
             #:with super #f))
  
  (define-splicing-syntax-class struct-name/new
    #:description "struct name (with optional super-struct name)"
    (pattern (~seq name:id super:id)
             #:attr old-spec #'(name super)
             #:with new-spec #'(name super))
    (pattern name:id
             #:with super #f
             #:attr old-spec #'name
             #:with new-spec #'(name)))

  (define-splicing-syntax-class maybe-type-vars
   #:description "optional list of type variables"
   #:attributes ((vars 1))
   (pattern (vars:id ...))
   (pattern (~seq) #:attr (vars 1) null))

  (define-splicing-syntax-class struct-options
    #:description "typed structure type options"
    #:attributes (guard mutable? transparent? [prop 1] [prop-val 1])
    (pattern (~seq (~or (~optional (~seq (~and #:mutable mutable?)))
                        (~optional (~seq (~and #:transparent transparent?)))
                        ;; FIXME: unsound, but relied on in core libraries
                        ;; #:guard ought to be supportable with some work
                        ;; #:property is harder
                        (~optional (~seq #:guard guard:expr))
                        (~seq #:property prop:expr prop-val:expr))
                   ...))))

;; User-facing macros for defining typed structure types
(define-syntaxes (define-typed-struct struct:)
  (values
   (lambda (stx)
     (syntax-parse stx
       [(_ vars:maybe-type-vars nm:struct-name (fs:fld-spec ...)
           opts:struct-options)
        (let ([mutable? (if (attribute opts.mutable?) #'(#:mutable) #'())]
              [cname (second (build-struct-names #'nm.name empty #t #t))])
          (with-syntax ([d-s (ignore-some-property
                               (syntax/loc stx (define-struct nm (fs.fld ...) . opts))
                               #t)]
                        [dtsi (quasisyntax/loc stx
                                (dtsi* (vars.vars ...) nm (fs ...)
                                       #:maker #,cname
                                       #,@mutable?))])
            (if (eq? (syntax-local-context) 'top-level)
                ;; Use `eval` at top-level to avoid an unbound id error
                ;; from dtsi trying to look at the d-s bindings.
                #'(begin (eval (quote-syntax d-s))
                         ;; It is important here that the object under the
                         ;; eval is a quasiquoted literal in order
                         ;; for #%top-interaction to get the lexical
                         ;; information for TR's actual #%top-interaction.
                         ;; This effectively lets us invoke the type-checker
                         ;; dynamically.
                         ;;
                         ;; The quote-syntax is also important because we want
                         ;; the `dtsi` to have the lexical information from
                         ;; this module. This ensures that the `dtsi` macro
                         ;; is actually bound to its definition above.
                         (eval `(#%top-interaction . ,(quote-syntax dtsi))))
                #'(begin d-s dtsi))))]))
   (lambda (stx)
     (syntax-parse stx
       [(_ vars:maybe-type-vars nm:struct-name/new (fs:fld-spec ...)
           opts:struct-options)
        (let ([mutable? (if (attribute opts.mutable?) #'(#:mutable) #'())])
          (with-syntax ([d-s (ignore-property (quasisyntax/loc stx
                                                (struct #,@(attribute nm.new-spec) (fs.fld ...)
                                                        . opts))
                                              #t)]
                        [dtsi (quasisyntax/loc stx
                                (dtsi* (vars.vars ...)
                                       nm.old-spec (fs ...)
                                       #,@mutable?))])
            ;; see comment above
            (if (eq? (syntax-local-context) 'top-level)
                #'(begin (eval (quote-syntax d-s))
                         (eval `(#%top-interaction . ,(quote-syntax dtsi))))
                #'(begin d-s dtsi))))]))))


;Copied from racket/private/define-struct
;FIXME when multiple bindings are supported
(define-for-syntax (self-ctor-transformer orig stx)
  (define (transfer-srcloc orig stx)
    (datum->syntax orig (syntax-e orig) stx orig))
  (syntax-case stx ()
    [(self arg ...) (datum->syntax stx
                                   (cons (syntax-property (transfer-srcloc orig #'self)
                                                          'constructor-for
                                                          (syntax-local-introduce #'self))
                                         (syntax-e (syntax (arg ...))))
                                   stx
                                   stx)]
    [_ (transfer-srcloc orig stx)]))


(define-for-syntax make-struct-info-self-ctor
 (let ()
  (struct struct-info-self-ctor (id info)
          #:property prop:procedure
                     (lambda (ins stx)
                      (self-ctor-transformer (struct-info-self-ctor-id ins) stx))
          #:property prop:struct-info (lambda (x) (extract-struct-info (struct-info-self-ctor-info x))))
  struct-info-self-ctor))



(define-syntaxes (require-typed-struct-legacy
                  require-typed-struct)
 (let ()

  (define-splicing-syntax-class (constructor-term legacy struct-name)
   (pattern (~seq) #:fail-when legacy #f #:attr name struct-name #:attr extra #f)
   (pattern (~seq) #:fail-unless legacy #f #:attr name (format-id struct-name "make-~a" struct-name) #:attr extra #t)
   (pattern (~seq #:constructor-name name:id) #:attr extra #f)
   (pattern (~seq #:extra-constructor-name name:id) #:attr extra #t))


  (define ((rts legacy) stx)
    (syntax-parse stx #:literals (:)
      [(_ name:opt-parent ([fld : ty] ...) (~var input-maker (constructor-term legacy #'name.nm)) lib)
       (with-syntax* ([nm #'name.nm]
                      [parent #'name.parent]
                      [hidden (generate-temporary #'name.nm)]
                      [orig-struct-info (generate-temporary #'nm)]
                      [spec (if (syntax-e #'name.parent) #'(nm parent) #'nm)]
                      [num-fields (syntax-length #'(fld ...))]
                      [(type-des _ pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                      [(mut ...) (stx-map (lambda _ #'#f) #'(sel ...))]
                      [maker-name #'input-maker.name]
                      ;maker-name's symbolic form is used in the require form
                      [id-is-ctor? (or (attribute input-maker.extra) (bound-identifier=? #'maker-name #'nm))]
                      [internal-maker (generate-temporary #'maker-name)] ;Only used if id-is-ctor? is true
                      [real-maker (if (syntax-e #'id-is-ctor?) #'internal-maker #'maker-name)] ;The actual identifier bound to the constructor
                      [extra-maker (and (attribute input-maker.extra)
                                        (not (bound-identifier=? #'make-name #'nm))
                                        #'maker-name)])
                     (define (maybe-add-quote-syntax stx)
                       (if (and stx (syntax-e stx)) #`(quote-syntax #,stx) stx))

                     (quasisyntax/loc stx
                       (begin
                         (require (only-in lib type-des (nm orig-struct-info)))

                         (define-for-syntax si
                           (let ()
                             (define-values (orig-type-des orig-maker orig-pred orig-sels orig-muts orig-parent)
                               (apply values (extract-struct-info/checked (quote-syntax orig-struct-info))))

                             (define (id-drop sels muts num)
                               (cond
                                [(zero? num) (values sels muts)]
                                [(null? sels) (int-err "id-drop: Too short of list")]
                                [(pair? sels)
                                 (cond
                                   [(not (car sels)) (values sels muts)]
                                   [else (id-drop (cdr sels) (cdr muts) (sub1 num))])]
                                [else (int-err "id-drop: Not a list")]))

                             (define (struct-info-list new-sels new-muts)
                               (list (quote-syntax type-des)
                                     (quote-syntax real-maker)
                                     (quote-syntax pred)
                                     (append (list #,@(map maybe-add-quote-syntax (reverse (syntax->list #'(sel ...)))))
                                             new-sels)
                                     (append (list #,@(map maybe-add-quote-syntax (reverse (syntax->list #'(mut ...)))))
                                             new-muts)
                                     orig-parent))

                             (make-struct-info
                               (lambda ()
                                 #,(if (syntax-e #'parent)
                                       (let-values (((parent-type-des parent-maker parent-pred
                                                      parent-sel  parent-mut grand-parent)
                                                     (apply values (extract-struct-info/checked #'parent))))
                                         #`(struct-info-list
                                             (list #,@(map maybe-add-quote-syntax parent-sel))
                                             (list #,@(map maybe-add-quote-syntax parent-mut))))
                                       #`(let-values (((new-sels new-muts) (id-drop orig-sels orig-muts num-fields)))
                                           (struct-info-list new-sels new-muts)))))))

                         (define-syntax nm
                              (if id-is-ctor?
                                  (make-struct-info-self-ctor #'internal-maker si)
                                  si))

                         (dtsi* () spec ([fld : ty] ...) #:maker maker-name #:type-only)
                         #,(ignore #'(require/contract pred hidden (any/c . c-> . boolean?) lib))
                         #,(internal #'(require/typed-internal hidden (Any -> Boolean : nm)))
                         (require/typed #:internal (maker-name real-maker) nm lib #:struct-maker parent)

                         ;This needs to be a different identifier to meet the specifications
                         ;of struct (the id constructor shouldn't expand to it)
                         #,(if (syntax-e #'extra-maker)
                               #'(require/typed #:internal (maker-name extra-maker) nm lib #:struct-maker parent)
                               #'(begin))

                         (require/typed lib
                           [sel (nm -> ty)]) ...)))]))

  (values (rts #t) (rts #f))))

(define-syntax (do: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name rest ...) ...)
        (stop?:expr ret ...)
        c:expr ...)
     (quasisyntax/loc
         stx
       (ann #,(syntax/loc
                  stx
                (do ((var.ann-name rest ...) ...)
                    (stop? ret ...)
                  c ...))
            ty))]))

;; wrap the original for with a type annotation
(define-syntax (for/annotation stx)
  (syntax-parse stx
   [(_ x ...)
    (quasisyntax/loc stx
      (ann #,(syntax/loc stx (for x ...)) Void))]))
(define-syntax (for*/annotation stx)
  (syntax-parse stx
   [(_ x ...)
    (syntax/loc stx
      (ann (for* x ...) Void))]))

;; we need handle #:when clauses manually because we need to annotate
;; the type of each nested for
(define-syntax (for: stx)
  (syntax-parse stx #:literals (: Void)
    ;; the annotation is not necessary (always of Void type), but kept
    ;; for consistency with the other for: macros
    [(_ (~optional (~seq : Void))
        ;; c is not always an expression, could be a break-clause
        clauses c ...) ; no need to annotate the type, it's always Void
     (let ((body #`(; break-clause ...
                    #,@(type-ascription-property #'(c ...) #'Void))))
       (let loop ((clauses #'clauses))
         (define-splicing-syntax-class for-clause
           ;; single-valued seq-expr
           ;; unlike the definitions in for-clauses.rkt, this does not include
           ;; #:when clauses, which are handled separately here
           (pattern (~seq (var:optionally-annotated-name seq-expr:expr))
                    #:with (expand ...) #'((var.ann-name seq-expr)))
           ;; multi-valued seq-expr
           ;; currently disabled because it triggers an internal error in the typechecker
           ;; (pattern ((v:optionally-annotated-name ...) seq-expr:expr)
           ;;          #:with (expand ...) #'(((v.ann-name ...) seq-expr)))
           ;; break-clause, pass it directly
           ;; Note: these don't ever typecheck
           (pattern (~seq (~and kw (~or #:break #:final)) guard-expr:expr)
                    #:with (expand ...) #'(kw guard-expr)))
         (define-syntax-class for-kw
           (pattern #:when
                    #:with replace-with #'when)
           (pattern #:unless
                    #:with replace-with #'unless))
         (syntax-parse clauses
           [(head:for-clause next:for-clause ... kw:for-kw rest ...)
            (type-ascription-property
             (quasisyntax/loc stx
               (for
                (head.expand ... next.expand ... ...)
                #,(loop #'(kw rest ...))))
             #'Void)]
           [(head:for-clause ...) ; we reached the end
            (type-ascription-property
             (quasisyntax/loc stx
               (for
                (head.expand ... ...)
                #,@body))
             #'Void)]
           [(kw:for-kw guard) ; we end on a keyword clause
            (quasisyntax/loc stx
              (kw.replace-with guard
                #,@body))]
           [(kw:for-kw guard rest ...)
            (quasisyntax/loc stx
              (kw.replace-with guard
                #,(loop #'(rest ...))))])))]))

(define-for-syntax (maybe-annotate-body body ty)
  (if (syntax-e ty)
      (type-ascription-property body ty)
      body))

;; Handling #:when clauses manually, like we do with for: above breaks
;; the semantics of for/list and co.
;; We must leave it to the untyped versions of the macros.
;; However, this means that some uses of these macros with #:when
;; clauses won't typecheck.
;; If the only #:when clause is the last clause, inference should work.
(define-for-syntax (define-for-variant name)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a:optional-standalone-annotation
          clause:for-clauses
          c ...) ; c is not always an expression, can be a break-clause
       (maybe-annotate-body
        (quasisyntax/loc stx
          (#,name
           (clause.expand ... ...)
           #,@(maybe-annotate-body
               #'(c ...)
               #'a.ty)))
        #'a.ty)])))
(define-syntax (define-for-variants stx)
  (syntax-parse stx
    [(_ (name untyped-name) ...)
     (quasisyntax/loc
         stx
       (begin (define-syntax name (define-for-variant #'untyped-name)) ...))]))

;; for/vector:, for/flvector:, for/and:, for/first: and
;; for/last:'s expansions can't currently be handled by the typechecker.
(define-for-variants
  (for/list: for/list)
  (for/and: for/and)
  (for/or: for/or)
  (for/first: for/first)
  (for/last: for/last))

;; Unlike with the above, the inferencer can handle any number of #:when
;; clauses with these 2.
(define-syntax (for/lists: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name) ...)
        clause:for-clauses
        c ...) ; c is not always an expression, can be a break-clause
     (type-ascription-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      #'ty)]
    [(_ ((var:annotated-name) ...)
        clause:for-clauses
        c ...)
     (type-ascription-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      #'(values var.ty ...))]))
(define-syntax (for/fold: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name init:expr) ...)
        clause:for-clauses
        c ...) ; c is not always an expression, can be a break-clause
     (type-ascription-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand ... ...)
          c ...))
      #'ty)]
    [(_ accum:accumulator-bindings
        clause:for-clauses
        c ...)
     (type-ascription-property
      (quasisyntax/loc stx
        (for/fold ((accum.ann-name accum.init) ...)
          (clause.expand ... ...)
          c ...))
      #'(values accum.ty ...))]))


(define-syntax (for*: stx)
  (syntax-parse stx #:literals (:)
    [(_ (~seq : Void) ...
        clause:for-clauses
        c ...) ; c is not always an expression, can be a break-clause
     (quasisyntax/loc stx
       (for: (clause.expand* ... ...)
             c ...))]))

;; These currently only typecheck in very limited cases.
(define-for-syntax (define-for*-variant name)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a:optional-standalone-annotation
          clause:for-clauses
          c ...) ; c is not always an expression, can be a break-clause
       (maybe-annotate-body
        (quasisyntax/loc stx
          (#,name (clause.expand ... ...)
                  c ...))
        #'a.ty)])))
(define-syntax (define-for*-variants stx)
  (syntax-parse stx
    [(_ (name no-colon-name) ...)
     (quasisyntax/loc
         stx
       (begin (define-syntax name (define-for*-variant #'no-colon-name))
              ...))]))
(define-for*-variants
  (for*/and: for*/and)
  (for*/or: for*/or)
  (for*/first: for*/first)
  (for*/last: for*/last))

;; Like for/lists: and for/fold:, the inferencer can handle these correctly.
(define-syntax (for*/lists: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name) ...)
        clause:for-clauses
        c ...) ; c is not always an expression, can be a break-clause
     (type-ascription-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand* ... ...)
          c ...))
      #'ty)]
    [(_ ((var:annotated-name) ...)
        clause:for-clauses
        c ...)
     (type-ascription-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand* ... ...)
          c ...))
      #'(values var.ty ...))]))
(define-syntax (for*/fold: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name init:expr) ...)
        clause:for-clauses
        c ...) ; c is not always an expression, can be a break-clause
     (type-ascription-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand* ... ...)
          c ...))
      #'ty)]
    [(_ ((var:annotated-name init:expr) ...)
        clause:for-clauses
        c ...)
     (type-ascription-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand* ... ...)
          c ...))
      #'(values var.ty ...))]))

(define-for-syntax (define-for/acc:-variant for*? for/folder: for/folder op initial final)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a:optional-standalone-annotation
          clause:for-clauses
          c ...) ; c is not always an expression, can be a break-clause
       (cond
        [(syntax-e #'a.ty)
         ;; ty has to include exact 0, exact 1, null (sum/product/list respectively),
         ;; the initial value of the accumulator
         ;; (to be consistent with Racket semantics).
         ;; We can't just change the initial value to be 0.0 if we expect a
         ;; Float result. This is problematic in some cases e.g:
         ;; (for/sum: : Float ([i : Float '(1.1)] #:when (zero? (random 1))) i)
         (quasisyntax/loc stx
           (#,final
             (#,for/folder: : a.ty ([acc : a.ty #,initial])
                           (clause.expand ... ...)
                           (let ([new (let () c ...)])
                             (#,op acc new)))))]
        ;; With no annotation, try our luck with the core form.
        ;; Exact base cases cause problems, thus the additional
        ;; annotation on the accumulator above.
        [for*? ((define-for*-variant for/folder) stx)]
        [else ((define-for-variant for/folder) stx)])])))

(define-syntax (define-for/acc:-variants stx)
  (syntax-parse stx
    [(_ (name for/folder: for/folder for*? op initial final) ...)
     (quasisyntax/loc stx
       (begin (define-syntax name
                (define-for/acc:-variant
                  for*? #'for/folder: #'for/folder #'op #'initial #'final))
              ...))]))
(define-for/acc:-variants
  (for/sum: for/fold: for/sum #f + 0 #%expression)
  (for*/sum: for*/fold: for*/sum #t + 0 #%expression)
  (for*/list: for*/fold: for*/list #t (lambda (x y) (cons y x)) null reverse)
  (for/product: for/fold: for/product #f * 1 #%expression)
  (for*/product: for*/fold: for*/product #t * 1 #%expression))

(define-for-syntax (define-for/hash:-variant hash-maker)
  (lambda (stx)
    (syntax-parse stx
      #:literals (:)
      ((_ (~seq : return-annotation:expr)
          (bind:optionally-annotated-binding ...)
          body ...) ; body is not always an expression, can be a break-clause
       (quasisyntax/loc stx
         (for/fold: : return-annotation
           ((return-hash : return-annotation (ann (#,hash-maker null) return-annotation)))
           (bind ...)
           (let-values (((key val) (let () body ...)))
             (hash-set return-hash key val))))))))

(define-syntax for/hash:    (define-for/hash:-variant #'make-immutable-hash))
(define-syntax for/hasheq:  (define-for/hash:-variant #'make-immutable-hasheq))
(define-syntax for/hasheqv: (define-for/hash:-variant #'make-immutable-hasheqv))

(define-for-syntax (define-for*/hash:-variant hash-maker)
  (lambda (stx)
    (syntax-parse stx
      #:literals (:)
      ((_ (~seq : return-annotation:expr)
          (bind:optionally-annotated-binding ...)
          body ...) ; body is not always an expression, can be a break-clause
       (quasisyntax/loc stx
         (for*/fold: : return-annotation
           ((return-hash : return-annotation (ann (#,hash-maker null) return-annotation)))
           (bind ...)
           (let-values (((key val) (let () body ...)))
             (hash-set return-hash key val))))))))

(define-syntax for*/hash:    (define-for*/hash:-variant #'make-immutable-hash))
(define-syntax for*/hasheq:  (define-for*/hash:-variant #'make-immutable-hasheq))
(define-syntax for*/hasheqv: (define-for*/hash:-variant #'make-immutable-hasheqv))


(define-syntax (provide: stx)
  (syntax-parse stx
    [(_ [i:id t] ...)
     ;; indirection through i*s allows `provide: to come
     ;; before the original definitions/type annotations
     (define i*s (generate-temporaries #'(i ...)))
     (for ([i* (in-list i*s)]
           [i  (in-list (syntax->list #'(i ...)))])
       ;; lift allows `provide:` to come before original definition
       (syntax-local-lift-module-end-declaration #`(define #,i* #,i)))
     (define/with-syntax (i* ...) i*s)
     (syntax/loc stx
       (begin (: i* t) ...
              (provide (rename-out [i* i] ...))))]))

(define-syntax (declare-refinement stx)
  (syntax-parse stx
    [(_ p:id)
     (quasisyntax/loc stx #,(internal #'(declare-refinement-internal p)))]))

(define-syntaxes (let/cc: let/ec:)
  (let ()
    (define ((mk l/c) stx)
      (syntax-parse stx
       [(_ (~var k (param-annotated-name (lambda (s) #`(#,s -> (U))))) . body)
        (quasisyntax/loc stx (#,l/c k.ann-name . body))]))
    (values (mk #'let/cc) (mk #'let/ec))))


(begin-for-syntax
  (define-syntax-class optional-arg
    (pattern name:id #:attr value #f)
    (pattern (name:id value:expr)))
  (define-splicing-syntax-class lambda-args
    #:attributes (required-pos
                  optional-pos
                  optional-kws
                  required-kws)
    (pattern (~seq (~or pos:optional-arg (~seq kw:keyword key:optional-arg)) ...)
             #:attr optional-pos (length (filter values (attribute pos.value)))
             #:attr required-pos (- (length (filter values (attribute pos.name)))
                                    (attribute optional-pos))
             #:attr optional-kws
               (for/list ((kw (attribute kw))
                          (kw-value (attribute key.value))
                          #:when kw-value)
                 kw)
             #:attr required-kws (remove* (attribute optional-kws) (attribute kw)))))


;; annotation to help tc-expr pick out keyword functions
(define-syntax (-lambda stx)
  (syntax-parse stx
    [(_ formals . body)
     (define d (syntax/loc stx (λ formals . body)))
     (syntax-parse #'formals
      [(~or (~and (args:lambda-args) (~bind (rest #f)))
            (args:lambda-args . rest:id))
       (define kw-property
         (> (+ (length (attribute args.required-kws))
               (length (attribute args.optional-kws)))
            0))
       (define opt-property
         (and (> (attribute args.optional-pos) 0)
           (list
             (attribute args.required-pos)
             (attribute args.optional-pos))))
       (syntax-property
         (syntax-property d 'kw-lambda kw-property)
         'opt-lambda opt-property)]
       ;; This is an error and will be caught by the real lambda
      [_ d])]))


;; do this ourselves so that we don't get the static bindings,
;; which are harder to typecheck
(define-syntax (-define stx)
  (define-values (i b) (normalize-definition stx #'-lambda #t #t))
  (datum->syntax stx `(,#'define ,i ,b) stx stx))

(define-syntax (with-asserts stx)
  (define-syntax-class with-asserts-clause
    [pattern [x:id]
             #:with cond-clause
             (syntax/loc #'x
               [(not x)
                (error "Assertion failed")])]
    [pattern [x:id pred]
             #:with cond-clause
             (syntax/loc #'x
               [(not (pred x))
                (error "Assertion failed")])])
   (syntax-parse stx
     [(_ (c:with-asserts-clause ...) body:expr ...+)
      (syntax/loc stx
        (cond c.cond-clause
              ...
              [else body ...]))]))

(define-syntax (typecheck-fail stx)
  (syntax-parse stx
    [(_ orig msg:str #:covered-id var:id)
     #'(quote-syntax (typecheck-fail-internal orig msg var))]
    [(_ orig msg:str)
     #'(quote-syntax (typecheck-fail-internal orig msg #f))]
    [(_ orig #:covered-id var:id)
     #'(quote-syntax (typecheck-fail-internal orig "Incomplete case coverage" var))]
    [(_ orig)
     #'(quote-syntax (typecheck-fail-internal orig "Incomplete case coverage" #f))]))

(define-syntax (base-for/vector stx)
  (syntax-case stx ()
    [(name for ann T K #:length n-expr #:fill fill-expr (clauses ...) body-expr)
     (syntax/loc stx
       (call/ec
        (ann (λ (break)
               (define n n-expr)
               (define vs (ann (make-vector n fill-expr) T))
               (define i 0)
               (for (clauses ...)
                 (unsafe-vector-set! vs i body-expr)
                 (set! i (unsafe-fx+ i 1))
                 (when (i . unsafe-fx>= . n) (break vs)))
               vs)
             K)))]
    [(name for ann T K #:length n-expr (clauses ...) body-expr)
     (syntax/loc stx
       (let ([n n-expr])
         (define vs
           (call/ec
            (ann (λ (break)
                   (define vs (ann (vector) T))
                   (define i 0)
                   (for (clauses ...)
                     (define v body-expr)
                     (cond [(unsafe-fx= i 0)  (define new-vs (ann (make-vector n v) T))
                                              (set! vs new-vs)]
                           [else  (unsafe-vector-set! vs i v)])
                     (set! i (unsafe-fx+ i 1))
                     (when (i . unsafe-fx>= . n) (break vs)))
                   vs)
                 K)))
         (cond [(= (vector-length vs) n)  vs]
               [else
                ;; Only happens when n > 0 and vs = (vector)
                (raise-result-error 'name (format "~e-element vector" n) vs)])))]
    [(_ for ann T K (clauses ...) body-expr)
     (syntax/loc stx
       (let ()
         (define n 0)
         (define vs (ann (vector) T))
         (define i 0)
         (for (clauses ...)
           (define v body-expr)
           (cond [(unsafe-fx= i n)  (define new-n (max 4 (unsafe-fx* 2 n)))
                                    (define new-vs (ann (make-vector new-n v) T))
                                    (vector-copy! new-vs 0 vs)
                                    (set! n new-n)
                                    (set! vs new-vs)]
                 [else  (unsafe-vector-set! vs i v)])
           (set! i (unsafe-fx+ i 1)))
         (vector-copy vs 0 i)))]))

(define-for-syntax (base-for/vector: stx for:)
  (syntax-parse stx #:literals (:)
    [(name (~optional (~seq : T:expr))
           (~optional (~seq #:length n-expr:expr))
           (~optional (~seq #:fill fill-expr:expr))
           (clauses ...)
           (~optional (~seq : A:expr))
           body ...+)
     (let ([T  (attribute T)]
           [A  (attribute A)])
       (with-syntax ([(maybe-length ...)  (if (attribute n-expr) #'(#:length n-expr) #'())]
                     [(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())]
                     [body-expr  (if A #`(ann (let () body ...) #,A) #'(let () body ...))]
                     [T  (cond [(and T A)  #`(U #,T (Vectorof #,A))]
                               [T  T]
                               [A  #`(Vectorof #,A)]
                               [else  #'(Vectorof Any)])])
         (quasisyntax/loc stx
           (base-for/vector #,for: ann T ((T -> Nothing) -> T)
                            maybe-length ... maybe-fill ... (clauses ...) body-expr))))]))

(define-syntax (for/vector: stx)
  (base-for/vector: stx #'for:))

(define-syntax (for*/vector: stx)
  (base-for/vector: stx #'for*:))

(define-syntax (base-for/flvector: stx)
  (syntax-parse stx
    [(_ for: #:length n-expr:expr (clauses ...) body ...+)
     (syntax/loc stx
       (let: ([n : Integer  n-expr])
         (cond [(n . > . 0)
                (define xs (make-flvector n))
                (define: i : Nonnegative-Fixnum 0)
                (let/ec: break : Void
                  (for: (clauses ...)
                    (unsafe-flvector-set! xs i (let () body ...))
                    (set! i (unsafe-fx+ i 1))
                    (when (i . unsafe-fx>= . n) (break (void)))))
                xs]
               [else  (flvector)])))]
    [(_ for: (clauses ...) body ...+)
     (syntax/loc stx
       (let ()
         (define n 4)
         (define xs (make-flvector 4))
         (define i 0)
         (for: (clauses ...)
           (let: ([x : Float  (let () body ...)])
             (cond [(unsafe-fx= i n)  (define new-n (unsafe-fx* 2 n))
                                      (define new-xs (make-flvector new-n x))
                                      (let: loop : Void ([i : Nonnegative-Fixnum 0])
                                        (when (i . unsafe-fx< . n)
                                          (unsafe-flvector-set! new-xs i (unsafe-flvector-ref xs i))
                                          (loop (unsafe-fx+ i 1))))
                                      (set! n new-n)
                                      (set! xs new-xs)]
                   [else  (unsafe-flvector-set! xs i x)]))
           (set! i (unsafe-fx+ i 1)))
         (flvector-copy xs 0 i)))]))

(define-syntax-rule (for/flvector: e ...)
  (base-for/flvector: for: e ...))

(define-syntax-rule (for*/flvector: e ...)
  (base-for/flvector: for*: e ...))
