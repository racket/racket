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


(provide (except-out (all-defined-out) dtsi* dtsi/exec* -let-internal define-for-variants define-for*-variants
                     with-handlers: for/annotation for*/annotation define-for/acc:-variants base-for/flvector: base-for/vector
                     -lambda -define -do -let -let* -let*-values -let-values -let/cc -let/ec -letrec -letrec-values -struct)
         ;; provide the contracted bindings as primitives
         (all-from-out "base-contracted.rkt")
         (all-from-out "top-interaction.rkt")
         (all-from-out "case-lambda.rkt")
         :
         (rename-out [define-typed-struct define-struct:]
                     [define-typed-struct define-struct]
                     [-struct struct]
                     [-struct struct:]
                     [lambda: λ:]
                     [-lambda lambda]
                     [-lambda λ]
                     [-define define]
                     [-let let]
                     [-let* let*]
                     [-letrec letrec]
                     [-let-values let-values]
                     [-letrec-values letrec-values]
                     [-let/cc let/cc]
                     [-let/ec let/ec]
                     [-let let:]
                     [-let* let*:]
                     [-letrec letrec:]
                     [-let-values let-values:]
                     [-letrec-values letrec-values:]
                     [-let/cc let/cc:]
                     [-let/ec let/ec:]
                     [for: for]
                     [for/list: for/list]
                     [for/vector: for/vector]
                     [for/hash: for/hash]
                     [for/hasheq: for/hasheq]
                     [for/hasheqv: for/hasheqv]
                     [for/and: for/and]
                     [for/or: for/or]
                     [for/sum: for/sum]
                     [for/product: for/product]
                     [for/lists: for/lists]
                     [for/first: for/first]
                     [for/last: for/last]
                     [for/fold: for/fold]
                     [for*: for*]
                     [for*/list: for*/list]
                     [for*/lists: for*/lists]
                     [for*/vector: for*/vector]
                     [for*/hash: for*/hash]
                     [for*/hasheq: for*/hasheq]
                     [for*/hasheqv: for*/hasheqv]
                     [for*/and: for*/and]
                     [for*/or: for*/or]
                     [for*/sum: for*/sum]
                     [for*/product: for*/product]
                     [for*/first: for*/first]
                     [for*/last: for*/last]
                     [for*/fold: for*/fold]
                     [-do do]
                     [-do do:]
                     [with-handlers: with-handlers]
                     [define-typed-struct/exec define-struct/exec:]
                     [define-typed-struct/exec define-struct/exec]))

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
         (rename-in racket/contract/base [-> c->] [->* c->*] [case-> c:case->])
         ;; contracted bindings to replace built-in ones
         "base-contracted.rkt"
         "top-interaction.rkt"
         "base-types.rkt"
         "base-types-extra.rkt"
         "case-lambda.rkt"
         'struct-extraction
         racket/flonum ; for for/flvector and for*/flvector
         racket/extflonum ; for for/extflvector and for*/extflvector
         (for-syntax
          racket/lazy-require
          syntax/parse
          syntax/parse/experimental/template
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
          "../private/parse-classes.rkt"
          "../private/syntax-properties.rkt"
          ;"../types/utils.rkt"
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
                ["../types/utils.rkt" (fv)]
                [syntax/define (normalize-definition)]
                [typed-racket/private/parse-type (parse-type)]
                [typed-racket/private/type-contract (type->contract type->contract-fail)]
                [typed-racket/env/type-alias-env (register-resolved-type-alias)]))

(define-for-syntax (with-type* expr ty)
  (with-type #`(ann #,expr #,ty)))
(define-for-syntax (ignore-some/expr expr ty)
  #`(#,(ignore-some-expr-property #'#%expression ty) #,expr))


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
         ;; We want the value bound to name to have a nice object name. Using the built in mechanism
         ;; of define has better performance than procedure-rename.
         #,(ignore
             #'(define name
                 (let ([pred (make-predicate ty)])
                   (lambda (x) (pred x)))))
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

         #`(#,(external-check-property #'#%expression check-valid-type)
            #,(ignore-some/expr #`(flat-contract-predicate #,name) #'(Any -> Boolean : ty))))
       (let ([typ (parse-type #'ty)])
         (if (Error? typ)
             ;; This code should never get run, typechecking will have an error earlier
             #`(error 'make-predicate "Couldn't parse type")
             #`(#%expression
                #,(ignore-some/expr
                   #`(flat-contract-predicate
                      #,(type->contract
                         typ
                         ;; must be a flat contract
                         #:kind 'flat
                         ;; the value is not from the typed side
                         #:typed-side #f
                         (type->contract-fail typ #'ty #:ctc-str "predicate")))
                   #'(Any -> Boolean : ty))))))]))

(define-syntax (cast stx)
  (syntax-parse stx
    [(_ v:expr ty:expr)
     (define (apply-contract ctc-expr)
       #`(#%expression
          #,(ignore-some/expr
             #`(let-values (((val) #,(with-type* #'v #'Any)))
                 #,(syntax-property
                    (quasisyntax/loc stx
                      (contract
                       #,ctc-expr
                       val
                       'cast
                       'typed-world
                       val
                       (quote-syntax #,stx)))
                    'feature-profile:TR-dynamic-check #t))
             #'ty)))

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
              #`(#,(external-check-property #'#%expression check-valid-type)
                 #,(apply-contract ctc)))]
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
     ;; This line appears redundant with the use of `define-type-alias` below, but
     ;; it's actually necessary for top-level uses because this opaque type may appear
     ;; in subsequent `require/typed` clauses, which needs to parse the types at
     ;; expansion-time, not at typechecking time when aliases are installed.
     (register-resolved-type-alias #'ty (make-Opaque #'pred))
     (with-syntax ([hidden (generate-temporary #'pred)])
       (quasisyntax/loc stx
         (begin
           #,(ignore #'(define pred-cnt (any/c . c-> . boolean?)))
           #,(internal #'(require/typed-internal hidden (Any -> Boolean : (Opaque pred))))
           #,(if (attribute ne)
                 (internal (syntax/loc stx (define-type-alias-internal ty (Opaque pred))))
                 (syntax/loc stx (define-type-alias ty (Opaque pred))))
           #,(ignore #'(require/contract pred hidden pred-cnt lib)))))]))

(define-syntax (plambda: stx)
  (syntax-parse stx
    [(plambda: tvars:type-variables formals . body)
     (plambda-property
       (syntax/loc stx (lambda: formals . body))
       #'(tvars.vars ...)) ]))

(define-syntax (popt-lambda: stx)
  (syntax-parse stx
    [(popt-lambda: tvars:type-variables formals . body)
     (plambda-property
       (syntax/loc stx (opt-lambda: formals . body))
       #'(tvars.vars ...))]))

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
     (add-ann #'arg #'ty)]))

(define-for-syntax (add-ann expr-stx ty-stx)
  (quasisyntax/loc expr-stx
    (#,(type-ascription-property #'#%expression ty-stx)
     #,expr-stx)))


(define-syntax (inst stx)
  (syntax-parse stx #:literals (:)
    [(_ arg : . tys)
     (syntax/loc stx (inst arg . tys))]
    ;; FIXME: Is the right choice to use a #:row keyword or just
    ;; to use a Row type constructor and keep it consistent?
    [(_ arg #:row e ...)
     (with-syntax ([expr (type-inst-property #'#%expression #'(#:row e ...))])
       (syntax/loc #'arg (expr arg)))]
    [(_ arg tys ... ty ddd b:id)
     #:when (eq? (syntax-e #'ddd) '...)
     (with-syntax ([expr (type-inst-property #'#%expression #'(tys ... (ty . b)))])
       (syntax/loc #'arg (expr arg)))]
    [(_ arg tys ...)
     (with-syntax ([expr (type-inst-property #'#%expression #'(tys ...))])
       (syntax/loc #'arg (expr arg)))]))

(define-syntax (lambda: stx)
  (syntax-parse stx
    [(lambda: formals:annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntax (opt-lambda: stx)
  (syntax-parse stx
    [(opt-lambda: formals:opt-lambda-annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntaxes (-let-internal -let* -letrec)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:optionally-annotated-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let) (mk #'let*) (mk #'letrec))))

(define-syntaxes (-let-values -let*-values -letrec-values)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:optionally-annotated-values-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let-values) (mk #'let*-values) (mk #'letrec-values))))

(define-syntax (-let stx)
  (syntax-parse stx #:literals (:)
    [(-let nm:id ~! ; named let:
           (~and (~seq (~optional (~seq : ret-ty))
                       (bs:optionally-annotated-binding ...) body ...)
                 (~seq rest ...)))
     (quasisyntax/loc stx
       (#,(syntax-parse #'(rest ...)
            #:literals (:)
            [(: ret-ty (bs:annotated-binding ...) . body)
             (quasisyntax/loc stx
               (-letrec ([nm : (bs.ty ... -> ret-ty)
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
    [(-let vars:lambda-type-vars
           ([bn:optionally-annotated-name e] ...)
           . rest)
     (define/with-syntax (bn* ...)
       ;; singleton names go to just the name
       (for/list ([bn (in-syntax #'(bn ...))])
         (if (empty? (stx-cdr bn))
             (stx-car bn)
             bn)))
     (template ((-lambda (?@ . vars) (bn* ...) . rest) e ...))]
    [(-let . rest)
     (syntax/loc stx (-let-internal . rest))]))

(define-syntax (plet: stx)
  (syntax-parse stx #:literals (:)
    [(_ (A:id ...) ([bn:optionally-annotated-name e] ...) . body)
     (syntax/loc stx
       ((plambda: (A ...) (bn ...) . body) e ...))]))

(define-syntax (define-type-alias stx)
  (define-syntax-class all-vars
    #:literals (All)
    #:attributes (poly-vars)
    (pattern (All (arg:id ...) rest)
             #:with poly-vars #'(arg ...))
    (pattern type:expr #:with poly-vars #'#f))

  (define-splicing-syntax-class omit-define-syntaxes
    #:attributes (omit)
    (pattern #:omit-define-syntaxes #:attr omit #t)
    (pattern (~seq) #:attr omit #f))

  (define-splicing-syntax-class type-alias-full
     #:attributes (tname type poly-vars omit)
     (pattern (~seq tname:id (~and type:expr :all-vars) :omit-define-syntaxes))
     (pattern (~seq (tname:id arg:id ...) body:expr :omit-define-syntaxes)
       #:with poly-vars #'(arg ...)
       #:with type (syntax/loc #'body (All (arg ...) body))))

  (syntax-parse stx
    [(_ :type-alias-full)
     (define/with-syntax stx-err-fun
       #'(lambda (stx)
           (raise-syntax-error
            'type-check
            "type name used out of context"
            stx
            (and (stx-pair? stx) (stx-car stx)))))
     #`(begin
         #,(if (not (attribute omit))
               (ignore #'(define-syntax tname stx-err-fun))
               #'(begin))
         #,(internal (syntax/loc stx
                       (define-type-alias-internal tname type poly-vars))))]))

(define-syntax (with-handlers: stx)
  (syntax-parse stx
    [(_ ([pred? action] ...) . body)
     (with-syntax ([(pred?* ...)
                    (for/list ([s (in-syntax #'(pred? ...))])
                      (with-type* s #'(Any -> Any)))]
                   [(action* ...)
                    (stx-map exn-handler #'(action ...))]
                   [body* (exn-body #'(let-values () . body))])
       (exn-handlers #'(with-handlers ([pred?* action*] ...) body*)))]))

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
      ([proc* (with-type* #'proc #'proc-ty)]
       [d-s (ignore-some (syntax/loc stx (define-struct nm (fld.name ...)
                                      #:property prop:procedure proc*)))]
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
    (pattern [fld:id : ty]
             #:with form this-syntax)
    (pattern fld:id
             #:fail-when #t
             (format "field `~a' requires a type annotation"
                     (syntax-e #'fld))
             #:with form 'dummy))

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
(define-syntaxes (define-typed-struct -struct)
  (values
   (lambda (stx)
     (syntax-parse stx
       [(_ vars:maybe-type-vars nm:struct-name (fs:fld-spec ...)
           opts:struct-options)
        (let ([mutable? (if (attribute opts.mutable?) #'(#:mutable) #'())]
              [cname (second (build-struct-names #'nm.name empty #t #t))])
          (with-syntax ([d-s (ignore-some
                               (syntax/loc stx (define-struct nm (fs.fld ...) . opts)))]
                        [dtsi (quasisyntax/loc stx
                                (dtsi* (vars.vars ...) nm (fs.form ...)
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
          (with-syntax ([d-s (ignore (quasisyntax/loc stx
                                       (struct #,@(attribute nm.new-spec) (fs.fld ...)
                                               . opts)))]
                        [dtsi (quasisyntax/loc stx
                                (dtsi* (vars.vars ...)
                                       nm.old-spec (fs.form ...)
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

(define-syntax (-do stx)
  (syntax-parse stx #:literals (:)
    [(_ (~optional (~seq : ty) #:defaults ([ty #f]))
        ((var:optionally-annotated-name rest ...) ...)
        (stop?:expr ret ...)
        c:expr ...)
     (define do-stx
       (syntax/loc stx
         (do ((var.ann-name rest ...) ...)
             (stop? ret ...)
           c ...)))
     (if (attribute ty)
         (quasisyntax/loc stx
           (ann #,do-stx #,(attribute ty)))
         do-stx)]))

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
     (let ((body #'(; break-clause ...
                    c ...)))
       (let loop ((clauses #'clauses))
         (define-splicing-syntax-class for-clause
           ;; single-valued seq-expr
           ;; unlike the definitions in for-clauses.rkt, this does not include
           ;; #:when clauses, which are handled separately here
           (pattern (~seq (var:optionally-annotated-name seq-expr:expr))
                    #:with (expand ...) #'((var.ann-name seq-expr)))
           ;; multi-valued seq-expr
           (pattern ((v:optionally-annotated-formal ...) seq-expr:expr)
                    #:with (expand ...) #'(((v.ann-name ...) seq-expr)))
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
            (add-ann
             (quasisyntax/loc stx
               (for
                (head.expand ... next.expand ... ...)
                #,(loop #'(kw rest ...))))
             #'Void)]
           [(head:for-clause ...) ; we reached the end
            (add-ann
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

(begin-for-syntax
  (define-splicing-syntax-class optional-standalone-annotation*
    #:attributes (ty annotate)
    (pattern :optional-standalone-annotation
      #:attr annotate (λ (stx) (if (attribute ty)
                                   (add-ann stx #'ty)
                                   stx)))))

;; Handling #:when clauses manually, like we do with for: above breaks
;; the semantics of for/list and co.
;; We must leave it to the untyped versions of the macros.
;; However, this means that some uses of these macros with #:when
;; clauses won't typecheck.
;; If the only #:when clause is the last clause, inference should work.
(define-for-syntax (define-for-variant name)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a:optional-standalone-annotation*
          clause:for-clauses
          c ...) ; c is not always an expression, can be a break-clause
       ((attribute a.annotate)
         (quasisyntax/loc stx
           (#,name
            (clause.expand ... ...)
            c ...)))])))

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
        (var:optionally-annotated-formal ...)
        clause:for-clauses
        c ...) ; c is not always an expression, can be a break-clause
     (add-ann
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      #'ty)]
    [(_ (var:optionally-annotated-formal ...)
        clause:for-clauses
        c ...)
     (define all-typed? (andmap values (attribute var.ty)))
     (define for-stx
       (quasisyntax/loc stx
          (for/lists (var.ann-name ...)
            (clause.expand ... ...)
            c ...)))
     (if all-typed?
         (add-ann
          for-stx
          #'(values var.ty ...))
         for-stx)]))
(define-syntax (for/fold: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name init:expr) ...)
        clause:for-clauses
        c ...) ; c is not always an expression, can be a break-clause
     (add-ann
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand ... ...)
          c ...))
      #'ty)]
    [(_ accum:accumulator-bindings
        clause:for-clauses
        c ...)
     (define all-typed? (andmap values (attribute accum.ty)))
     (define for-stx
       (quasisyntax/loc stx
         (for/fold ((accum.ann-name accum.init) ...)
                   (clause.expand ... ...)
           c ...)))
     (if all-typed?
         (add-ann
          for-stx
          #'(values accum.ty ...))
         for-stx)]))

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
      [(_ a:optional-standalone-annotation*
          clause:for-clauses
          c ...) ; c is not always an expression, can be a break-clause
       ((attribute a.annotate)
        (quasisyntax/loc stx
          (#,name (clause.expand ... ...)
                  c ...)))])))
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
     (add-ann
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand* ... ...)
          c ...))
      #'ty)]
    [(_ ((var:annotated-name) ...)
        clause:for-clauses
        c ...)
     (add-ann
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
     (add-ann
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand* ... ...)
          c ...))
      #'ty)]
    [(_ ((var:annotated-name init:expr) ...)
        clause:for-clauses
        c ...)
     (add-ann
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand* ... ...)
          c ...))
      #'(values var.ty ...))]))

(define-for-syntax (define-for/acc:-variant for*? for/folder: for/folder op initial final)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a:optional-standalone-annotation*
          clause:for-clauses
          c ...) ; c is not always an expression, can be a break-clause
       (cond
        [(attribute a.ty)
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
      [(_ (~seq : return-annotation:expr)
          clause:for-clauses
          body ...) ; body is not always an expression, can be a break-clause
       (quasisyntax/loc stx
         (for/fold: : return-annotation
           ((return-hash : return-annotation (ann (#,hash-maker null) return-annotation)))
           (clause.expand ... ...)
           (let-values (((key val) (let () body ...)))
             (hash-set return-hash key val))))]
      [(_ clause:for-clauses body ...)
       (syntax/loc stx
         (for/hash (clause.expand ... ...)
           body ...))])))

(define-syntax for/hash:    (define-for/hash:-variant #'make-immutable-hash))
(define-syntax for/hasheq:  (define-for/hash:-variant #'make-immutable-hasheq))
(define-syntax for/hasheqv: (define-for/hash:-variant #'make-immutable-hasheqv))

(define-for-syntax (define-for*/hash:-variant hash-maker)
  (lambda (stx)
    (syntax-parse stx
      #:literals (:)
      ((_ (~seq : return-annotation:expr)
          clause:for-clauses
          body ...) ; body is not always an expression, can be a break-clause
       (quasisyntax/loc stx
         (for*/fold: : return-annotation
           ((return-hash : return-annotation (ann (#,hash-maker null) return-annotation)))
           (clause.expand* ... ...)
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

(define-syntaxes (-let/cc -let/ec)
  (let ()
    (define ((mk l/c) stx)
      (syntax-parse stx
       [(_ (~or (~var k (param-annotated-name (lambda (s) #`(#,s -> (U)))))
                (~and k:id (~bind [k.ann-name #'k]))) . body)
        (quasisyntax/loc stx (#,l/c k.ann-name . body))]))
    (values (mk #'let/cc) (mk #'let/ec))))


;; lambda with optional type annotations, uses syntax properties
(define-syntax (-lambda stx)
  (syntax-parse stx
    #:literals (:)
    [(_ vars:maybe-lambda-type-vars
        formals:lambda-formals
        return:return-ann
        (~describe "body expression or definition" e) ...
        (~describe "body expression" last-e))
     ;; Annotate the last expression with the return type. Should be correct
     ;; since if a function returns, it has to do so through the last expression
     ;; even with continuations.
     (define/with-syntax last-e*
       (if (attribute return.type)
           #`(ann last-e #,(attribute return.type))
           #'last-e))
     (define d (syntax/loc stx (λ formals.erased e ... last-e*)))
     (define d/prop
       (if (attribute formals.kw-property)
           (kw-lambda-property d (attribute formals.kw-property))
           (opt-lambda-property d (attribute formals.opt-property))))
     ;; attach a plambda property if necessary
     (if (attribute vars.type-vars)
         (quasisyntax/loc stx
           (#%expression
            #,(plambda-property d/prop (attribute vars.type-vars))))
         d/prop)]))

;; for backwards compatibility, note that this only accepts formals
;; with type annotations and also accepts type variables differently
;; than -define
(define-syntax (define: stx)
  (syntax-parse stx #:literals (:)
    [(define: (nm:id . formals:annotated-formals) (~describe "return type annotation" (~seq : ret-ty)) body ...)
     (with-syntax ([arrty (syntax/loc stx (formals.arg-ty ... -> ret-ty))])
       (syntax/loc stx
         (-define nm : arrty
           (-lambda formals body ...))))]
    [(define: nm:id ~! (~describe ":" :) (~describe "type" ty) body)
     (syntax/loc stx (-define nm : ty body))]
    [(define: tvars:type-variables nm:id : ty body)
     (syntax/loc stx (-define #:forall tvars nm : ty body))]
    [(define: tvars:type-variables (nm:id . formals:annotated-formals) : ret-ty body ...)
     (syntax/loc stx (-define #:forall tvars (nm . formals) : ret-ty body ...))]))

(define-syntax (-define stx)
  (syntax-parse stx #:literals (:)
    ;; the first three cases are actually subsumed by the last,
    ;; but manually expanding to using the : annotation form
    ;; produces better error messages on duplicate annotations
    [(-define nm:id body)
     (syntax/loc stx (define nm body))]
    [(-define nm:id return:return-ann body)
     (quasisyntax/loc stx
       (begin (: nm #,(attribute return.type)) (define nm body)))]
    [(-define vars:lambda-type-vars nm:id : ty body)
     (define/with-syntax type
       (syntax/loc #'ty (All vars.type-vars ty)))
     (syntax/loc stx
       (begin
         (: nm : type)
         (define nm body)))]
    [(-define vars:maybe-lambda-type-vars
              formals:curried-formals
              return:return-ann
              body ... last-body)
     ;; have to preprocess for the return type annotation
     (define/with-syntax last-body*
       (if (attribute return.type)
           #`(ann last-body #,(attribute return.type))
           #'last-body))
     (define-values (defined-id rhs)
       (normalize-definition
        #`(define formals.erased body ... last-body*)
        #'-lambda
        #t #t))
     ;; insert in type variables if necessary
     (define rhs*
       (syntax-parse rhs
         #:literals (-lambda)
         [(-lambda formals . others)
          (template (-lambda (?@ . vars) formals . others))]
         [_ rhs]))
     (quasisyntax/loc stx (define #,defined-id #,rhs*))]))

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
      (syntax-property
       (quasisyntax/loc stx
         (cond c.cond-clause
               ...
               [else #,(syntax-property
                        #'(begin body ...)
                        'feature-profile:TR-dynamic-check 'antimark)]))
       'feature-profile:TR-dynamic-check #t)]))

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
    [(_ for: Float flvector make-flvector unsafe-flvector-ref unsafe-flvector-set! flvector-copy
        #:length n-expr:expr (clauses ...) body ...+)
     (syntax/loc stx
       (-let ([n : Integer  n-expr])
         (cond [(n . > . 0)
                (define xs (make-flvector n))
                (define: i : Nonnegative-Fixnum 0)
                (-let/ec break : Void
                  (for: (clauses ...)
                    (unsafe-flvector-set! xs i (let () body ...))
                    (set! i (unsafe-fx+ i 1))
                    (when (i . unsafe-fx>= . n) (break (void)))))
                xs]
               [else  (flvector)])))]
    [(_ for: Float flvector make-flvector unsafe-flvector-ref unsafe-flvector-set! flvector-copy
        (clauses ...) body ...+)
     (syntax/loc stx
       (let ()
         (define n 4)
         (define xs (make-flvector 4))
         (define i 0)
         (for: (clauses ...)
           (-let ([x : Float  (let () body ...)])
             (cond [(unsafe-fx= i n)  (define new-n (unsafe-fx* 2 n))
                                      (define new-xs (make-flvector new-n x))
                                      (-let loop : Void ([i : Nonnegative-Fixnum 0])
                                        (when (i . unsafe-fx< . n)
                                          (unsafe-flvector-set! new-xs i (unsafe-flvector-ref xs i))
                                          (loop (unsafe-fx+ i 1))))
                                      (set! n new-n)
                                      (set! xs new-xs)]
                   [else  (unsafe-flvector-set! xs i x)]))
           (set! i (unsafe-fx+ i 1)))
         (flvector-copy xs 0 i)))]))

(define-syntax-rule (for/flvector: e ...)
  (base-for/flvector: for: Flonum flvector make-flvector unsafe-flvector-ref unsafe-flvector-set! flvector-copy e ...))

(define-syntax-rule (for*/flvector: e ...)
  (base-for/flvector: for*: Flonum flvector make-flvector unsafe-flvector-ref unsafe-flvector-set! flvector-copy e ...))

(define-syntax-rule (for/extflvector: e ...)
  (base-for/flvector: for: ExtFlonum extflvector make-extflvector unsafe-extflvector-ref unsafe-extflvector-set! extflvector-copy e ...))

(define-syntax-rule (for*/extflvector: e ...)
  (base-for/flvector: for*: ExtFlonum extflvector make-extflvector unsafe-extflvector-ref unsafe-extflvector-set! extflvector-copy e ...))
