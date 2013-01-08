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
                     with-handlers: for/annotation for*/annotation define-for/sum:-variants base-for/flvector: base-for/vector
                     -lambda -define)
         ;; provide the contracted bindings as primitives
         (all-from-out "base-contracted.rkt")
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

(require "../utils/require-contract.rkt"
         "colon.rkt"
         "../typecheck/internal-forms.rkt"
         (rename-in racket/contract/base [-> c->] [case-> c:case->])
         ;; contracted bindings to replace built-in ones
         (except-in "base-contracted.rkt" initialize-contracted)
         "base-types.rkt"
         "base-types-extra.rkt"
         racket/flonum ; for for/flvector and for*/flvector
         (for-syntax
          racket/lazy-require
          syntax/parse
          racket/syntax
          racket/base
          racket/struct-info
          syntax/struct
          "annotate-classes.rkt"
          "internal.rkt"
          "../utils/tc-utils.rkt"
          "../types/utils.rkt"
          "for-clauses.rkt")
         "../types/numeric-predicates.rkt"
         racket/unsafe/ops
         racket/vector)
(provide index?) ; useful for assert, and racket doesn't have it

(begin-for-syntax 
  (lazy-require ["../rep/type-rep.rkt" (make-Opaque Error?)]
                [syntax/define (normalize-definition)]))

(define-for-syntax (ignore stx) (syntax-property stx 'typechecker:ignore #t))

;; dynamically loaded b/c they're only used at the top-level, so we save a lot
;; of loading by not having them when we're in a module
(define-for-syntax (parse-type stx) ((dynamic-require 'typed-racket/private/parse-type 'parse-type) stx))
(define-for-syntax type->contract
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (keyword-apply
      (dynamic-require 'typed-racket/private/type-contract 'type->contract)
      kws kw-args rest))))

(define-syntaxes (require/typed-legacy require/typed)
 (let ()
  (define-syntax-class opt-rename
    #:attributes (nm spec)
    (pattern nm:id
             #:with spec #'nm)
    (pattern (orig-nm:id internal-nm:id)
             #:with spec #'(orig-nm internal-nm)
             #:with nm #'internal-nm))

  (define-syntax-class opt-parent
    #:attributes (nm parent)
    (pattern nm:id
             #:with parent #'#f)
    (pattern (nm:id parent:id)))

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
       (unless (< 0 (length (syntax->list #'(c ...))))
         (raise-syntax-error #f "at least one specification is required" stx))
       #`(begin c.spec ...)]
      [(_ nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
       #`(require/typed #:internal nm ty lib #,@(if (attribute parent)
                                                    #'(#:struct-maker parent)
                                                    #'()))]
      [(_ #:internal nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
       (with-syntax ([cnt* (generate-temporary #'nm.nm)]
                     [hidden (generate-temporary #'nm.nm)]
                     [sm (if (attribute parent)
                             #'(#:struct-maker parent)
                             #'())])
         (let ([prop-name (if (attribute parent)
                              'typechecker:contract-def/maker
                              'typechecker:contract-def)])
           (quasisyntax/loc stx
             (begin
               #,(syntax-property (if (eq? (syntax-local-context) 'top-level)
                                      (let ([typ (parse-type #'ty)])
                                        #`(define cnt*
                                            #,(type->contract
                                               typ
                                               ;; this is for a `require/typed', so the value is not from the typed side
                                               #:typed-side #f
                                               (lambda () (tc-error/stx #'ty "Type ~a could not be converted to a contract." typ)))))
                                      (syntax-property #'(define cnt* #f)
                                                   prop-name #'ty))
                                  'typechecker:ignore #t)
               #,(internal #'(require/typed-internal hidden ty . sm))
               #,(syntax-property #'(require/contract nm.spec hidden cnt* lib)
                                  'typechecker:ignore #t)))))]))
  (values (r/t-maker #t) (r/t-maker #f))))

(define-syntax-rule (require/typed/provide lib [nm t] ...)
  (begin
    (require/typed lib [nm t] ...)
    (provide nm ...)))

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
         #,(syntax-property (if (eq? (syntax-local-context) 'top-level)
                                #'(define name (procedure-rename (make-predicate ty) 'name))
                                (syntax-property #'(define name #f)
                                                 'typechecker:flat-contract-def #'ty))
                            'typechecker:ignore #t)
         ;; not a require, this is just the unchecked declaration syntax
         #,(internal #'(require/typed-internal name (Any -> Boolean : ty))))]))

(define-syntax (make-predicate stx)
  (syntax-parse stx
    [(_ ty:expr)
     (if (syntax-transforming-module-expression?)
       (let ((name (syntax-local-lift-expression
                     (syntax-property #'#f 'typechecker:flat-contract-def #'ty))))
         (define (check-valid-type _)
           (define type (parse-type #'ty))
           (define vars (fv type))
           ;; If there was an error don't create another one
           (unless (or (Error? type) (null? vars))
             (tc-error/delayed
               "Type ~a could not be converted to a predicate because it contains free variables."
               type)))

         #`(ann
             #,(syntax-property
                 (syntax-property name 'typechecker:ignore-some #t)
                 'typechecker:external-check check-valid-type)

             (Any -> Boolean : ty)))
       (let ([typ (parse-type #'ty)])
         (if (Error? typ)
             ;; This code should never get run, typechecking will have an error earlier
             #`(error 'make-predicate "Couldn't parse type")
             #`(ann
               #,(syntax-property
                   (type->contract
                     typ
                     ;; must be a flat contract
                     #:kind 'flat
                     ;; the value is not from the typed side
                     #:typed-side #f
                     (lambda () (tc-error/stx #'ty "Type ~a could not be converted to a predicate." typ)))
                    'typechecker:ignore-some #t)
               (Any -> Boolean : ty)))))]))

(define-syntax (cast stx)
  (syntax-parse stx
    [(_ v:expr ty:expr)
     (define (apply-contract ctc-expr)
       #`(#%expression
           (ann
             #,(syntax-property
                 #`(let-values (((val) #,(syntax-property #'(ann v Any) 'with-type #t)))
                     (contract
                       #,ctc-expr
                       val
                       'cast
                       'typed-world
                       val
                       (quote-syntax #,stx)))
                   'typechecker:ignore-some #t)
             ty)))

     (if (syntax-transforming-module-expression?)
         (let ((ctc (syntax-local-lift-expression
                       (syntax-property #'#f 'typechecker:contract-def #'ty))))
           (define (check-valid-type _)
             (define type (parse-type #'ty))
             (define vars (fv type))
             ;; If there was an error don't create another one
             (unless (or (Error? type) (null? vars))
               (tc-error/delayed
                 "Type ~a could not be converted to a contract because it contains free variables."
                 type)))
             (syntax-property (apply-contract ctc)
                              'typechecker:external-check check-valid-type))
         (let ([typ (parse-type #'ty)])
           (if (Error? typ)
               ;; This code should never get run, typechecking will have an error earlier
               #`(error 'cast "Couldn't parse type")
               (apply-contract
                 (type->contract
                   typ
                   ;; the value is not from the typed side
                   #:typed-side #f
                   (lambda ()
                     (tc-error/stx #'ty "Type ~a could not be converted to a contract" typ)))))))]))


(define-syntax (:type stx)
  (error ":type is only valid at the top-level of an interaction"))
(define-syntax (:print-type stx)
  (error ":print-type is only valid at the top-level of an interaction"))
(define-syntax (:query-type/args stx)
  (error ":query-type/args is only valid at the top-level of an interaction"))
(define-syntax (:query-type/result stx)
  (error ":query-type/result is only valid at the top-level of an interaction"))

(define-syntax (require/opaque-type stx)
  (define-syntax-class name-exists-kw
    (pattern #:name-exists))
  (syntax-parse stx
    [(_ ty:id pred:id lib (~optional ne:name-exists-kw) ...)
     ((dynamic-require 'typed-racket/env/type-name-env 'register-type-name)
      #'ty (make-Opaque #'pred (syntax-local-certifier)))
     (with-syntax ([hidden (generate-temporary #'pred)])
       (quasisyntax/loc stx
         (begin
           #,(syntax-property #'(define pred-cnt (any/c . c-> . boolean?))
                              'typechecker:ignore #t)
           #,(internal #'(require/typed-internal hidden (Any -> Boolean : (Opaque pred))))
           #,(if (attribute ne)
                 (internal (syntax/loc stx (define-type-alias-internal ty (Opaque pred))))
                 (syntax/loc stx (define-type-alias ty (Opaque pred))))
           #,(syntax-property #'(require/contract pred hidden pred-cnt lib)
                              'typechecker:ignore #t))))]))

(define-syntax (plambda: stx)
  (syntax-parse stx
    [(plambda: (tvars:id ...) formals . body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(tvars ...)))
                 "duplicate type variable declaration"
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (lambda: formals . body))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (pcase-lambda: stx)
  (syntax-parse stx
    [(pcase-lambda: (tvars:id ...) cl ...)
     #:fail-when (check-duplicate-identifier (syntax->list #'(tvars ...)))
                 "duplicate type variable declaration"
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (case-lambda: cl ...))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (popt-lambda: stx)
  (syntax-parse stx
    [(popt-lambda: (tvars:id ...) formals . body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(tvars ...)))
                 "duplicate type variable declaration"
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (opt-lambda: formals . body))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (pdefine: stx)
  (syntax-parse stx #:literals (:)
    [(pdefine: (tvars:id ...) (nm:id . formals:annotated-formals) : ret-ty . body)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars ...) (formals.arg-ty ... -> ret-ty)))])
       (syntax/loc stx
         (begin
          (: nm : type)
          (define (nm . formals.ann-formals) . body))))]))

(define-syntax (ann stx)
  (syntax-parse stx #:literals (:)
    [(_ (~or (~seq arg : ty) (~seq arg ty)))
     (syntax-property #'arg 'type-ascription #'ty)]))

(define-syntax (inst stx)
  (syntax-parse stx #:literals (:)
    [(_ arg : . tys)
     (syntax/loc stx (inst arg . tys))]
    [(_ arg tys ... ty ddd b:id)
     #:when (eq? (syntax-e #'ddd) '...)
     (syntax-property #'arg 'type-inst #'(tys ... (ty . b)))]
    [(_ arg tys ...)
     (syntax-property #'arg 'type-inst #'(tys ...))]))

(define-syntax (define: stx)
  (syntax-parse stx #:literals (:)
    [(define: (nm:id . formals:annotated-formals) (~describe "return type annotation" (~seq : ret-ty)) body ...)
     (with-syntax ([arrty (syntax/loc stx (formals.arg-ty ... -> ret-ty))])
       (syntax/loc stx
         (define: nm : arrty
           (lambda: formals body ...))))]
    [(define: nm:id ~! (~describe ":" :) (~describe "type" ty) body)
     (identifier? #'nm)
     (with-syntax ([new-nm (syntax-property #'nm 'type-label #'ty)])
       (syntax/loc stx (define new-nm body)))]
    [(define: (tvars:id ...) nm:id : ty body)
     (with-syntax ([type (syntax/loc #'ty (All (tvars ...) ty))])
       (syntax/loc stx
         (begin
           (: nm : type)
           (define nm body))))]
    [(define: (tvars:id ...) (nm:id . formals:annotated-formals) : ret-ty body ...)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars ...) (formals.arg-ty ... -> ret-ty)))])
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
           (~and (~seq rest ...)
                 (~seq (~optional (~seq : ret-ty))
                       (bs:optionally-annotated-binding ...) body ...)))
     (quasisyntax/loc stx
       (#,(syntax-parse #'(rest ...)
            #:literals (:)
            [(: ret-ty (bs:annotated-binding ...) . body)
             (quasisyntax/loc stx
               (letrec: ([nm : (bs.ty ... -> ret-ty)
                             #,(quasisyntax/loc stx
                                 (lambda (bs.ann-name ...) . #,(syntax/loc stx body)))])
                        #,(quasisyntax/loc stx nm)))]
            [(: ret-ty (bs:optionally-annotated-binding ...) . body)
             (quasisyntax/loc stx
               (letrec ([nm #,(quasisyntax/loc stx
                                (lambda (bs.ann-name ...) . (ann #,(syntax/loc stx body) ret-ty)))])
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
    [(_ tname:id rest)
     #`(begin
         #,(ignore #'(define-syntax tname (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))))
         #,(internal (syntax/loc stx (define-type-alias-internal tname rest))))]
    [(_ (tname:id args:id ...) rest)
     (syntax/loc stx (define-type-alias tname (All (args ...) rest)))]))

(define-syntax (with-handlers: stx)
  (syntax-parse stx
    [(_ ([pred? action] ...) . body)
     (with-syntax ([(pred?* ...) (map (lambda (s) (syntax-property #`(ann #,s : (Any -> Any)) 'typechecker:with-type #t))
                                      (syntax->list #'(pred? ...)))]
                   [(action* ...)
                    (map (lambda (s) (syntax-property s 'typechecker:exn-handler #t)) (syntax->list #'(action ...)))]
                   [body* (syntax-property #'(let-values () . body) 'typechecker:exn-body #t)])
       (syntax-property #'(with-handlers ([pred?* action*] ...) body*)
                        'typechecker:with-handlers
                        #t))]))

(begin-for-syntax
  (define-syntax-class struct-name
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
      ([proc* (syntax-property #'(ann proc : proc-ty) 'typechecker:with-type #t)]
       [d-s (syntax-property (syntax/loc stx (define-struct nm (fld.name ...)
                                               #:property prop:procedure proc*))
                             'typechecker:ignore-some #t)]
       [dtsi (quasisyntax/loc stx (dtsi/exec* () nm (fld ...) proc-ty))])
      #'(begin d-s dtsi))]))

(define-syntaxes (dtsi* dtsi/exec*)
  (let ()
    (define (mk internal-id)
      (lambda (stx)
        (syntax-parse stx
          [(_ () nm:struct-name . rest)
           (internal (quasisyntax/loc stx
                       (#,internal-id
                        #,(syntax-property #'nm 'struct-info (attribute nm.value)) . rest)))]
          [(_ (vars:id ...) nm:struct-name . rest)
           (internal (quasisyntax/loc stx
                       (#,internal-id (vars ...)
                                      #,(syntax-property #'nm 'struct-info (attribute nm.value)) . rest)))])))
    (values (mk #'define-typed-struct-internal)
            (mk #'define-typed-struct/exec-internal))))
              
  
(define-syntaxes (define-typed-struct struct:)
  (let ()
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


    (define (mutable? opts)
      (if (memq '#:mutable (syntax->datum opts)) '(#:mutable) '()))
    (values
     (lambda (stx)
       (syntax-parse stx
         [(_ vars:maybe-type-vars nm:struct-name (fs:fld-spec ...) . opts)
          (let ([mutable (mutable? #'opts)])
            (with-syntax ([d-s (syntax-property (syntax/loc stx (define-struct nm (fs.fld ...) . opts))
                                                'typechecker:ignore #t)]
                          [dtsi (quasisyntax/loc stx (dtsi* (vars.vars ...) nm (fs ...) #,@mutable))])
              #'(begin d-s dtsi)))]))
     (lambda (stx)
       (syntax-parse stx
         [(_ vars:maybe-type-vars nm:struct-name/new (fs:fld-spec ...) . opts)
          (let ([mutable (mutable? #'opts)]
                [cname (datum->syntax #f (format-symbol "make-~a" (syntax-e #'nm.name)))])
            (with-syntax ([d-s (syntax-property (quasisyntax/loc stx
                                                  (struct #,@(attribute nm.new-spec) (fs.fld ...)
                                                          #:extra-constructor-name #,cname
                                                          . opts))
                                                'typechecker:ignore #t)]
                          [dtsi (quasisyntax/loc stx (dtsi* (vars.vars ...) nm.old-spec (fs ...) #:maker #,cname #,@mutable))])
              #'(begin d-s dtsi)))])))))


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
  (define-syntax-class opt-parent
    (pattern nm:id #:attr parent #'#f)
    (pattern (nm:id parent:id)))

  (define-splicing-syntax-class (constructor-term legacy struct-name)
   (pattern (~seq) #:fail-when legacy #f #:attr name struct-name #:attr extra #f)
   (pattern (~seq) #:fail-unless legacy #f #:attr name (format-id struct-name "make-~a" struct-name) #:attr extra #t)
   (pattern (~seq #:constructor-name name:id) #:attr extra #f)
   (pattern (~seq #:extra-constructor-name name:id) #:attr extra #t))

  (define (maybe-add-quote-syntax stx)
   (if (and stx (syntax-e stx)) #`(quote-syntax #,stx) stx))

  (define ((rts legacy) stx)
    (syntax-parse stx #:literals (:)
      [(_ name:opt-parent ([fld : ty] ...) (~var input-maker (constructor-term legacy #'name.nm)) lib)
       (with-syntax* ([nm #'name.nm]
                      [parent #'name.parent]
                      [hidden (generate-temporary #'name.nm)]
                      [orig-struct-info (generate-temporary #'nm)]
                      [spec (if (syntax-e #'name.parent) #'(nm parent) #'nm)]
                      [num-fields (length (syntax->list #'(fld ...)))]
                      [(type-des _ pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                      [(mut ...) (map (lambda _ #'#f) (syntax->list #'(sel ...)))]
                      [maker-name #'input-maker.name]
                      ;maker-name's symbolic form is used in the require form
                      [id-is-ctor? (or (attribute input-maker.extra) (bound-identifier=? #'maker-name #'nm))]
                      [internal-maker (generate-temporary #'maker-name)] ;Only used if id-is-ctor? is true
                      [real-maker (if (syntax-e #'id-is-ctor?) #'internal-maker #'maker-name)] ;The actual identifier bound to the constructor
                      [extra-maker (and (attribute input-maker.extra)
                                        (not (bound-identifier=? #'make-name #'nm))
                                        #'maker-name)])
                     (quasisyntax/loc stx
                       (begin
                         (require (only-in lib type-des (nm orig-struct-info)))

                         (define-for-syntax si
                           (let ()
                            (define-values (orig-type-des orig-maker orig-pred orig-sels orig-muts orig-parent)
                             (apply values (extract-struct-info (syntax-local-value (quote-syntax orig-struct-info)))))

                            (define (id-drop sels muts num)
                             (cond
                              ((zero? num) (values sels muts))
                              ((null? sels) (int-err "id-drop: Too short of list"))
                              ((pair? sels)
                               (cond
                                ((not (car sels)) (values sels muts))
                                (else (id-drop (cdr sels) (cdr muts) (sub1 num)))))
                              (else (int-err "id-drop: Not a list"))))

                           (make-struct-info
                            (lambda ()
                              #,(if (syntax-e #'parent)
                                    (let-values (((parent-type-des parent-maker parent-pred
                                                   parent-sel  parent-mut grand-parent)
                                                  (apply values (extract-struct-info (syntax-local-value #'parent)))))
                                      #`(list (quote-syntax type-des)
                                              (quote-syntax real-maker)
                                              (quote-syntax pred)
                                              (list #,@(map maybe-add-quote-syntax (append (reverse (syntax->list #'(sel ...))) parent-sel)))
                                              (list #,@(map maybe-add-quote-syntax (append (reverse (syntax->list #'(mut ...))) parent-mut)))
                                              (quote-syntax parent)))
                                    #`(let-values (((new-sels new-muts) (id-drop orig-sels orig-muts num-fields)))
                                        (list (quote-syntax type-des)
                                              (quote-syntax real-maker)
                                              (quote-syntax pred)
                                              (list* #,@(map maybe-add-quote-syntax (reverse (syntax->list #'(sel ...)))) new-sels)
                                              (list* #,@(map maybe-add-quote-syntax (reverse (syntax->list #'(mut ...)))) new-muts)
                                              orig-parent)))))))

                         (define-syntax nm
                              (if id-is-ctor?
                                  (make-struct-info-self-ctor #'internal-maker si)
                                  si))

                         (dtsi* () spec ([fld : ty] ...) #:maker maker-name #:type-only)
                         #,(ignore #'(require/contract pred hidden (any/c . c-> . boolean?) lib))
                         #,(internal #'(require/typed-internal hidden (Any -> Boolean : nm)))
                         (require/typed (maker-name real-maker) nm lib #:struct-maker parent)

                         ;This needs to be a different identifier to meet the specifications
                         ;of struct (the id constructor shouldn't expand to it)
                         #,(if (syntax-e #'extra-maker)
                               #'(require/typed (maker-name extra-maker) nm lib #:struct-maker parent)
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
    (syntax/loc stx
      (ann (for x ...) Void))]))
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
    [(_ (~seq : Void) ...
        ;; c is not always an expression, could be a break-clause
        clauses c ...) ; no need to annotate the type, it's always Void
     (let ((body #`(; break-clause ...
                    #,@(syntax-property #'(c ...) 'type-ascription #'Void))))
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
            (syntax-property
             (quasisyntax/loc stx
               (for
                (head.expand ... next.expand ... ...)
                #,(loop #'(kw rest ...))))
             'type-ascription
             #'Void)]
           [(head:for-clause ...) ; we reached the end
            (syntax-property
             (quasisyntax/loc stx
               (for
                (head.expand ... ...)
                #,@body))
             'type-ascription
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
      (syntax-property body 'type-ascription ty)
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
          (clause:for-clause ...)
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
  (for/last: for/last)
  (for/product: for/product))

;; Unlike with the above, the inferencer can handle any number of #:when
;; clauses with these 2.
(define-syntax (for/lists: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name) ...)
        (clause:for-clause ...)
        c ...) ; c is not always an expression, can be a break-clause
     (syntax-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name) ...)
        (clause:for-clause ...)
        c ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'(values var.ty ...))]))
(define-syntax (for/fold: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name init:expr) ...)
        (clause:for-clause ...)
        c ...) ; c is not always an expression, can be a break-clause
     (syntax-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name init:expr) ...)
        (clause:for-clause ...)
        c ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'(values var.ty ...))]))


(define-syntax (for*: stx)
  (syntax-parse stx #:literals (:)
    [(_ (~seq : Void) ...
        (clause:for-clause ...)
        c ...) ; c is not always an expression, can be a break-clause
     (quasisyntax/loc stx
       (for: (clause.expand* ... ...)
             c ...))]))

;; These currently only typecheck in very limited cases.
(define-for-syntax (define-for*-variant name)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a:optional-standalone-annotation
          (clause:for-clause ...)
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
  (for*/list: for*/list)
  (for*/and: for*/and)
  (for*/or: for*/or)
  (for*/first: for*/first)
  (for*/last: for*/last)
  (for*/product: for*/product))

;; Like for/lists: and for/fold:, the inferencer can handle these correctly.
(define-syntax (for*/lists: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name) ...)
        (clause:for-clause ...)
        c ...) ; c is not always an expression, can be a break-clause
     (syntax-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand* ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name) ...)
        (clause:for-clause ...)
        c ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand* ... ...)
          c ...))
      'type-ascription
      #'(values var.ty ...))]))
(define-syntax (for*/fold: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name init:expr) ...)
        (clause:for-clause ...)
        c ...) ; c is not always an expression, can be a break-clause
     (syntax-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand* ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name init:expr) ...)
        (clause:for-clause ...)
        c ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand* ... ...)
          c ...))
      'type-ascription
      #'(values var.ty ...))]))


(define-for-syntax (define-for/sum:-variant for/folder)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ : ty
          (clause:for-clause ...)
          c ...) ; c is not always an expression, can be a break-clause
       ;; ty has to include exact 0, the initial value of the accumulator
       ;; (to be consistent with Racket semantics).
       ;; We can't just change the initial value to be 0.0 if we expect a
       ;; Float result. This is problematic in some cases e.g:
       ;; (for/sum: : Float ([i : Float '(1.1)] #:when (zero? (random 1))) i)
       (quasisyntax/loc stx
         (#,for/folder : ty ([acc : ty 0])
                       (clause.expand ... ...)
                       (let ([new (let () c ...)])
                         (+ acc new))))])))
(define-syntax (define-for/sum:-variants stx)
  (syntax-parse stx
    [(_ (name for/folder) ...)
     (quasisyntax/loc stx
       (begin (define-syntax name (define-for/sum:-variant #'for/folder))
              ...))]))
(define-for/sum:-variants (for/sum: for/fold:) (for*/sum: for*/fold:))

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
     (syntax/loc stx
       (begin (: i t) ...
              (provide i ...)))]))

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

;; annotation to help tc-expr pick out keyword functions
(define-syntax (-lambda stx)
  (syntax-parse stx
    [(_ formals . body)
     (define d (datum->syntax stx `(,#'λ ,#'formals . ,#'body)
                              stx stx))
     (syntax-property d 'kw-lambda #t)]))

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
