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

|#


(provide (except-out (all-defined-out) dtsi* let-internal: define-for-variants define-for*-variants 
                     with-handlers: for/annotation for*/annotation define-for/sum:-variants
                     -lambda -define)
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
         "base-types.rkt"
         "base-types-extra.rkt"
         racket/flonum ; for for/flvector and for*/flvector
         mzlib/etc
         (for-syntax
          syntax/parse
          racket/syntax
          racket/base
          syntax/define
          racket/struct-info
          syntax/struct
          "../rep/type-rep.rkt"
          "annotate-classes.rkt"
          "internal.rkt"
          "../utils/tc-utils.rkt"
          "../env/type-name-env.rkt"
          "for-clauses.rkt")
         "../types/numeric-predicates.rkt")
(provide index?) ; useful for assert, and racket doesn't have it

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
    (pattern [struct nm:opt-parent (body ...) (~var constructor (opt-constructor legacy #'nm.nm))]
             #:fail-unless (eq? 'struct (syntax-e #'struct)) #f
             #:with (constructor-parts ...) #'constructor.value))

  (define-syntax-class opaque-clause
    ;#:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [opaque ty:id pred:id]
             #:fail-unless (eq? 'opaque (syntax-e #'opaque)) #f
             #:with opt #'())
    (pattern [opaque ty:id pred:id #:name-exists]
             #:fail-unless (eq? 'opaque (syntax-e #'opaque)) #f
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


(define-syntax (define-predicate stx)
  (syntax-parse stx
    [(_ name:id ty:expr)
     #`(begin
         #,(syntax-property (if (eq? (syntax-local-context) 'top-level)
                                (let ([typ (parse-type #'ty)])
                                  #`(define name
                                      #,(type->contract
                                         typ
                                         ;; must be a flat contract
                                         #:flat #t
                                         ;; this is for a `require/typed', so the value is not from the typed side
                                         #:typed-side #f
                                         (lambda () (tc-error/stx #'ty "Type ~a could not be converted to a predicate." typ)))))
                                (syntax-property #'(define name #f)
                                                 'typechecker:flat-contract-def #'ty))
                            'typechecker:ignore #t)
         ;; not a require, this is just the unchecked declaration syntax
         #,(internal #'(require/typed-internal name (Any -> Boolean : ty))))]))

(define-syntax (:type stx)
  (error ":type is only valid at the top-level of an interaction"))
(define-syntax (:print-type stx)
  (error ":print-type is only valid at the top-level of an interaction"))
(define-syntax (:query-result-type stx)
  (error ":query-result-type is only valid at the top-level of an interaction"))

(define-syntax (require/opaque-type stx)
  (define-syntax-class name-exists-kw
    (pattern #:name-exists))
  (syntax-parse stx
    [(_ ty:id pred:id lib (~optional ne:name-exists-kw) ...)
     (register-type-name #'ty (make-Opaque #'pred (syntax-local-certifier)))
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
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (lambda: formals . body))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (pcase-lambda: stx)
  (syntax-parse stx
    [(pcase-lambda: (tvars:id ...) cl ...)
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (case-lambda: cl ...))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (popt-lambda: stx)
  (syntax-parse stx
    [(popt-lambda: (tvars:id ...) formals . body)
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
         (define: nm : type
           (plambda: (tvars ...) formals . body))))]))

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
    [(define: (tvars:id ...) (nm:id . formals:annotated-formals) : ret-ty body ...)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars ...) (formals.arg-ty ... -> ret-ty)))])
       (syntax/loc stx
         (define: nm : type
           (plambda: (tvars ...) formals body ...))))]))

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
     (syntax/loc stx (opt-lambda formals.ann-formals . body))]))

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
    [(let: nm:id ~! : ret-ty (bs:annotated-binding ...) . body)
     (syntax/loc stx ((letrec: ([nm : (bs.ty ... -> ret-ty) (lambda (bs.ann-name ...) . body)]) nm) bs.rhs ...))]
    [(let: . rest)
     (syntax/loc stx (let-internal: . rest))]))

(define-syntax (define-type-alias stx)
  (syntax-parse stx
    [(_ tname:id rest)
     #`(begin
         #,(ignore #'(define-syntax tname (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))))
         #,(internal (syntax/loc stx (define-type-alias-internal tname rest))))]
    [(_ (tname:id args:id ...) rest)
     (syntax/loc stx (define-type-alias tname (All (args ...) rest)))]))

(define-syntax (define-typed-struct/exec stx)
  (syntax-parse stx #:literals (:)
    [(_ nm ((~describe "field specification" [fld:optionally-annotated-name]) ...) [proc : proc-ty])
     (with-syntax*
      ([proc* (syntax-property #'(ann proc : proc-ty) 'typechecker:with-type #t)]
       [d-s (syntax-property (syntax/loc stx (define-struct nm (fld.name ...)
                                               #:property prop:procedure proc*))
                             'typechecker:ignore-some #t)]
       [dtsi (internal (syntax/loc stx (define-typed-struct/exec-internal nm (fld ...) proc-ty)))])
      #'(begin d-s dtsi))]))

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

(define-syntax (dtsi* stx)
  (define-syntax-class struct-name
    #:description "struct name (with optional super-struct name)"
    #:attributes (name super value)
    (pattern ((~var name (static struct-info? "struct name")) super:id)
             #:attr value (attribute name.value))
    (pattern (~var name (static struct-info? "struct name"))
             #:attr value (attribute name.value)
             #:with super #f))
  (syntax-parse stx
    [(_ () nm:struct-name . rest)
     (internal (quasisyntax/loc stx
                 (define-typed-struct-internal
                   #,(syntax-property #'nm 'struct-info (attribute nm.value)) . rest)))]
    [(_ (vars:id ...) nm:struct-name . rest)
     (internal (quasisyntax/loc stx
                 (define-typed-struct-internal (vars ...)
                   #,(syntax-property #'nm 'struct-info (attribute nm.value)) . rest)))]))

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
     (syntax/loc
         stx
       (ann (do ((var.ann-name rest ...) ...)
                (stop? ret ...)
              c ...)
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
        clauses ; no need to annotate the type, it's always Void
        c:expr ...)
     (let ((body (syntax-property #'(c ...) 'type-ascription #'Void)))
       (let loop ((clauses #'clauses))
         (define-syntax-class for-clause
           ;; single-valued seq-expr
           ;; unlike the definitions in for-clauses.rkt, this does not include
           ;; #:when clauses, which are handled separately here
           (pattern (var:optionally-annotated-name seq-expr:expr)
                    #:with expand #'(var.ann-name seq-expr))
           ;; multi-valued seq-expr
           ;; currently disabled because it triggers an internal error in the typechecker
           #;(pattern ((v:optionally-annotated-name ...) seq-expr:expr)
                    #:with expand #'((v.ann-name ...) seq-expr)))
         (syntax-parse clauses
           [(head:for-clause next:for-clause ... #:when rest ...)
            (syntax-property
             (quasisyntax/loc stx
               (for
                (head.expand next.expand ...)
                #,(loop #'(#:when rest ...))))
             'type-ascription
             #'Void)]
           [(head:for-clause ...) ; we reached the end
            (syntax-property
             (quasisyntax/loc stx
               (for
                (head.expand ...)
                #,@body))
             'type-ascription
             #'Void)]
           [(#:when guard) ; we end on a #:when clause
            (quasisyntax/loc stx
              (when guard
                #,@body))]
           [(#:when guard rest ...)
            (quasisyntax/loc stx
              (when guard
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
          c:expr ...)
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
;; for/hash{,eq,eqv}:, for/vector:, for/flvector:, for/and:, for/first: and
;; for/last:'s expansions can't currently be handled by the typechecker.
(define-for-variants
  (for/list: for/list)
  (for/hash: for/hash)
  (for/hasheq: for/hasheq)
  (for/hasheqv: for/hasheqv)
  (for/and: for/and)
  (for/or: for/or)
  (for/first: for/first)
  (for/last: for/last)
  (for/vector: for/vector)
  (for/flvector: for/flvector)
  (for/product: for/product))

;; Unlike with the above, the inferencer can handle any number of #:when
;; clauses with these 2.
(define-syntax (for/lists: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name) ...)
        (clause:for-clause ...)
        c:expr ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name) ...)
        (clause:for-clause ...)
        c:expr ...)
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
        c:expr ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name init:expr) ...)
        (clause:for-clause ...)
        c:expr ...)
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
        (clause:for*-clause ...)
        c:expr ...)
     (quasisyntax/loc stx
       (for: (clause.expand ... ...)
             c ...))]))

;; These currently only typecheck in very limited cases.
(define-for-syntax (define-for*-variant name)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a:optional-standalone-annotation
          (clause:for-clause ...)
          c:expr ...)
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
  (for*/hash: for*/hash)
  (for*/hasheq: for*/hasheq)
  (for*/hasheqv: for*/hasheqv)
  (for*/and: for*/and)
  (for*/or: for*/or)
  (for*/first: for*/first)
  (for*/last: for*/last)
  (for*/vector: for*/vector)
  (for*/flvector: for*/flvector)
  (for*/product: for*/product))

;; Like for/lists: and for/fold:, the inferencer can handle these correctly.
(define-syntax (for*/lists: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name) ...)
        (clause:for*-clause ...)
        c:expr ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name) ...)
        (clause:for*-clause ...)
        c:expr ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/lists (var.ann-name ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'(values var.ty ...))]))
(define-syntax (for*/fold: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty
        ((var:optionally-annotated-name init:expr) ...)
        (clause:for*-clause ...)
        c:expr ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'ty)]
    [(_ ((var:annotated-name init:expr) ...)
        (clause:for*-clause ...)
        c:expr ...)
     (syntax-property
      (quasisyntax/loc stx
        (for/fold ((var.ann-name init) ...)
          (clause.expand ... ...)
          c ...))
      'type-ascription
      #'(values var.ty ...))]))


(define-for-syntax (define-for/sum:-variant for/folder)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ : ty
          (clause:for-clause ...)
          c:expr ...)
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
