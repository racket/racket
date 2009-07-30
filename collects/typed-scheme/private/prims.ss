#lang scheme/base

#|

This file defines two sorts of primitives. All of them are provided into any module using the typed scheme language.

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


(provide (all-defined-out)
	 (rename-out [define-typed-struct define-struct:]))

(require (except-in "../utils/utils.ss" extend))
(require (for-syntax 
          stxclass
	  stxclass/util
          scheme/base
          (rep type-rep)
          mzlib/match
          "parse-type.ss"
          syntax/struct
          syntax/stx
          scheme/struct-info
	  (except-in (utils utils tc-utils))
          (env type-name-env)
          "type-contract.ss"))

(require (utils require-contract)
         (typecheck internal-forms)
         (except-in mzlib/contract ->)
         (only-in mzlib/contract [-> c->])
         mzlib/struct
         "base-types.ss")

(define-for-syntax (ignore stx) (syntax-property stx 'typechecker:ignore #t))

(define-for-syntax (internal stx)
  (quasisyntax/loc stx
    (define-values ()
      (begin
        (quote-syntax #,stx)
        (#%plain-app values)))))



(define-syntax (require/typed stx)
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
  (define-syntax-class struct-clause
    #:literals (struct)
    #:attributes (nm (body 1))
    (pattern [struct nm:opt-rename (body ...)]))
  (define-syntax-class opaque-clause
    #:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [opaque ty:id pred:id]
             #:with opt #'())
    (pattern [opaque ty:id pred:id #:name-exists]
             #:with opt #'(#:name-exists)))
  (syntax-parse stx
    [(_ lib (~or [sc:simple-clause] [strc:struct-clause] [oc:opaque-clause]) ...)
     #'(begin 
	 (require/opaque-type oc.ty oc.pred lib . oc.opt) ...
	 (require/typed sc.nm sc.ty lib) ... 
	 (require-typed-struct strc.nm (strc.body ...) lib) ...)]
    [(_ nm:opt-rename ty lib (~or [#:struct-maker parent] #:opt) ...)
     (with-syntax ([cnt* (generate-temporary #'nm.nm)]
		   [sm (if #'parent
                           #'(#:struct-maker parent)
                           #'())])
       (let ([prop-name (if #'parent
                            'typechecker:contract-def/maker
                            'typechecker:contract-def)])
         (quasisyntax/loc stx 
           (begin 
             #,(syntax-property (syntax-property #'(define cnt #f)
                                                 prop-name #'ty)
                                'typechecker:ignore #t)
             #,(internal #'(require/typed-internal nm.nm ty . sm))
             #,(syntax-property #'(require/contract nm.spec cnt lib)
                                'typechecker:ignore #t)))))]))

(define-syntax (require/opaque-type stx)
  (define-syntax-class name-exists-kw
    (pattern #:name-exists))
  (syntax-parse stx
    [(_ ty:id pred:id lib (~or [ne:name-exists-kw] #:opt) ...)
     (register-type-name #'ty (make-Opaque #'pred (syntax-local-certifier)))
     (quasisyntax/loc stx
       (begin 
         #,(syntax-property #'(define pred-cnt (any/c . c-> . boolean?))
                            'typechecker:ignore #t)
         #,(internal #'(require/typed-internal pred (Any -> Boolean : (Opaque pred))))
         #,(if #'ne
               (internal (syntax/loc stx (define-type-alias-internal ty (Opaque pred))))
               (syntax/loc stx (define-type-alias ty (Opaque pred))))
         #,(syntax-property #'(require/contract pred pred-cnt lib)
                            'typechecker:ignore #t)))]))

(define-for-syntax (formal-annotation-error stx src)
  (let loop ([stx stx])
    (syntax-case stx ()
      ;; should never happen
      [() (raise-syntax-error #f "bad annotation syntax" src stx)]
      [[var : ty]
       (identifier? #'var)
       (raise-syntax-error #f "expected dotted or starred type" src #'ty)]
      [([var : ty] . rest)
       (identifier? #'var)
       (loop #'rest)]
      [([var : ty] . rest)
       (raise-syntax-error #f "not a variable" src #'var)]
      [(e . rest)
       (raise-syntax-error #f "expected annotated variable of the form [x : T], got something else" src #'e)])))

(define-for-syntax (types-of-formals stx src)
  (syntax-case stx (:)
    [([var : ty] ...) (quasisyntax/loc stx (ty ...))]
    [([var : ty] ... . [rest : rest-ty star])
     (eq? '* (syntax-e #'star))
     (syntax/loc stx (ty ... rest-ty star))]
    [([var : ty] ... . [rest : rest-ty ddd bound])
     (eq? '... (syntax-e #'ddd))
     (syntax/loc stx (ty ... rest-ty ddd bound))]
    [_ (formal-annotation-error stx src)]))


(define-syntax (plambda: stx)
  (syntax-case stx ()
    [(plambda: (tvars ...) formals . body)
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (lambda: formals . body))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (pcase-lambda: stx)
  (syntax-case stx ()
    [(pcase-lambda: (tvars ...) cl ...)
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (case-lambda: cl ...))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (pdefine: stx)
  (syntax-case stx (:)
    [(pdefine: tvars (nm . formals) : ret-ty . body)
     (with-syntax* ([(tys ...) (types-of-formals #'formals stx)]
                    [type (syntax/loc #'ret-ty (All tvars (tys ... -> ret-ty)))])
                   (syntax/loc stx
                     (define: nm : type
                       (plambda: tvars formals . body))))]))

(define-syntax (ann stx)
  (syntax-case stx (:)
    [(_ arg : ty)
     (syntax-property #'arg 'type-ascription #'ty)]
    [(_ arg ty)
     (syntax-property #'arg 'type-ascription #'ty)]))

(define-syntax (: stx)
  (define stx*
    ;; make it possible to add another colon after the id for clarity
    ;; and in that case, a `->' on the RHS does not need to be
    ;; explicitly parenthesized
    (syntax-case stx (:)
      [(: id : x ...)
       (ormap (lambda (x) (eq? '-> (syntax-e x))) (syntax->list #'(x ...)))
       (syntax/loc stx (: id (x ...)))]
      [(: id : . more) (syntax/loc stx (: id . more))]
      [_ stx]))
  (define (err str . sub)
    (apply raise-syntax-error '|type declaration| str stx sub))
  (syntax-case stx* ()
    [(_ id ty)
     (identifier? #'id)
     (syntax-property (internal (syntax/loc stx (:-internal id ty)))
                      'disappeared-use #'id)]
    [(_ id x ...)
     (case (length (syntax->list #'(x ...)))
       [(1)  (err "can only annotate identifiers with types" #'id)]
       [(0)  (err "missing type")]
       [else (err "bad syntax (multiple types after identifier)")])]))

(define-syntax (inst stx)
  (syntax-case stx (:)
    [(_ arg : . tys)
     (syntax/loc stx (inst arg . tys))]    
    [(_ arg tys ... ty ddd b)
     (eq? (syntax-e #'ddd) '...)
     (syntax-property #'arg 'type-inst #'(tys ... (ty . b)))]
    [(_ arg tys ...)
     (syntax-property #'arg 'type-inst #'(tys ...))]))

(define-syntax (define: stx)
  (syntax-case stx (:)
    [(define: (nm . formals) : ret-ty body ...)
     (identifier? #'nm)
     (with-syntax* ([(tys ...) (types-of-formals #'formals stx)]
                    [arrty (syntax/loc stx (tys ... -> ret-ty))])
                   (syntax/loc stx
                     (define: nm : arrty
                       (lambda: formals body ...))))]
    [(define: nm : ty body)
     (identifier? #'nm)
     (with-syntax ([new-nm (syntax-property #'nm 'type-label #'ty)])
       (syntax/loc stx (define new-nm body)))]
    [(define: (vars ...) (f args ...) : ret body ...)
     (andmap identifier? (syntax->list #'(vars ...)))
     #'(pdefine: (vars ...) (f args ...) : ret body ...)]
    [(define: (nm . formals) body ...)
     (raise-syntax-error #f "missing return type annotation" stx)]
    [(define: nm body)
     (raise-syntax-error #f "missing type annotation" stx)]))


;; helper function for annoating the bound names
(define-for-syntax (annotate-names stx src)
  (define (label-one var ty)
    (syntax-property var 'type-label ty))
  (define (label vars tys)
    (map label-one
         (syntax->list vars)
         (syntax->list tys)))
  (define (label-dotted var ty bound)
    (syntax-property (syntax-property var 'type-ascription ty)
                      'type-dotted 
                      bound))
  (syntax-case stx (:)
    [([var : ty] ...)
     (label #'(var ...) #'(ty ...))]
    [([var : ty] ... . [rest : rest-ty star])
     (eq? '* (syntax-e #'star))
     (append (label #'(var ...) #'(ty ...)) (label-one #'rest #'rest-ty))]
    [([var : ty] ... . [rest : rest-ty ddd bound])
     (eq? '... (syntax-e #'ddd))
     (append (label #'(var ...) #'(ty ...)) (label-dotted #'rest #'rest-ty #'bound))]
    [_ (formal-annotation-error stx src)]))

(define-syntax-rule (λ: . args) (lambda: . args))

(define-syntax (lambda: stx)
  (syntax-case stx (:)
    [(lambda: formals . body)
     (with-syntax ([labeled-formals (annotate-names #'formals stx)])
       (syntax/loc stx (lambda labeled-formals . body)))]))

(define-syntax (case-lambda: stx)
  (syntax-case stx (:)
    [(case-lambda: [formals . body] ...)
     (with-syntax ([(lab-formals ...) (map (lambda (s) (annotate-names s stx))
                                           (syntax->list #'(formals ...)))])
       (syntax/loc stx (case-lambda [lab-formals . body] ...)))]))

(define-syntaxes (let-internal: let*: letrec:)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-case stx (:)
                  [(_ ([nm : ty . exprs] ...) . body)
                   (with-syntax* ([(vars ...) (annotate-names #'([nm : ty] ...) stx)]
                                  [bindings (map (lambda (v e loc)
                                                   (quasisyntax/loc loc [#,v . #,e]))
                                                 (syntax->list #'(vars ...))
                                                 (syntax->list #'(exprs ...))
                                                 (syntax->list (syntax-case stx () [(_ bs . body) #'bs])))])
                                 (quasisyntax/loc stx (#,form bindings . body)))])))])
    (values (mk #'let) (mk #'let*) (mk #'letrec))))

(define-syntax (let: stx)
  (syntax-case stx (:)
    [(let: nm : ret-ty ([arg : ty val] ...) . body)
     (identifier? #'nm)
     (syntax/loc stx ((letrec: ([nm : (ty ... -> ret-ty) (lambda: ([arg : ty] ...) . body)]) nm) val ...))]
    [(let: . rest)
     (syntax/loc stx (let-internal: . rest))]))

(define-syntax (define-type-alias stx)
  (syntax-case stx ()
    [(_ tname rest) 
     (identifier? #'tname)
     (begin
       #`(begin
           #,(ignore #'(define-syntax tname (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))))
           #,(internal (syntax/loc stx (define-type-alias-internal tname rest)))))]
    [(_ (tname . args) rest)
     (andmap identifier? (syntax->list #'args))
     #'(define-type-alias tname (All args rest))]))

(define-syntax (define-typed-struct/exec stx)
  (syntax-case stx (:)
    [(_ nm ([fld : ty] ...) [proc : proc-ty])
     (with-syntax* 
      ([proc* (syntax-property #'(ann proc : proc-ty) 'typechecker:with-type #t)]
       [d-s (syntax-property (syntax/loc stx (define-struct/properties nm (fld ...)
                                               ([prop:procedure proc*])))
                             'typechecker:ignore-some #t)]
       [dtsi (internal (syntax/loc stx (define-typed-struct/exec-internal nm ([fld : ty] ...) proc-ty)))])
      #'(begin d-s dtsi))]))

(define-syntax (with-handlers: stx)
  (syntax-case stx ()
    [(_ ([pred? action] ...) . body)
     (with-syntax ([(pred?* ...) (map (lambda (s) (syntax-property #`(ann #,s : (Any -> Any)) 'typechecker:with-type #t))
                                      (syntax->list #'(pred? ...)))]
                   [(action* ...)
                    (map (lambda (s) (syntax-property s 'typechecker:exn-handler #t)) (syntax->list #'(action ...)))]
                   [body* (syntax-property #'(let-values () . body) 'typechecker:exn-body #t)])
       (syntax-property #'(with-handlers ([pred?* action*] ...) body*)
                        'typechecker:with-handlers
                        #t))]))

(define-syntax (define-typed-struct stx)
  (syntax-case stx (:)
    [(_ nm ([fld : ty] ...) . opts)
     (let ([mutable (if (memq '#:mutable (syntax->datum #'opts))
                        '(#:mutable)
                        '())])
       (with-syntax ([d-s (syntax-property (syntax/loc stx (define-struct nm (fld ...) . opts))
                                           'typechecker:ignore #t)]
                     [dtsi (internal (quasisyntax/loc stx (define-typed-struct-internal nm ([fld : ty] ...) #,@mutable)))])
         #'(begin d-s dtsi)))]
    [(_ (vars ...) nm ([fld : ty] ...) . opts)
     (with-syntax ([d-s (syntax-property (syntax/loc stx (define-struct nm (fld ...) . opts))
                                         'typechecker:ignore #t)]
                   [dtsi (internal (syntax/loc stx (define-typed-struct-internal (vars ...) nm ([fld : ty] ...))))])
       #'(begin d-s dtsi))]))

(define-syntax (require-typed-struct stx)
  (syntax-case stx (:)
    [(_ nm ([fld : ty] ...) lib)
     (identifier? #'nm)
     (with-syntax* ([(struct-info maker pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                    [(mut ...) (map (lambda _ #'#f) (syntax->list #'(sel ...)))])
                   (quasisyntax/loc stx
                     (begin
                       (require (only-in lib struct-info))
                       (define-syntax nm (make-struct-info 
                                          (lambda ()
                                            (list #'struct-info
                                                  #'maker
                                                  #'pred
                                                  (reverse (list #'sel ...))
                                                  (list mut ...)
                                                  #f))))
                       #,(internal #'(define-typed-struct-internal nm ([fld : ty] ...) #:type-only))
                       #,(ignore #'(require/contract pred (any/c . c-> . boolean?) lib))
                       #,(internal #'(require/typed-internal pred (Any -> Boolean : nm)))
                       (require/typed maker nm lib #:struct-maker #f)
                       (require/typed lib 
                         [sel (nm -> ty)]) ...)))]
    [(_ (nm parent) ([fld : ty] ...) lib)
     (and (identifier? #'nm) (identifier? #'parent))
     (with-syntax* ([(struct-info maker pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                    [(mut ...) (map (lambda _ #'#f) (syntax->list #'(sel ...)))]
                    #;[(parent-tys ...) (Struct-flds (parse-type #'parent))])
                   #`(begin
                       (require (only-in lib struct-info))
                       (define-syntax nm (make-struct-info 
                                          (lambda ()
                                            (list #'struct-info
                                                  #'maker
                                                  #'pred
                                                  (list #'sel ...)
                                                  (list mut ...)
                                                  #f))))
                       #,(internal #'(define-typed-struct-internal (nm parent) ([fld : ty] ...) #:type-only))
                       #,(ignore #'(require/contract pred (any/c . c-> . boolean?) lib))
                       #,(internal #'(require/typed-internal pred (Any -> Boolean : nm)))
                       (require/typed maker nm lib #:struct-maker parent)
                       (require/typed lib                          
                         [sel (nm -> ty)]) ...))]))

(define-syntax (do: stx)
  (syntax-case stx (:)
    [(_ : ty ((var : tys init . step) ...) (e0 e1 ...) c ...)
     (with-syntax ([(step ...)
                    (map (lambda (v s)
                           (syntax-case s ()
                             [() v]
                             [(e) #'e]
                             [_ (raise-syntax-error 
                                 #f
                                 "bad variable syntax"
                                 stx)]))
                         (syntax->list #'(var ...))
                         (syntax->list #'(step ...)))])
       (syntax-case #'(e1 ...) ()
         [() (syntax/loc
                 stx
               (let: doloop : ty ([var : tys init] ...)
                     (if (not e0)
                         (begin c ... (doloop step ...)))))]
         [(e1 e2 ...)
          (syntax/loc
              stx
            (let: doloop : ty ([var : tys init] ...)
                  (if e0
                      (begin e1 e2 ...)
                      (begin c ... (doloop step ...)))))]))]))

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
       #:literals (:)
       [(_ k:id : t . body)
	(quasisyntax/loc stx
   	  (let/cc #,(annotate-names #'([k : t]) stx) . body))]))
    (values (mk #'let/cc) (mk #'let/ec))))
