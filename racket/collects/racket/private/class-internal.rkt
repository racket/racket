#lang racket/base
(require (for-syntax racket/base)
         racket/contract/base
         racket/contract/combinator
         (only-in racket/contract/region current-contract-region)
         (only-in racket/contract/private/arrow making-a-method method-contract?)
         (only-in racket/list remove-duplicates)
         racket/stxparam
         racket/unsafe/ops
         "serialize-structs.rkt"
         racket/runtime-path
         (for-syntax racket/stxparam
                     syntax/kerncase
                     syntax/stx
                     syntax/name
                     syntax/define
                     syntax/flatten-begin
                     syntax/private/boundmap
                     syntax/parse
                     "classidmap.rkt"))

(define insp (current-inspector)) ; for all opaque structures

;;--------------------------------------------------------------------
;;  spec for external interface
;;--------------------------------------------------------------------

(provide provide-public-names
         ;; needed for Typed Racket
         (protect-out do-make-object find-method/who))
(define-syntax-rule (provide-public-names)
  (provide class class* class/derived
           define-serializable-class define-serializable-class*
           class?
           mixin
           interface interface* interface?
           object% object? externalizable<%> printable<%> writable<%> equal<%>
           object=?
           new make-object instantiate
           send send/apply send/keyword-apply send* send+ dynamic-send
           class-field-accessor class-field-mutator with-method
           get-field set-field! field-bound? field-names
           dynamic-get-field dynamic-set-field!
           private* public*  pubment*
           override* overment*
           augride* augment*
           public-final* override-final* augment-final*
           define/private define/public define/pubment
           define/override define/overment
           define/augride define/augment
           define/public-final define/override-final define/augment-final
           define-local-member-name define-member-name 
           member-name-key generate-member-key 
           member-name-key? member-name-key=? member-name-key-hash-code
           generic make-generic send-generic
           is-a? subclass? implementation? interface-extension?
           object-interface object-info object->vector
           object-method-arity-includes?
           method-in-interface? interface->method-names class->interface class-info
           (struct-out exn:fail:object)
           make-primitive-class
           class/c ->m ->*m ->dm case->m object/c instanceof/c
           
           ;; "keywords":
           private public override augment
           pubment overment augride
           public-final override-final augment-final
           field init init-field init-rest
           rename-super rename-inner inherit inherit/super inherit/inner inherit-field
           this this% super inner
           super-make-object super-instantiate super-new
           inspect absent abstract))


;;--------------------------------------------------------------------
;;  keyword setup
;;--------------------------------------------------------------------

(define-for-syntax (do-class-keyword stx orig-sym)
  (let ([orig-stx (datum->syntax #f orig-sym stx)])
    (if (identifier? stx)
        (raise-syntax-error
         #f
         "illegal (unparenthesized) use of a class keyword"
         orig-stx)
        (raise-syntax-error
         #f
         "use of a class keyword is not in a class top-level"
         orig-stx))))

(define-for-syntax (rewrite-renaming-class-keyword stx internal-id)
  (syntax-case stx ()
    [(_ elem ...)
     ;; Set taint mode on elem ...
     (with-syntax ([internal-id internal-id]
                   [(elem ...) (for/list ([e (in-list (syntax->list #'(elem ...)))])
                                 (if (identifier? e)
                                     e
                                     (syntax-property e 'taint-mode 'transparent)))])
       (syntax-property (syntax/loc stx (internal-id elem ...))
                        'taint-mode
                        'transparent))]))

(define-syntax provide-renaming-class-keyword
  (syntax-rules ()
    [(_ [id internal-id] ...)
     (begin
       (define-syntax (id stx) (rewrite-renaming-class-keyword stx #'internal-id))
       ...
       (define-syntax (internal-id stx) (do-class-keyword stx 'id))
       ...
       (provide id ...))]))

(provide-renaming-class-keyword [private -private]
                                [public -public]
                                [override -override]
                                [augride -augride]
                                [pubment -pubment]
                                [overment -overment]
                                [augment -augment]
                                [public-final -public-final]
                                [override-final -override-final]
                                [augment-final -augment-final]
                                [rename-super -rename-super]
                                [rename-inner -rename-inner]
                                [inherit -inherit]
                                [inherit-field -inherit-field]
                                [inherit/super -inherit/super]
                                [inherit/inner -inherit/inner]
                                [abstract -abstract])

(define-for-syntax (rewrite-naming-class-keyword stx internal-id)
  (syntax-case stx ()
    [(_ elem ...)
     (with-syntax ([internal-id internal-id])
       (syntax-property (syntax/loc stx (internal-id elem ...))
                        'taint-mode
                        'transparent))]))

(define-syntax provide-naming-class-keyword
  (syntax-rules ()
    [(_ [id internal-id] ...)
     (begin
       (define-syntax (id stx) (rewrite-naming-class-keyword stx #'internal-id))
       ...
       (define-syntax (internal-id stx) (do-class-keyword stx 'id))
       ...
       (provide id ...))]))

(provide-naming-class-keyword [inspect -inspect]
                              [init-rest -init-rest])

;; Going ahead and doing this in a generic fashion, in case we later realize that
;; we need more class contract-specific keywords.
(define-for-syntax (do-class-contract-keyword stx)
  (raise-syntax-error
   #f
   "use of a class contract keyword is not in a class contract"
   stx))

(define-syntax provide-class-contract-keyword
  (syntax-rules ()
    [(_ id ...)
     (begin
       (define-syntax (id stx) (do-class-contract-keyword stx))
       ...
       (provide id ...))]))

(provide-class-contract-keyword absent)

(define-for-syntax (do-define-like-internal stx)
  (syntax-case stx ()
    [(_ orig . __)
     (raise-syntax-error
      #f
      "use of a class keyword is not in a class top-level"
      #'orig)]))

(define-for-syntax (do-define-like stx internal-id)
  (syntax-case stx ()
    [(_ elem ...)
     (syntax-property
      #`(#,internal-id #,stx
                       #,@(map (lambda (e)
                                 (if (identifier? e)
                                     e
                                     (syntax-property
                                      (syntax-case e ()
                                        [((n1 n2) . expr)
                                         (syntax-property
                                          (quasisyntax/loc e
                                            (#,(syntax-property
                                                #'(n1 n2)
                                                'certify-mode 'transparent)
                                             . expr))
                                          'certify-mode 'transparent)]
                                        [(n . expr)
                                         (identifier? #'n)
                                         (syntax-property e 'certify-mode 'transparent)]
                                        [_else e])
                                      'certify-mode 'transparent)))
                               (syntax-e #'(elem ...))))
      'certify-mode
      'transparent)]
    [(_ . elems)
     #`(#,internal-id #,stx . elems)]
    [_else 
     (raise-syntax-error #f "illegal (unparenthesized) use of class keyword" stx)]))

(define-syntax provide-class-define-like-keyword
  (syntax-rules ()
    [(_ [internal-id id] ...)
     (begin
       (define-syntax (internal-id stx) (do-define-like-internal stx))
       ...
       (define-syntax (id stx) (do-define-like stx #'internal-id))
       ...
       (provide id ...))]))

(provide-class-define-like-keyword 
 [-field field]
 [-init init]
 [-init-field init-field])


(define-for-syntax not-in-a-class
  (lambda (stx)
    (if (eq? (syntax-local-context) 'expression)
        (raise-syntax-error
         #f
         "use of a class keyword is not in a class"
         stx)
        (quasisyntax/loc stx (#%expression #,stx)))))

(define-syntax define/provide-context-keyword
  (syntax-rules ()
    [(_ (id param-id) ...)
     (begin
       (begin
         (provide id)
         (define-syntax-parameter param-id 
           (make-set!-transformer not-in-a-class))
         (define-syntax id
           (make-parameter-rename-transformer #'param-id)))
       ...)]))

(define/provide-context-keyword
  [this this-param]
  [this% this%-param]
  [super super-param]
  [inner inner-param]
  [super-make-object super-make-object-param]
  [super-instantiate super-instantiate-param]
  [super-new super-new-param])

;;--------------------------------------------------------------------
;;  local member name lookup
;;--------------------------------------------------------------------

(define-for-syntax (localize orig-id)
  (do-localize orig-id #'validate-local-member))

(define (validate-local-member orig s)
  (if (symbol? s)
      s
      (obj-error 'local-member-name
                 "used before its definition"
                 "name" (as-write orig))))

;;--------------------------------------------------------------------
;; field info creation/access
;;--------------------------------------------------------------------

;; A field-info is a (vector iref iset eref eset)
;; where
;;   iref, iset, eref, and eset are projections to be applied
;;     on internal and external access and mutation.

;; make-field-info creates a new field-info for a field.
;; The caller gives the class and relative position (in the
;; new object struct layer), and this function fills
;; in the projections.
(define (make-field-info cls rpos)
  (let ([field-ref (make-struct-field-accessor (class-field-ref cls) rpos)]
        [field-set! (make-struct-field-mutator (class-field-set! cls) rpos)])
    (vector field-ref field-set! field-ref field-set!)))

(define (field-info-extend-internal fi ppos pneg)
  (let* ([old-ref (unsafe-vector-ref fi 0)]
         [old-set! (unsafe-vector-ref fi 1)])
    (vector (λ (o) (ppos (old-ref o)))
            (λ (o v) (old-set! o (pneg v)))
            (unsafe-vector-ref fi 2)
            (unsafe-vector-ref fi 3))))

(define (field-info-extend-external fi ppos pneg)
  (let* ([old-ref (unsafe-vector-ref fi 2)]
         [old-set! (unsafe-vector-ref fi 3)])
    (vector (unsafe-vector-ref fi 0)
            (unsafe-vector-ref fi 1)
            (λ (o) (ppos (old-ref o)))
            (λ (o v) (old-set! o (pneg v))))))

(define (field-info-internal-ref  fi) (unsafe-vector-ref fi 0))
(define (field-info-internal-set! fi) (unsafe-vector-ref fi 1))
(define (field-info-external-ref  fi) (unsafe-vector-ref fi 2))
(define (field-info-external-set! fi) (unsafe-vector-ref fi 3))

;;--------------------------------------------------------------------
;;  class macros
;;--------------------------------------------------------------------

(define-syntaxes (class* _class class/derived)
  (let ()
    ;; Start with Helper functions
    
    (define (expand-all-forms stx defn-and-exprs def-ctx bind-local-id)
      (let* ([stop-forms
              (append
               (kernel-form-identifier-list)
               (list
                (quote-syntax #%app) ; racket/base app, as opposed to #%plain-app
                (quote-syntax lambda) ; racket/base lambda, as opposed to #%plain-lambda
                (quote-syntax -init)
                (quote-syntax -init-rest)
                (quote-syntax -field)
                (quote-syntax -init-field)
                (quote-syntax -inherit-field)
                (quote-syntax -private)
                (quote-syntax -public)
                (quote-syntax -override)
                (quote-syntax -augride)
                (quote-syntax -public-final)
                (quote-syntax -override-final)
                (quote-syntax -augment-final)
                (quote-syntax -pubment)
                (quote-syntax -overment)
                (quote-syntax -augment)
                (quote-syntax -rename-super)
                (quote-syntax -inherit)
                (quote-syntax -inherit/super)
                (quote-syntax -inherit/inner)
                (quote-syntax -rename-inner)
                (quote-syntax -abstract)
                (quote-syntax super)
                (quote-syntax inner)
                (quote-syntax this)
                (quote-syntax this%)
                (quote-syntax super-instantiate)
                (quote-syntax super-make-object)
                (quote-syntax super-new)
                (quote-syntax -inspect)))]
             [expand-context (generate-class-expand-context)]
             [expand
              (lambda (defn-or-expr)
                (local-expand
                 defn-or-expr
                 expand-context
                 stop-forms
                 def-ctx))])
        (let loop ([l defn-and-exprs])
          (if (null? l)
              null
              (let ([e (expand (car l))])
                (syntax-case e (begin define-syntaxes define-values)
                  [(begin . _)
                   (loop (append
                          (flatten-begin e)
                          (cdr l)))]
                  [(define-syntaxes (id ...) rhs)
                   (andmap identifier? (syntax->list #'(id ...)))
                   (begin
                     (with-syntax ([rhs (local-transformer-expand
                                         #'rhs
                                         'expression
                                         null)])
                       (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
                       (cons #'(define-syntaxes (id ...) rhs) (loop (cdr l)))))]
                  [(define-values (id ...) rhs)
                   (andmap identifier? (syntax->list #'(id ...)))
                   (begin
                     (map bind-local-id (syntax->list #'(id ...)))
                     (cons e (loop (cdr l))))]
                  [_else 
                   (cons e (loop (cdr l)))]))))))
    
    ;; returns two lists: expressions that start with an identifier in
    ;; `kws', and expressions that don't
    (define (extract kws l out-cons)
      (let loop ([l l])
        (if (null? l)
            (values null null)
            (let-values ([(in out) (loop (cdr l))])
              (cond
                [(and (stx-pair? (car l))
                      (let ([id (stx-car (car l))])
                        (and (identifier? id)
                             (ormap (lambda (k) (free-identifier=? k id)) kws))))
                 (values (cons (car l) in) out)]
                [else
                 (values in (out-cons (car l) out))])))))
    
    (define (extract* kws l)
      (let-values ([(in out) (extract kws l void)])
        in))
    
    (define (flatten alone l)
      (apply append
             (map (lambda (i)
                    (let ([l (let ([l (syntax->list i)])
                               (if (ormap (lambda (i)
                                            (free-identifier=? (car l) i))
                                          (syntax-e (quote-syntax (-init -init-field -field))))
                                   (cddr l)
                                   (cdr l)))])
                      (if alone
                          (map (lambda (i)
                                 (if (identifier? i)
                                     (alone i)
                                     (cons (stx-car i)
                                           (stx-car (stx-cdr i)))))
                               l)
                          l)))
                  l)))
    
    ;; Used with flatten:
    (define (pair i) (cons i i))
    
    (define (normalize-init/field i)
      ;; Put i in ((iid eid) optional-expr) form
      (cond
        [(identifier? i) (list (list i i))]
        [else (let ([a (stx-car i)])
                (if (identifier? a)
                    (cons (list a a) (stx-cdr i))
                    i))]))
    
    (define (norm-init/field-iid norm) (stx-car (stx-car norm)))
    (define (norm-init/field-eid norm) (stx-car (stx-cdr (stx-car norm))))
    
    ;; expands an expression enough that we can check whether it has
    ;; the right form for a method; must use local syntax definitions
    (define (proc-shape name orig-stx xform? 
                        the-obj the-finder
                        bad class-name expand-stop-names
                        def-ctx lookup-localize)
      (define (expand expr locals)
        (local-expand
         expr
         'expression
         (append locals (list #'lambda #'λ) expand-stop-names)
         def-ctx))
      ;; Checks whether the vars sequence is well-formed
      (define (vars-ok? vars)
        (or (identifier? vars)
            (stx-null? vars)
            (and (stx-pair? vars)
                 (identifier? (stx-car vars))
                 (vars-ok? (stx-cdr vars)))))
      (define (kw-vars-ok? vars)
        (or (identifier? vars)
            (stx-null? vars)
            (and (stx-pair? vars)
                 (let ([a (stx-car vars)]
                       [opt-arg-ok?
                        (lambda (a)
                          (or (identifier? a)
                              (and (stx-pair? a)
                                   (identifier? (stx-car a))
                                   (stx-pair? (stx-cdr a))
                                   (stx-null? (stx-cdr (stx-cdr a))))))])
                   (or (and (opt-arg-ok? a)
                            (kw-vars-ok? (stx-cdr vars)))
                       (and (keyword? (syntax-e a))
                            (stx-pair? (stx-cdr vars))
                            (opt-arg-ok? (stx-car (stx-cdr vars)))
                            (kw-vars-ok? (stx-cdr (stx-cdr vars)))))))))
      ;; mk-name: constructs a method name
      ;; for error reporting, etc.
      (define (mk-name name)
        (datum->syntax 
         #f 
         (string->symbol (format "~a method~a~a" 
                                 (syntax-e name)
                                 (if class-name
                                     " in "
                                     "")
                                 (or class-name 
                                     ""))) 
         #f))
      ;; -- transform loop starts here --
      (let loop ([stx orig-stx][can-expand? #t][name name][locals null])
        (syntax-case (disarm stx) (#%plain-lambda lambda λ case-lambda letrec-values let-values)
          [(lam vars body1 body ...)
           (or (and (free-identifier=? #'lam #'#%plain-lambda)
                    (vars-ok? (syntax vars)))
               (and (or (free-identifier=? #'lam #'lambda)
                        (free-identifier=? #'lam #'λ))
                    (kw-vars-ok? (syntax vars))))
           (if xform?
               (with-syntax ([the-obj the-obj]
                             [the-finder the-finder]
                             [name (mk-name name)])
                 (with-syntax ([vars (if (or (free-identifier=? #'lam #'lambda)
                                             (free-identifier=? #'lam #'λ))
                                         (let loop ([vars #'vars])
                                           (cond
                                             [(identifier? vars) vars]
                                             [(syntax? vars)
                                              (datum->syntax vars
                                                             (loop (syntax-e vars))
                                                             vars
                                                             vars)]
                                             [(pair? vars)
                                              (syntax-case (car vars) ()
                                                [(id expr)
                                                 (identifier? #'id)
                                                 ;; optional argument; need to wrap arg expression
                                                 (cons
                                                  (with-syntax ([expr (syntax/loc #'expr
                                                                        (let-syntax ([the-finder (quote-syntax the-obj)])
                                                                          (#%expression expr)))])
                                                    (syntax/loc (car vars)
                                                      (id expr)))
                                                  (loop (cdr vars)))]
                                                [_ (cons (car vars) (loop (cdr vars)))])]
                                             [else vars]))
                                         #'vars)])
                   (let ([l (syntax/loc stx 
                              (lambda (the-obj . vars) 
                                (let-syntax ([the-finder (quote-syntax the-obj)])
                                  body1 body ...)))])
                     (syntax-track-origin
                      (with-syntax ([l (rearm (add-method-property l) stx)])
                        (syntax/loc stx 
                          (let ([name l]) name)))                  
                      stx
                      (syntax-local-introduce #'lam)))))
               stx)]
          [(#%plain-lambda . _)
           (bad "ill-formed lambda expression for method" stx)]
          [(lambda . _)
           (bad "ill-formed lambda expression for method" stx)]
          [(λ . _)
           (bad "ill-formed lambda expression for method" stx)]
          [(case-lam [vars body1 body ...] ...)
           (and (free-identifier=? #'case-lam #'case-lambda)
                (andmap vars-ok? (syntax->list (syntax (vars ...)))))
           (if xform?
               (with-syntax ([the-obj the-obj]
                             [the-finder the-finder]
                             [name (mk-name name)])
                 (let ([cl (syntax/loc stx
                             (case-lambda [(the-obj . vars) 
                                           (let-syntax ([the-finder (quote-syntax the-obj)])
                                             body1 body ...)] ...))])
                   (syntax-track-origin 
                    (with-syntax ([cl (rearm (add-method-property cl) stx)])
                      (syntax/loc stx
                        (let ([name cl]) name)))
                    stx
                    (syntax-local-introduce #'case-lam))))
               stx)]
          [(case-lambda . _)
           (bad "ill-formed case-lambda expression for method" stx)]
          [(let- ([(id) expr] ...) let-body)
           (and (or (free-identifier=? (syntax let-) 
                                       (quote-syntax let-values))
                    (free-identifier=? (syntax let-) 
                                       (quote-syntax letrec-values)))
                (andmap identifier? (syntax->list (syntax (id ...)))))
           (let* ([letrec? (free-identifier=? (syntax let-) 
                                              (quote-syntax letrec-values))]
                  [ids (syntax->list (syntax (id ...)))]
                  [new-ids (if xform?
                               (map
                                (lambda (id)
                                  (datum->syntax
                                   #f
                                   (gensym (syntax-e id))))
                                ids)
                               ids)]
                  [body-locals (append ids locals)]
                  [exprs (map (lambda (expr id)
                                (loop expr #t id (if letrec?
                                                     body-locals
                                                     locals)))
                              (syntax->list (syntax (expr ...)))
                              ids)]
                  [body (let ([body (syntax let-body)])
                          (if (identifier? body)
                              (ormap (lambda (id new-id)
                                       (and (bound-identifier=? body id)
                                            new-id))
                                     ids new-ids)
                              (loop body #t name body-locals)))])
             (unless body
               (bad "bad form for method definition" orig-stx))
             (with-syntax ([(proc ...) exprs]
                           [(new-id ...) new-ids]
                           [mappings
                            (if xform?
                                (map
                                 (lambda (old-id new-id)
                                   (with-syntax ([old-id old-id]
                                                 [old-id-localized (lookup-localize (localize old-id))]
                                                 [new-id new-id]
                                                 [the-obj the-obj]
                                                 [the-finder the-finder])
                                     (syntax (old-id (make-direct-method-map 
                                                      (quote-syntax the-finder)
                                                      (quote the-obj)
                                                      (quote-syntax old-id)
                                                      (quote-syntax old-id-localized)
                                                      (quote new-id))))))
                                 ids new-ids)
                                null)]
                           [body body])
               (syntax-track-origin
                (rearm
                 (if xform?
                     (if letrec?
                         (syntax/loc stx (letrec-syntax mappings
                                           (let- ([(new-id) proc] ...) 
                                                 body)))
                         (syntax/loc stx (let- ([(new-id) proc] ...) 
                                               (letrec-syntax mappings
                                                 body))))
                     (syntax/loc stx (let- ([(new-id) proc] ...) 
                                           body)))
                 stx)
                stx
                (syntax-local-introduce #'let-))))]
          [_else 
           (if can-expand?
               (loop (expand stx locals) #f name locals)
               (bad "bad form for method definition" orig-stx))])))
    
    (define (add-method-property l)
      (syntax-property l 'method-arity-error #t))

    ;; `class' wants to be priviledged with respect to
    ;; syntax taints: save the declaration-time inspector and use it 
    ;; to disarm syntax taints
    (define method-insp (variable-reference->module-declaration-inspector
                         (#%variable-reference)))
    (define (disarm stx)
      (syntax-disarm stx method-insp))
    (define (rearm new old)
      (syntax-rearm new old))
    
    ;; --------------------------------------------------------------------------------
    ;; Start here:
    
    (define (main stx super-expr deserialize-id-expr name-id interface-exprs defn-and-exprs)
      (let-values ([(this-id) #'this-id]
                   [(the-obj) (datum->syntax (quote-syntax here) (gensym 'self))]
                   [(the-finder) (datum->syntax (quote-syntax here) (gensym 'find-self))])
        
        (let* ([def-ctx (syntax-local-make-definition-context)]
               [localized-map (make-bound-identifier-mapping)]
               [any-localized? #f]
               [localize/set-flag (lambda (id)
                                    (let ([id2 (localize id)])
                                      (unless (eq? id id2)
                                        (set! any-localized? #t))
                                      id2))]
               [bind-local-id (lambda (id)
                                (let ([l (localize/set-flag id)])
                                  (syntax-local-bind-syntaxes (list id) #f def-ctx)
                                  (bound-identifier-mapping-put!
                                   localized-map
                                   id
                                   l)))]
               [lookup-localize (lambda (id)
                                  (bound-identifier-mapping-get
                                   localized-map
                                   id
                                   (lambda ()
                                     ;; If internal & external names are distinguished,
                                     ;; we need to fall back to localize:
                                     (localize id))))])
          
          ;; ----- Expand definitions -----
          (let ([defn-and-exprs (expand-all-forms stx defn-and-exprs def-ctx bind-local-id)]
                [bad (lambda (msg expr)
                       (raise-syntax-error #f msg stx expr))]
                [class-name (if name-id
                                (syntax-e name-id)
                                (let ([s (syntax-local-infer-name stx)])
                                  (if (syntax? s)
                                      (syntax-e s)
                                      s)))])
            
            ;; ------ Basic syntax checks -----
            (for-each (lambda (stx)
                        (syntax-case stx (-init -init-rest -field -init-field -inherit-field
                                                -private -public -override -augride
                                                -public-final -override-final -augment-final
                                                -pubment -overment -augment
                                                -rename-super -inherit -inherit/super -inherit/inner -rename-inner
                                                -abstract
                                                -inspect)
                          [(form orig idp ...)
                           (and (identifier? (syntax form))
                                (or (free-identifier=? (syntax form) (quote-syntax -init))
                                    (free-identifier=? (syntax form) (quote-syntax -init-field))))
                           
                           (let ([form (syntax-e (stx-car (syntax orig)))])
                             (for-each 
                              (lambda (idp)
                                (syntax-case idp ()
                                  [id (identifier? (syntax id)) 'ok]
                                  [((iid eid)) (and (identifier? (syntax iid))
                                                    (identifier? (syntax eid))) 'ok]
                                  [(id expr) (identifier? (syntax id)) 'ok]
                                  [((iid eid) expr) (and (identifier? (syntax iid))
                                                         (identifier? (syntax eid))) 'ok]
                                  [else
                                   (bad 
                                    (format
                                     "~a element is not an optionally renamed identifier or identifier-expression pair"
                                     form)
                                    idp)]))
                              (syntax->list (syntax (idp ...)))))]
                          [(-inspect expr)
                           'ok]
                          [(-inspect . rest)
                           (bad "ill-formed inspect clause" stx)]
                          [(-init orig . rest)
                           (bad "ill-formed init clause" #'orig)]
                          [(-init-rest)
                           'ok]
                          [(-init-rest rest)
                           (identifier? (syntax rest))
                           'ok]
                          [(-init-rest . rest)
                           (bad "ill-formed init-rest clause" stx)]
                          [(-init-field orig . rest)
                           (bad "ill-formed init-field clause" #'orig)]
                          [(-field orig idp ...)
                           (for-each (lambda (idp)
                                       (syntax-case idp ()
                                         [(id expr) (identifier? (syntax id)) 'ok]
                                         [((iid eid) expr) (and (identifier? (syntax iid))
                                                                (identifier? (syntax eid)))
                                                           'ok]
                                         [else
                                          (bad 
                                           "field element is not an optionally renamed identifier-expression pair"
                                           idp)]))
                                     (syntax->list (syntax (idp ...))))]
                          [(-field orig . rest)
                           (bad "ill-formed field clause" #'orig)]
                          [(-private id ...)
                           (for-each
                            (lambda (id)
                              (unless (identifier? id)
                                (bad "private element is not an identifier" id)))
                            (syntax->list (syntax (id ...))))]
                          [(-private . rest)
                           (bad "ill-formed private clause" stx)]
                          [(-abstract id ...)
                           (for-each
                            (lambda (id)
                              (unless (identifier? id)
                                (bad "abstract element is not an identifier" id)))
                            (syntax->list (syntax (id ...))))]
                          [(-abstract . rest)
                           (bad "ill-formed abstract clause" stx)]
                          [(form idp ...)
                           (and (identifier? (syntax form))
                                (ormap (lambda (f) (free-identifier=? (syntax form) f))
                                       (syntax-e (quote-syntax (-public
                                                                -override
                                                                -augride
                                                                -public-final
                                                                -override-final
                                                                -augment-final
                                                                -pubment
                                                                -overment
                                                                -augment
                                                                -inherit
                                                                -inherit/super
                                                                -inherit/inner
                                                                -inherit-field)))))
                           (let ([form (syntax-e (syntax form))])
                             (for-each
                              (lambda (idp)
                                (syntax-case idp ()
                                  [id (identifier? (syntax id)) 'ok]
                                  [(iid eid) (and (identifier? (syntax iid)) (identifier? (syntax eid))) 'ok]
                                  [else
                                   (bad 
                                    (format
                                     "~a element is not an identifier or pair of identifiers"
                                     form)
                                    idp)]))
                              (syntax->list (syntax (idp ...)))))]
                          [(-public . rest)
                           (bad "ill-formed public clause" stx)]
                          [(-override . rest)
                           (bad "ill-formed override clause" stx)]
                          [(-augride . rest)
                           (bad "ill-formed augride clause" stx)]
                          [(-public-final . rest)
                           (bad "ill-formed public-final clause" stx)]
                          [(-override-final . rest)
                           (bad "ill-formed override-final clause" stx)]
                          [(-augment-final . rest)
                           (bad "ill-formed augment-final clause" stx)]
                          [(-pubment . rest)
                           (bad "ill-formed pubment clause" stx)]
                          [(-overment . rest)
                           (bad "ill-formed overment clause" stx)]
                          [(-augment . rest)
                           (bad "ill-formed augment clause" stx)]
                          [(-inherit . rest)
                           (bad "ill-formed inherit clause" stx)]
                          [(-inherit/super . rest)
                           (bad "ill-formed inherit/super clause" stx)]
                          [(-inherit/inner . rest)
                           (bad "ill-formed inherit/inner clause" stx)]
                          [(-inherit-field . rest)
                           (bad "ill-formed inherit-field clause" stx)]
                          [(kw idp ...)
                           (and (identifier? #'kw)
                                (or (free-identifier=? #'-rename-super #'kw)
                                    (free-identifier=? #'-rename-inner #'kw)))
                           (for-each 
                            (lambda (idp)
                              (syntax-case idp ()
                                [(iid eid) (and (identifier? (syntax iid)) (identifier? (syntax eid))) 'ok]
                                [else
                                 (bad 
                                  (format "~a element is not a pair of identifiers" (syntax-e #'kw))
                                  idp)]))
                            (syntax->list (syntax (idp ...))))]
                          [(-rename-super . rest)
                           (bad "ill-formed rename-super clause" stx)]
                          [(-rename-inner . rest)
                           (bad "ill-formed rename-inner clause" stx)]
                          [_ 'ok]))
                      defn-and-exprs)
            
            ;; ----- Sort body into different categories -----
            (let*-values ([(decls exprs)
                           (extract (syntax-e (quote-syntax (-inherit-field
                                                             -private
                                                             -public
                                                             -override
                                                             -augride
                                                             -public-final
                                                             -override-final
                                                             -augment-final
                                                             -pubment
                                                             -overment
                                                             -augment
                                                             -rename-super
                                                             -inherit
                                                             -inherit/super
                                                             -inherit/inner
                                                             -abstract
                                                             -rename-inner)))
                                    defn-and-exprs
                                    cons)]
                          [(inspect-decls exprs)
                           (extract (list (quote-syntax -inspect))
                                    exprs
                                    cons)]
                          [(plain-inits)
                           ;; Normalize after, but keep un-normal for error reporting
                           (flatten #f (extract* (syntax-e 
                                                  (quote-syntax (-init -init-rest)))
                                                 exprs))]
                          [(normal-plain-inits) (map normalize-init/field plain-inits)]
                          [(init-rest-decls _)
                           (extract (list (quote-syntax -init-rest))
                                    exprs
                                    void)]
                          [(inits)
                           (flatten #f (extract* (syntax-e 
                                                  (quote-syntax (-init -init-field)))
                                                 exprs))]
                          [(normal-inits)
                           (map normalize-init/field inits)]
                          [(plain-fields)
                           (flatten #f (extract* (list (quote-syntax -field)) exprs))]
                          [(normal-plain-fields)
                           (map normalize-init/field plain-fields)]
                          [(plain-init-fields)
                           (flatten #f (extract* (list (quote-syntax -init-field)) exprs))]
                          [(normal-plain-init-fields)
                           (map normalize-init/field plain-init-fields)]
                          [(inherit-fields)
                           (flatten pair (extract* (list (quote-syntax -inherit-field)) decls))]
                          [(privates)
                           (flatten pair (extract* (list (quote-syntax -private)) decls))]
                          [(publics)
                           (flatten pair (extract* (list (quote-syntax -public)) decls))]
                          [(overrides)
                           (flatten pair (extract* (list (quote-syntax -override)) decls))]
                          [(augrides)
                           (flatten pair (extract* (list (quote-syntax -augride)) decls))]
                          [(public-finals)
                           (flatten pair (extract* (list (quote-syntax -public-final)) decls))]
                          [(override-finals)
                           (flatten pair (extract* (list (quote-syntax -override-final)) decls))]
                          [(pubments)
                           (flatten pair (extract* (list (quote-syntax -pubment)) decls))]
                          [(overments)
                           (flatten pair (extract* (list (quote-syntax -overment)) decls))]
                          [(augments)
                           (flatten pair (extract* (list (quote-syntax -augment)) decls))]
                          [(augment-finals)
                           (flatten pair (extract* (list (quote-syntax -augment-final)) decls))]
                          [(rename-supers)
                           (flatten pair (extract* (list (quote-syntax -rename-super)) decls))]
                          [(inherits)
                           (flatten pair (extract* (list (quote-syntax -inherit)) decls))]
                          [(inherit/supers)
                           (flatten pair (extract* (list (quote-syntax -inherit/super)) decls))]
                          [(inherit/inners)
                           (flatten pair (extract* (list (quote-syntax -inherit/inner)) decls))]
                          [(abstracts)
                           (flatten pair (extract* (list (quote-syntax -abstract)) decls))]
                          [(rename-inners)
                           (flatten pair (extract* (list (quote-syntax -rename-inner)) decls))])
              
              ;; this function copies properties from the declarations expressions
              ;; that get dropped from a class form (e.g. (public x) from the body
              ;; of a class). It doesn't use syntax-track-origin because there is
              ;; no residual code that it would make sense to be the result of expanding
              ;; those away. So, instead we only look at a few properties (as below).
              (define (add-decl-props stx)
                (for/fold ([stx stx])
                          ([decl (in-list (append inspect-decls decls))])
                  (define (copy-prop src dest stx)
                    (syntax-property 
                     stx
                     dest
                     (cons (syntax-property decl src)
                           (syntax-property stx dest))))
                  (copy-prop
                   'origin 'disappeared-use
                   (copy-prop
                    'disappeared-use 'disappeared-use
                    (copy-prop
                     'disappeared-binding 'disappeared-binding
                     stx)))))
              
              ;; At most one inspect:
              (unless (or (null? inspect-decls)
                          (null? (cdr inspect-decls)))
                (bad "multiple inspect clauses" (cadr inspect-decls)))
              
              ;; At most one init-rest:
              (unless (or (null? init-rest-decls)
                          (null? (cdr init-rest-decls)))
                (bad "multiple init-rest clauses" (cadr init-rest-decls)))
              
              ;; Make sure init-rest is last
              (unless (null? init-rest-decls)
                (let loop ([l exprs] [saw-rest? #f])
                  (unless (null? l)
                    (cond
                      [(and (stx-pair? (car l))
                            (identifier? (stx-car (car l))))
                       (let ([form (stx-car (car l))])
                         (cond
                           [(free-identifier=? #'-init-rest form)
                            (loop (cdr l) #t)]
                           [(not saw-rest?) (loop (cdr l) #f)]
                           [(free-identifier=? #'-init form)
                            (bad "init clause follows init-rest clause" (stx-car (stx-cdr (car l))))]
                           [(free-identifier=? #'-init-field form)
                            (bad "init-field clause follows init-rest clause" (stx-car (stx-cdr (car l))))]
                           [else (loop (cdr l) #t)]))]
                      [else (loop (cdr l) saw-rest?)]))))
              
              ;; --- Check initialization on inits: ---
              (let loop ([inits inits] [normal-inits normal-inits])
                (unless (null? normal-inits)
                  (if (stx-null? (stx-cdr (car normal-inits)))
                      (loop (cdr inits)(cdr normal-inits))
                      (let loop ([inits (cdr inits)] [normal-inits (cdr normal-inits)])
                        (unless (null? inits)
                          (if (stx-null? (stx-cdr (car normal-inits)))
                              (bad "initializer without default follows an initializer with default"
                                   (car inits))
                              (loop (cdr inits) (cdr normal-inits))))))))
              
              ;; ----- Extract method definitions; check that they look like procs -----
              ;;  Optionally transform them, can expand even if not transforming.
              (let* ([field-names (map norm-init/field-iid
                                       (append normal-plain-fields normal-plain-init-fields))]
                     [inherit-field-names (map car inherit-fields)]
                     [plain-init-names (map norm-init/field-iid normal-plain-inits)]
                     [inherit-names (map car inherits)]
                     [inherit/super-names (map car inherit/supers)]
                     [inherit/inner-names (map car inherit/inners)]
                     [abstract-names (map car abstracts)]
                     [rename-super-names (map car rename-supers)]
                     [rename-inner-names (map car rename-inners)]
                     [local-public-dynamic-names (map car (append publics overrides augrides
                                                                  overments augments
                                                                  override-finals augment-finals
                                                                  abstracts))]
                     [local-public-names (append (map car (append pubments public-finals))
                                                 local-public-dynamic-names)]
                     [local-method-names (append (map car privates) local-public-names)]
                     [expand-stop-names (append
                                         local-method-names
                                         field-names
                                         inherit-field-names
                                         plain-init-names
                                         inherit-names
                                         inherit/super-names
                                         inherit/inner-names
                                         rename-super-names
                                         rename-inner-names
                                         (kernel-form-identifier-list))])
                ;; Do the extraction:
                (let-values ([(methods          ; (listof (cons id stx))
                               private-methods  ; (listof (cons id stx))
                               exprs            ; (listof stx)
                               stx-defines)     ; (listof (cons (listof id) stx))
                              (let loop ([exprs exprs][ms null][pms null][es null][sd null])
                                (if (null? exprs)
                                    (values (reverse ms) (reverse pms) (reverse es) (reverse sd))
                                    (syntax-case (car exprs) (define-values define-syntaxes)
                                      [(d-v (id ...) expr)
                                       (free-identifier=? #'d-v #'define-values)
                                       (let ([ids (syntax->list (syntax (id ...)))])
                                         ;; Check form:
                                         (for-each (lambda (id)
                                                     (unless (identifier? id)
                                                       (bad "not an identifier for definition" id)))
                                                   ids)
                                         ;; method defn? (id in the list of privates/publics/overrides/augrides?)
                                         (if (ormap (lambda (id)
                                                      (ormap (lambda (i) (bound-identifier=? i id))
                                                             local-method-names))
                                                    ids)
                                             ;; Yes, it's a method:
                                             (begin
                                               (unless (null? (cdr ids))
                                                 (bad "each method variable needs its own definition"
                                                      (car exprs)))
                                               (let ([expr 
                                                      (syntax-track-origin
                                                       (proc-shape #f (syntax expr) #f 
                                                                   the-obj the-finder
                                                                   bad class-name expand-stop-names
                                                                   def-ctx lookup-localize)
                                                       (car exprs)
                                                       (syntax-local-introduce #'d-v))]
                                                     [public? (ormap (lambda (i) 
                                                                       (bound-identifier=? i (car ids)))
                                                                     local-public-names)])
                                                 (loop (cdr exprs) 
                                                       (if public?
                                                           (cons (cons (car ids) expr) ms)
                                                           ms)
                                                       (if public?
                                                           pms
                                                           (cons (cons (car ids) expr) pms))
                                                       es
                                                       sd)))
                                             ;; Non-method defn:
                                             (loop (cdr exprs) ms pms (cons (car exprs) es) sd)))]
                                      [(define-values . _)
                                       (bad "ill-formed definition" (car exprs))]
                                      [(define-syntaxes (id ...) expr)
                                       (let ([ids (syntax->list (syntax (id ...)))])
                                         (for-each (lambda (id) (unless (identifier? id)
                                                                  (bad "syntax name is not an identifier" id)))
                                                   ids)
                                         (loop (cdr exprs) ms pms es (cons (cons ids (car exprs)) sd)))]
                                      [(define-syntaxes . _)
                                       (bad "ill-formed syntax definition" (car exprs))]
                                      [_else
                                       (loop (cdr exprs) ms pms (cons (car exprs) es) sd)])))])
                  
                  ;; ---- Extract all defined names, including field accessors and mutators ---
                  (let ([defined-syntax-names (apply append (map car stx-defines))]
                        [defined-method-names (append (map car methods)
                                                      (map car private-methods))]
                        [private-field-names (let loop ([l exprs])
                                               (if (null? l)
                                                   null
                                                   (syntax-case (car l) (define-values)
                                                     [(define-values (id ...) expr)
                                                      (append (syntax->list (syntax (id ...)))
                                                              (loop (cdr l)))]
                                                     [_else (loop (cdr l))])))]
                        [init-mode (cond
                                     [(null? init-rest-decls) 'normal]
                                     [(stx-null? (stx-cdr (car init-rest-decls))) 'stop]
                                     [else 'list])])
                    
                    ;; -- Look for duplicates --
                    (let ([dup (check-duplicate-identifier
                                (append defined-syntax-names
                                        defined-method-names
                                        private-field-names
                                        field-names
                                        inherit-field-names
                                        plain-init-names
                                        inherit-names
                                        inherit/super-names
                                        inherit/inner-names
                                        rename-super-names
                                        rename-inner-names))])
                      (when dup
                        (bad "duplicate declared identifier" dup)))
                    
                    ;; -- Could still have duplicates within private/public/override/augride --
                    (let ([dup (check-duplicate-identifier local-method-names)])
                      (when dup
                        (bad "duplicate declared identifier" dup)))
                    
                    ;; -- Check for duplicate external method names, init names, or field names
                    (let ([check-dup
                           (lambda (what l)
                             (let ([ht (make-hasheq)])
                               (for-each (lambda (id)
                                           (when (hash-ref ht (syntax-e id) #f)
                                             (bad (format "duplicate declared external ~a name" what) id))
                                           (hash-set! ht (syntax-e id) #t))
                                         l)))])
                      ;; method names
                      (check-dup "method" (map cdr (append publics overrides augrides
                                                           pubments overments augments
                                                           public-finals override-finals augment-finals)))
                      ;; inits
                      (check-dup "init" (map norm-init/field-eid (append normal-inits)))
                      ;; fields
                      (check-dup "field" (map norm-init/field-eid (append normal-plain-fields normal-plain-init-fields))))
                    
                    ;; -- Check that private/public/override/augride are defined --
                    ;; -- and that abstracts are *not* defined                   --
                    (let ([ht (make-hasheq)]
                          [stx-ht (make-hasheq)])
                      (for-each
                       (lambda (defined-name)
                         (let ([l (hash-ref ht (syntax-e defined-name) null)])
                           (hash-set! ht (syntax-e defined-name) (cons defined-name l))))
                       defined-method-names)
                      (for-each
                       (lambda (defined-name)
                         (let ([l (hash-ref stx-ht (syntax-e defined-name) null)])
                           (hash-set! stx-ht (syntax-e defined-name) (cons defined-name l))))
                       defined-syntax-names)
                      (for-each
                       (lambda (pubovr-name)
                         (let ([l (hash-ref ht (syntax-e pubovr-name) null)]
                               [stx-l (hash-ref stx-ht (syntax-e pubovr-name) null)])
                           (cond ;; defined as value
                                 [(ormap (lambda (i) (bound-identifier=? i pubovr-name)) l)
                                  ;; check if abstract and fail if so
                                  (when (memq pubovr-name abstract-names)
                                    (bad "method declared as abstract but was defined"
                                         pubovr-name))]
                                 ;; defined as syntax
                                 [(ormap (lambda (i) (bound-identifier=? i pubovr-name)) stx-l)
                                  (bad "method declared but defined as syntax"
                                       pubovr-name)]
                                 ;; undefined
                                 [else
                                  (unless (memq pubovr-name abstract-names)
                                    (bad "method declared as concrete but not defined"
                                         pubovr-name))])))
                       local-method-names))
                    
                    ;; ---- Check that rename-inner doesn't have a non-final decl ---
                    (unless (null? rename-inners)
                      (let ([ht (make-hasheq)])
                        (for-each (lambda (pub)
                                    (hash-set! ht (syntax-e (cdr pub)) #t))
                                  (append publics public-finals overrides override-finals augrides))
                        (for-each (lambda (inn)
                                    (when (hash-ref ht (syntax-e (cdr inn)) #f)
                                      (bad
                                       "inner method is locally declared as public, override, public-final, override-final, or augride"
                                       (cdr inn))))
                                  rename-inners)))
                    
                    ;; ---- Convert expressions ----
                    ;;  Non-method definitions to set!
                    ;;  Initializations args access/set!
                    (let ([exprs (map (lambda (e)
                                        (syntax-case e ()
                                          [(d-v (id ...) expr)
                                           (and (identifier? #'d-v)
                                                (free-identifier=? #'d-v #'define-values))
                                           (syntax-track-origin (syntax/loc e (set!-values (id ...) expr))
                                                                e
                                                                #'d-v)]
                                          [(_init orig idp ...)
                                           (and (identifier? (syntax _init))
                                                (ormap (lambda (it) 
                                                         (free-identifier=? it (syntax _init)))
                                                       (syntax-e (quote-syntax (-init
                                                                                -init-field)))))
                                           (let* ([norms (map normalize-init/field
                                                              (syntax->list (syntax (idp ...))))]
                                                  [iids (map norm-init/field-iid norms)]
                                                  [exids (map norm-init/field-eid norms)])
                                             (with-syntax ([(id ...) iids]
                                                           [(idpos ...) (map localize/set-flag exids)]
                                                           [(defval ...) 
                                                            (map (lambda (norm)
                                                                   (if (stx-null? (stx-cdr norm))
                                                                       (syntax #f)
                                                                       (with-syntax ([defexp (stx-car (stx-cdr norm))])
                                                                         (syntax (lambda () defexp)))))
                                                                 norms)]
                                                           [class-name class-name])
                                               (syntax-track-origin
                                                (syntax/loc e 
                                                  (begin 
                                                    1 ; to ensure a non-empty body
                                                    (set! id (extract-arg 'class-name `idpos init-args defval))
                                                    ...))
                                                e
                                                #'_init)))]
                                          [(-fld orig idp ...)
                                           (and (identifier? #'-fld)
					        (free-identifier=? #'-fld #'-field))
                                           (with-syntax ([(((iid eid) expr) ...)
                                                          (map normalize-init/field (syntax->list #'(idp ...)))])
                                             (syntax-track-origin
                                              (syntax/loc e (begin 
                                                              1 ; to ensure a non-empty body
                                                              (set! iid expr)
                                                              ...))
                                              e
                                              #'-fld))]
                                          [(-i-r id/rename)
                                           (and (identifier? #'-i-r) 
                                                (free-identifier=? #'-i-r #'-init-rest))
                                           (with-syntax ([n (+ (length plain-inits)
                                                               (length plain-init-fields)
                                                               -1)]
                                                         [id (if (identifier? #'id/rename)
                                                                 #'id/rename
                                                                 (stx-car #'id/rename))])
                                             (syntax-track-origin
                                              (syntax/loc e (set! id (extract-rest-args n init-args)))
                                              e
                                              #'-i-r))]
                                          [(-i-r)
                                           (and (identifier? #'-i-r) 
                                                (free-identifier=? #'-i-r #'-init-rest))
                                           (syntax-track-origin (syntax (void)) e #'-i-r)]
                                          [_else e]))
                                      exprs)]
                          [mk-method-temp
                           (lambda (id-stx)
                             (datum->syntax (quote-syntax here)
                                            (gensym (syntax-e id-stx))))]
                          [rename-super-extras (append overments overrides override-finals inherit/supers)]
                          [rename-inner-extras (append pubments overments augments inherit/inners)]
                          [all-rename-inners (append (map car rename-inners)
                                                     (generate-temporaries (map car pubments))
                                                     (generate-temporaries (map car overments))
                                                     (generate-temporaries (map car augments))
                                                     (generate-temporaries (map car inherit/inners)))]
                          [all-inherits (append inherits inherit/supers inherit/inners)]
                          [definify (lambda (l)
                                      (map bind-local-id l)
                                      l)])
                      
                      ;; ---- set up field and method mappings ----
                      (with-syntax ([(rename-super-orig ...) (definify (map car rename-supers))]
                                    [(rename-super-orig-localized ...) (map lookup-localize (map car rename-supers))]
                                    [(rename-super-extra-orig ...) (map car rename-super-extras)]
                                    [(rename-super-temp ...) (definify (generate-temporaries (map car rename-supers)))]
                                    [(rename-super-extra-temp ...) (generate-temporaries (map car rename-super-extras))]
                                    [(rename-inner-orig ...) (definify (map car rename-inners))]
                                    [(rename-inner-orig-localized ...) (map lookup-localize (map car rename-inners))]
                                    [(rename-inner-extra-orig ...) (map car rename-inner-extras)]
                                    [(rename-inner-temp ...) (generate-temporaries (map car rename-inners))]
                                    [(rename-inner-extra-temp ...) (generate-temporaries (map car rename-inner-extras))]
                                    [(private-name ...) (map car privates)]
                                    [(private-name-localized ...) (map lookup-localize (map car privates))]
                                    [(private-temp ...) (map mk-method-temp (map car privates))]
                                    [(pubment-name ...) (map car pubments)]
                                    [(pubment-name-localized ...) (map lookup-localize (map car pubments))]
                                    [(pubment-temp ...) (map
                                                         mk-method-temp
                                                         (map car pubments))]
                                    [(public-final-name ...) (map car public-finals)]
                                    [(public-final-name-localized ...) (map lookup-localize (map car public-finals))]
                                    [(public-final-temp ...) (map
                                                              mk-method-temp
                                                              (map car public-finals))]
                                    [(method-name ...) (append local-public-dynamic-names
                                                               (map car all-inherits))]
                                    [(method-name-localized ...) (map lookup-localize
                                                                      (append local-public-dynamic-names
                                                                              (map car all-inherits)))]
                                    [(method-accessor ...) (generate-temporaries
                                                            (append local-public-dynamic-names
                                                                    (map car all-inherits)))]
                                    [(inherit-field-accessor ...) (generate-temporaries
                                                                   (map (lambda (id)
                                                                          (format "get-~a"
                                                                                  (syntax-e id)))
                                                                        inherit-field-names))]
                                    [(inherit-field-mutator ...) (generate-temporaries
                                                                  (map (lambda (id)
                                                                         (format "set-~a!"
                                                                                 (syntax-e id)))
                                                                       inherit-field-names))]
                                    [(inherit-name ...) (definify (map car all-inherits))]
                                    [(inherit-field-name ...) (definify inherit-field-names)]
                                    [(inherit-field-name-localized ...) (map lookup-localize inherit-field-names)]
                                    [(local-field ...) (definify
                                                         (append field-names
                                                                 private-field-names))]
                                    [(local-field-localized ...) (map lookup-localize
                                                                      (append field-names
                                                                              private-field-names))]
                                    [(local-field-pos ...) (let loop ([pos 0][l (append field-names
                                                                                        private-field-names)])
                                                             (if (null? l)
                                                                 null
                                                                 (cons pos (loop (add1 pos) (cdr l)))))]
                                    [(local-field-accessor ...) (generate-temporaries (append field-names private-field-names))]
                                    [(local-field-mutator ...) (generate-temporaries (append field-names private-field-names))]
                                    [(plain-init-name ...) (definify plain-init-names)]
                                    [(plain-init-name-localized ...) (map lookup-localize plain-init-names)]
                                    [(local-plain-init-name ...) (generate-temporaries plain-init-names)])
                        (let ([mappings
                               ;; make-XXX-map is supplied by private/classidmap.rkt
                               (with-syntax ([the-obj the-obj]
                                             [the-finder the-finder]
                                             [this-id this-id])
                                 (syntax 
                                  ([(inherit-field-name ...
                                     local-field ...
                                     rename-super-orig ...
                                     rename-inner-orig ...
                                     method-name ...
                                     private-name ...
                                     public-final-name ...
                                     pubment-name ...)
                                    (values
                                     (make-field-map (quote-syntax the-finder)
                                                     (quote the-obj)
                                                     (quote-syntax inherit-field-name)
                                                     (quote-syntax inherit-field-name-localized)
                                                     (quote-syntax inherit-field-accessor)
                                                     (quote-syntax inherit-field-mutator))
                                     ...
                                     (make-field-map (quote-syntax the-finder)
                                                     (quote the-obj)
                                                     (quote-syntax local-field)
                                                     (quote-syntax local-field-localized)
                                                     (quote-syntax local-field-accessor)
                                                     (quote-syntax local-field-mutator))
                                     ...
                                     (make-rename-super-map (quote-syntax the-finder)
                                                            (quote the-obj)
                                                            (quote-syntax rename-super-orig)
                                                            (quote-syntax rename-super-orig-localized)
                                                            (quote-syntax rename-super-temp))
                                     ...
                                     (make-rename-inner-map (quote-syntax the-finder)
                                                            (quote the-obj)
                                                            (quote-syntax rename-inner-orig)
                                                            (quote-syntax rename-inner-orig-localized)
                                                            (quote-syntax rename-inner-temp))
                                     ...
                                     (make-method-map (quote-syntax the-finder)
                                                      (quote the-obj)
                                                      (quote-syntax method-name)
                                                      (quote-syntax method-name-localized)
                                                      (quote-syntax method-accessor))
                                     ...
                                     (make-direct-method-map (quote-syntax the-finder)
                                                             (quote the-obj)
                                                             (quote-syntax private-name)
                                                             (quote-syntax private-name-localized)
                                                             (quote private-temp))
                                     ...
                                     (make-direct-method-map (quote-syntax the-finder)
                                                             (quote the-obj)
                                                             (quote-syntax public-final-name)
                                                             (quote-syntax public-final-name-localized)
                                                             (quote public-final-temp))
                                     ...
                                     (make-direct-method-map (quote-syntax the-finder)
                                                             (quote the-obj)
                                                             (quote-syntax pubment-name)
                                                             (quote-syntax pubment-name-localized)
                                                             (quote pubment-temp))
                                     ...)])))]
                              [extra-init-mappings (syntax 
                                                    ([(plain-init-name ...)
                                                      (values
                                                       (make-init-error-map (quote-syntax plain-init-name-localized))
                                                       ...)]))])
                          
                          (let ([find-method 
                                 (lambda (methods)
                                   (lambda (name)
                                     (ormap 
                                      (lambda (m)
                                        (and (bound-identifier=? (car m) name)
                                             (with-syntax ([proc (proc-shape (car m) (cdr m) #t 
                                                                             the-obj the-finder
                                                                             bad class-name expand-stop-names
                                                                             def-ctx lookup-localize)]
                                                           [extra-init-mappings extra-init-mappings])
                                               (syntax
                                                (syntax-parameterize 
                                                 ([super-instantiate-param super-error-map]
                                                  [super-make-object-param super-error-map]
                                                  [super-new-param super-error-map])
                                                 (letrec-syntaxes+values extra-init-mappings ()
                                                   proc))))))
                                      methods)))]
                                [lookup-localize-cdr (lambda (p) (lookup-localize (cdr p)))])
                            
                            (internal-definition-context-seal def-ctx)
                            
                            ;; ---- build final result ----
                            (with-syntax ([public-names (map lookup-localize-cdr publics)]
                                          [public-final-names (map lookup-localize-cdr public-finals)]
                                          [override-names (map lookup-localize-cdr overrides)]
                                          [override-final-names (map lookup-localize-cdr override-finals)]
                                          [augride-names (map lookup-localize-cdr augrides)]
                                          [pubment-names (map lookup-localize-cdr pubments)]
                                          [overment-names (map lookup-localize-cdr overments)]
                                          [augment-names (map lookup-localize-cdr augments)]
                                          [augment-final-names (map lookup-localize-cdr augment-finals)]
                                          [(rename-super-name ...) (map lookup-localize-cdr rename-supers)]
                                          [(rename-super-extra-name ...) (map lookup-localize-cdr rename-super-extras)]
                                          [(rename-inner-name ...) (map lookup-localize-cdr rename-inners)]
                                          [(rename-inner-extra-name ...) (map lookup-localize-cdr rename-inner-extras)]
                                          [inherit-names (map lookup-localize-cdr all-inherits)]
                                          [abstract-names (map lookup-localize-cdr abstracts)]
                                          [num-fields (datum->syntax
                                                       (quote-syntax here)
                                                       (+ (length private-field-names)
                                                          (length plain-init-fields)
                                                          (length plain-fields)))]
                                          [field-names (map (lambda (norm)
                                                              (lookup-localize (norm-init/field-eid norm)))
                                                            (append
                                                             normal-plain-fields
                                                             normal-plain-init-fields))]
                                          [inherit-field-names (map lookup-localize (map cdr inherit-fields))]
                                          [init-names (map (lambda (norm)
                                                             (lookup-localize
                                                              (norm-init/field-eid norm)))
                                                           normal-inits)]
                                          [init-mode init-mode]
                                          [(private-method ...) (map (find-method private-methods) (map car privates))]
                                          [public-methods (map (find-method methods) (map car publics))]
                                          [override-methods (map (find-method methods) (map car (append overments
                                                                                                        override-finals
                                                                                                        overrides)))]
                                          [augride-methods (map (find-method methods) (map car (append augments
                                                                                                       augment-finals
                                                                                                       augrides)))]
                                          [(pubment-method ...) (map (find-method methods) (map car pubments))]
                                          [(public-final-method ...) (map (find-method methods) (map car public-finals))]
                                          ;; store a dummy method body that should never be called for abstracts
                                          [(abstract-method ...) (map (lambda (abs)
                                                                        #'(lambda (this . rest)
                                                                            (obj-error 'class "cannot call abstract method")))
                                                                      (map car abstracts))]
                                          [mappings mappings]
                                          
                                          [exprs exprs]
                                          [the-obj the-obj]
                                          [the-finder the-finder]
                                          [name class-name]
                                          [(stx-def ...) (map cdr stx-defines)]
                                          [super-expression super-expr]
                                          [(interface-expression ...) interface-exprs]
                                          [inspector (if (pair? inspect-decls)
                                                         (stx-car (stx-cdr (car inspect-decls)))
                                                         #'(current-inspector))]
                                          [deserialize-id-expr deserialize-id-expr])
                              (add-decl-props
                              (quasisyntax/loc stx
                                (let ([superclass super-expression]
                                      [interfaces (list interface-expression ...)])
                                  (compose-class 
                                   'name superclass interfaces inspector deserialize-id-expr #,any-localized?
                                   ;; Field count:
                                   num-fields
                                   ;; Field names:
                                   `field-names
                                   `inherit-field-names
                                   ;; Method names:
                                   `(rename-super-name ... rename-super-extra-name ...)
                                   `(rename-inner-name ... rename-inner-extra-name ...)
                                   `pubment-names
                                   `public-final-names
                                   `public-names
                                   `overment-names
                                   `override-final-names
                                   `override-names
                                   `augment-names
                                   `augment-final-names
                                   `augride-names
                                   `inherit-names
                                   `abstract-names
                                   ;; Init arg names (in order)
                                   `init-names
                                   (quote init-mode)
                                   ;; Methods (when given needed super-methods, etc.):
                                   #, ;; Attach srcloc (useful for profiling)
                                   (quasisyntax/loc stx
                                     (lambda (local-accessor
                                              local-mutator
                                              inherit-field-accessor ...  ; inherit
                                              inherit-field-mutator ...
                                              rename-super-temp ... rename-super-extra-temp ...
                                              rename-inner-temp ... rename-inner-extra-temp ...
                                              method-accessor ...) ; for a local call that needs a dynamic lookup
                                       (let ([local-field-accessor
                                              (make-struct-field-accessor local-accessor local-field-pos #f)]
                                             ...
                                             [local-field-mutator
                                              (make-struct-field-mutator local-mutator local-field-pos #f)]
                                             ...)
                                         (syntax-parameterize
                                          ([this-param (make-this-map (quote-syntax this-id)
                                                                      (quote-syntax the-finder)
                                                                      (quote the-obj))]
                                           [this%-param (make-this%-map (quote-syntax (object-ref this))
                                                                        (quote-syntax the-finder))])
                                          (let-syntaxes
                                           mappings
                                           (syntax-parameterize 
                                            ([super-param
                                              (lambda (stx)
                                                (syntax-case stx (rename-super-extra-orig ...)
                                                  [(_ rename-super-extra-orig . args) 
                                                   (generate-super-call 
                                                    stx
                                                    (quote-syntax the-finder)
                                                    (quote the-obj)
                                                    (quote-syntax rename-super-extra-temp)
                                                    (syntax args))]
                                                  ...
                                                  [(_ id . args)
                                                   (identifier? #'id)
                                                   (raise-syntax-error
                                                    #f
                                                    (string-append
                                                     "identifier for super call does not have an override, "
                                                     "override-final, overment, or inherit/super declaration")
                                                    stx
                                                    #'id)]
                                                  [_else
                                                   (raise-syntax-error
                                                    #f
                                                    "expected an identifier after the keyword"
                                                    stx)]))]
                                             [inner-param
                                              (lambda (stx)
                                                (syntax-case stx (rename-inner-extra-orig ...)
                                                  [(_ default-expr rename-inner-extra-orig . args)
                                                   (generate-inner-call 
                                                    stx
                                                    (quote-syntax the-finder)
                                                    (quote the-obj)
                                                    (syntax default-expr)
                                                    (quote-syntax rename-inner-extra-temp)
                                                    (syntax args))]
                                                  ...
                                                  [(_ default-expr id . args)
                                                   (identifier? #'id)
                                                   (raise-syntax-error
                                                    #f
                                                    (string-append
                                                     "identifier for inner call does not have a pubment, augment, "
                                                     "overment, or inherit/inner declaration")
                                                    stx
                                                    #'id)]
                                                  [(_)
                                                   (raise-syntax-error
                                                    #f
                                                    "expected a default-value expression after the keyword"
                                                    stx
                                                    #'id)]
                                                  [_else
                                                   (raise-syntax-error
                                                    #f
                                                    "expected an identifier after the keyword and default-value expression"
                                                    stx)]))])
                                            stx-def ...
                                            (letrec ([private-temp private-method]
                                                     ...
                                                     [pubment-temp pubment-method]
                                                     ...
                                                     [public-final-temp public-final-method]
                                                     ...)
                                              (values
                                               (list pubment-temp ... public-final-temp ...
                                                     abstract-method ... . public-methods)
                                               (list . override-methods)
                                               (list . augride-methods)
                                               ;; Initialization
                                               #, ;; Attach srcloc (useful for profiling)
                                               (quasisyntax/loc stx
                                                 (lambda (the-obj super-go si_c si_inited? si_leftovers init-args)
                                                   (let-syntax ([the-finder (quote-syntax the-obj)])
                                                     (syntax-parameterize
                                                      ([super-instantiate-param
                                                        (lambda (stx)
                                                          (syntax-case stx () 
                                                            [(_ (arg (... ...)) (kw kwarg) (... ...))
                                                             (with-syntax ([stx stx])
                                                               (syntax (-instantiate super-go stx #f (the-obj si_c si_inited? 
                                                                                                           si_leftovers)
                                                                                     (list arg (... ...)) 
                                                                                     (kw kwarg) (... ...))))]))]
                                                       [super-new-param
                                                        (lambda (stx)
                                                          (syntax-case stx () 
                                                            [(_ (kw kwarg) (... ...))
                                                             (with-syntax ([stx stx])
                                                               (syntax (-instantiate super-go stx #f (the-obj si_c si_inited? 
                                                                                                           si_leftovers)
                                                                                     null
                                                                                     (kw kwarg) (... ...))))]))]
                                                       [super-make-object-param
                                                        (lambda (stx)
                                                          (let ([code 
                                                                 (quote-syntax
                                                                  (lambda args
                                                                    (super-go the-obj si_c si_inited? si_leftovers args null)))])
                                                            (if (identifier? stx)
                                                                code
                                                                (datum->syntax
                                                                 code
                                                                 (cons code
                                                                       (cdr (syntax-e stx)))))))])
                                                      (letrec-syntaxes+values
                                                          ([(plain-init-name) (make-init-redirect 
                                                                               (quote-syntax set!)
                                                                               (quote-syntax #%plain-app)
                                                                               (quote-syntax local-plain-init-name)
                                                                               (quote-syntax plain-init-name-localized))] ...)
                                                        ([(local-plain-init-name) undefined] ...)
                                                        (void) ; in case the body is empty
                                                        . exprs)))))))))))))
                                   ;; Not primitive:
                                   #f)))))))))))))))))
    
    ;; The class* and class entry points:
    (values
     ;; class*
     (lambda (stx)
        (syntax-case stx ()
          [(_  super-expression (interface-expr ...)
               defn-or-expr
               ...)
           (main stx
                 #'super-expression 
                 #f #f
                 (syntax->list #'(interface-expr ...))
                 (syntax->list #'(defn-or-expr ...)))]))
     ;; class
     (lambda (stx)
        (syntax-case stx ()
          [(_ super-expression
              defn-or-expr
              ...)
           (main stx
                 #'super-expression 
                 #f #f
                 null
                 (syntax->list #'(defn-or-expr ...)))]))
     ;; class/derived
     (lambda (stx)
        (syntax-case stx ()
          [(_  orig-stx
               [name-id super-expression (interface-expr ...) deserialize-id-expr]
               defn-or-expr
               ...)
           (main #'orig-stx
                 #'super-expression 
                 #'deserialize-id-expr 
                 (and (syntax-e #'name-id) #'name-id)
                 (syntax->list #'(interface-expr ...))
                 (syntax->list #'(defn-or-expr ...)))]))
     )))

(define-syntax (-define-serializable-class stx)
  (syntax-case stx ()
    [(_ orig-stx name super-expression (interface-expr ...)
        defn-or-expr ...)
     (let ([deserialize-name-info (datum->syntax
                                   #'name
                                   (string->symbol
                                    (format "deserialize-info:~a" (syntax-e #'name)))
                                   #'name)])
       (unless (memq (syntax-local-context) '(top-level module))
         (raise-syntax-error
          #f
          "allowed only at the top level or within a module top level"
          #'orig-stx))
       (with-syntax ([deserialize-name-info deserialize-name-info]
                     [(provision ...) (if (eq? (syntax-local-context) 'module)
                                          #`((runtime-require (submod "." deserialize-info))
                                             (module+ deserialize-info (provide #,deserialize-name-info)))
                                          #'())])
         #'(begin
             (define-values (name deserialize-name-info)
               (class/derived orig-stx [name
                                        super-expression 
                                        (interface-expr ...)
                                        #'deserialize-name-info]
                              defn-or-expr ...))
             provision ...)))]))

(define-syntax (define-serializable-class* stx)
  (syntax-case stx ()
    [(_ name super-expression (interface-expr ...)
        defn-or-expr ...)
     (with-syntax ([orig-stx stx])
       #'(-define-serializable-class orig-stx
                                     name
                                     super-expression
                                     (interface-expr ...)
                                     defn-or-expr ...))]))

(define-syntax (define-serializable-class stx)
  (syntax-case stx ()
    [(_ name super-expression
        defn-or-expr ...)
     (with-syntax ([orig-stx stx])
       #'(-define-serializable-class orig-stx
                                     name
                                     super-expression
                                     ()
                                     defn-or-expr ...))]))


(define-syntaxes (private* public* pubment* override* overment* augride* augment*
                           public-final* override-final* augment-final*)
  (let ([mk
         (lambda (who decl-form)
           (lambda (stx)
             (unless (class-top-level-context? (syntax-local-context))
               (raise-syntax-error
                #f
                "use of a class keyword is not in a class top-level"
                stx))
             (syntax-case stx ()
               [(_ binding ...)
                (let ([bindings (syntax->list (syntax (binding ...)))])
                  (let ([name-exprs
                         (map (lambda (binding)
                                (syntax-case binding ()
                                  [(name expr)
                                   (identifier? (syntax name))
                                   (cons (syntax name) (syntax expr))]
                                  [_else
                                   (identifier? (syntax name))
                                   (raise-syntax-error
                                    #f
                                    "expected an identifier and expression"
                                    stx
                                    binding)]))
                              bindings)])
                    (with-syntax ([(name ...) (map car name-exprs)]
                                  [(expr ...) (map cdr name-exprs)]
                                  [decl-form decl-form])
                      (syntax
                       (begin
                         (decl-form name ...)
                         (define name expr)
                         ...)))))])))])
    (values
     (mk 'private* (syntax private))
     (mk 'public* (syntax public))
     (mk 'pubment* (syntax pubment))
     (mk 'override* (syntax override))
     (mk 'overment* (syntax overment))
     (mk 'augride* (syntax augride))
     (mk 'augment* (syntax augment))
     (mk 'public-final* (syntax public-final))
     (mk 'override-final* (syntax override-final))
     (mk 'augment-final* (syntax augment)))))

(define-syntaxes (define/private define/public define/pubment 
                   define/override define/overment
                   define/augride define/augment
                   define/public-final define/override-final define/augment-final)
  (let ([mk
         (lambda (decl-form)
           (lambda (stx)
             (unless (class-top-level-context? (syntax-local-context))
               (raise-syntax-error
                #f
                "use of a class keyword is not in a class top-level"
                stx))
             (let-values ([(id rhs) (normalize-definition stx #'lambda #f #t)])
               (quasisyntax/loc stx
                 (begin
                   (#,decl-form #,id)
                   (define #,id #,rhs))))))])
    (values
     (mk #'private)
     (mk #'public)
     (mk #'pubment)
     (mk #'override)
     (mk #'overment)
     (mk #'augride)
     (mk #'augment)
     (mk #'public-final)
     (mk #'override-final)
     (mk #'augment-final))))

(define-syntax (define-local-member-name stx)
  (syntax-case stx ()
    [(_ id ...)
     (let ([ids (syntax->list (syntax (id ...)))])
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error
                      #f
                      "expected an identifier"
                      stx
                      id)))
                 ids)
       (let ([dup (check-duplicate-identifier ids)])
         (when dup
           (raise-syntax-error
            #f
            "duplicate identifier"
            stx
            dup)))
       (if (eq? (syntax-local-context) 'top-level)
           ;; Does nothing in particular at the top level:
           (syntax/loc stx (define-syntaxes (id ...) (values 'id ...)))
           ;; Map names to private indicators, which are made private
           ;;  simply by introduction:
           (with-syntax ([(gen-id ...) (generate-temporaries ids)])
             (with-syntax ([stx-defs
                            ;; Need to attach srcloc to this definition:
                            (syntax/loc stx
                              (define-syntaxes (id ...)
                                (values (make-private-name (quote-syntax id) (quote-syntax gen-id))
                                        ...)))])
               (syntax/loc stx
                 (begin
                   (define-values (gen-id ...)
                     (values (generate-local-member-name 'id) ...))
                   stx-defs))))))]))

(define-syntax (define-member-name stx)
  (syntax-case stx ()
    [(_ id expr)
     (let ([name #'id])
       (unless (identifier? name)
         (raise-syntax-error
          #f
          "expected an identifier for definition"
          stx
          name))
       (with-syntax ([stx-def
                      ;; Need to attach srcloc to this definition:
                      (syntax/loc stx
                        (define-syntax id
                          (make-private-name (quote-syntax id) 
                                             ((syntax-local-certifier) (quote-syntax member-name)))))])
         #'(begin
             (define member-name (check-member-key 'id expr))
             stx-def)))]))

(define (generate-local-member-name id)
  (string->uninterned-symbol
   (symbol->string id)))


(define-values (struct:member-key make-member-key member-name-key? member-key-ref member-key-set!)
  (make-struct-type 'member-name-key
                    #f
                    1 0 #f
                    (list
                     (cons prop:custom-write 
                           (lambda (v p write?)
                             (fprintf p "#<member-key:~a>" (member-key-id v)))))))

(define member-key-id (make-struct-field-accessor member-key-ref 0))

(define (check-member-key id v)
  (unless (member-name-key? v)
    (obj-error 'define-local-member-name 
               "value is not a member key" 
               "value" v
               "local name" (as-write id)))
  (member-key-id v))

(define-syntax (member-name-key stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([id (localize #'id)])
       (syntax/loc stx (make-member-key `id)))]
    [(_ x)
     (raise-syntax-error
      #f
      "not an identifier"
      stx
      #'x)]))

(define (generate-member-key)
  (make-member-key (generate-local-member-name (gensym 'member))))

(define (member-name-key=? a b)
  (if (and (member-name-key? a)
           (member-name-key? b))
      (eq? (member-key-id a) (member-key-id b))
      (eq? a b)))

(define (member-name-key-hash-code a)
  (unless (member-name-key? a)
    (raise-argument-error
     'member-name-key-hash-code
     "member-name-key?"
     a))
  (eq-hash-code (member-key-id a)))

;;--------------------------------------------------------------------
;;  class implementation
;;--------------------------------------------------------------------

(define-struct class (name
                      pos supers     ; pos is subclass depth, supers is vector
                      self-interface ; self interface
                      insp-mk        ; dummy struct maker to control inspection access
                      
                      method-width   ; total number of methods
                      method-ht      ; maps public names to vector positions
                      method-ids     ; reverse-ordered list of public method names
                      abstract-ids   ; list of abstract method names
                      method-ictcs   ; list of indices of methods to fix for interface ctcs

                      [ictc-classes  ; #f or weak hash of cached classes keyed by blame
                       #:mutable]

                      methods        ; vector of methods (for external dynamic dispatch)
                      super-methods  ; vector of methods (for subclass super calls)
                      int-methods    ; vector of vector of methods (for internal dynamic dispatch)
                      beta-methods   ; vector of vector of methods
                      meth-flags     ; vector: #f => primitive-implemented
                      ;         'final => final
                      ;         'augmentable => can augment
                      
                      inner-projs    ; vector of projections for the last inner slot
                      dynamic-idxs   ; vector of indexs for access into int-methods
                      dynamic-projs  ; vector of vector of projections for internal dynamic dispatch
                      
                      field-width    ; total number of fields
                      field-pub-width ; total number of public fields
                      field-ht       ; maps public field names to field-infos (see make-field-info above)
                      field-ids      ; list of public field names
                      
                      [struct:object ; structure type for instances
                       #:mutable]
                      [object?       ; predicate
                       #:mutable]
                      [make-object   ; : (-> object), constructor that creates an uninitialized object
                          #:mutable]
                      [field-ref     ; accessor
                       #:mutable]
                      [field-set!    ; mutator
                       #:mutable]
                      
                      init-args      ; list of symbols in order; #f => only by position
                      init-mode      ; 'normal, 'stop (don't accept by-pos for super), or 'list
                      
                      [init          ; initializer
                       #:mutable]    ; :   object
                      ;     (object class (box boolean) leftover-args new-by-pos-args new-named-args 
                      ;      -> void) // always continue-make-super?
                      ;     class
                      ;     (box boolean)
                      ;     leftover-args
                      ;     named-args
                      ;  -> void
                      
                      [orig-cls      ; uncontracted version of this class (or same class)
                       #:mutable]
                      [serializer    ; proc => serializer, #f => not serializable
                       #:mutable]
                      [fixup         ; for deserialization
                       #:mutable]
                      
                      no-super-init?); #t => no super-init needed
  #:inspector insp)

;; compose-class: produces one result if `deserialize-id' is #f, two
;;                results if `deserialize-id' is not #f
(define (compose-class name                ; symbol
                       super               ; class
                       interfaces          ; list of interfaces
                       inspector           ; inspector or #f
                       deserialize-id      ; identifier or #f
                       any-localized?      ; #t => need to double-check distinct external names
                       
                       num-fields          ; total fields (public & private)
                       public-field-names  ; list of symbols (shorter than num-fields)
                       inherit-field-names ; list of symbols (not included in num-fields)
                       
                       rename-super-names  ; list of symbols
                       rename-inner-names
                       pubment-names
                       public-final-names
                       public-normal-names
                       overment-names
                       override-final-names
                       override-normal-names
                       augment-names
                       augment-final-names
                       augride-normal-names
                       inherit-names
                       abstract-names
                       
                       init-args           ; list of symbols in order, or #f
                       init-mode           ; 'normal, 'stop, or 'list
                       
                       make-methods        ; takes field and method accessors
                       make-struct:prim)   ; see "primitive classes", below
  (define (make-method proc meth-name)
    (procedure-rename
     (procedure->method proc)
     (string->symbol
      (format "~a method~a~a"
              meth-name
              (if name " in " "")
              (or name "")))))
  
  ;; -- Check superclass --
  (unless (class? super)
    (obj-error 'class* "superclass expression result is not a class"
               "result" super
               #:class-name name))
  
  (when any-localized?
    (check-still-unique name
                        init-args
                        "initialization argument names")
    ;; We intentionally leave inherited names out of the lists below,
    ;;  on the threory that it's ok to decide to inherit from
    ;;  yourself:
    (check-still-unique name
                        (append public-field-names)
                        "field names")
    (check-still-unique name
                        (append pubment-names public-final-names public-normal-names
                                overment-names override-final-names override-normal-names
                                augment-names augment-final-names augride-normal-names
                                abstract-names)
                        "method names"))
  
  ;; -- Create new class's name --
  (let* ([name (or name
                   (let ([s (class-name super)])
                     (and s 
                          (not (eq? super object%))
                          (if (symbol? s) ;; how can 's' not be a symbol at this point?
                              (string->symbol (format "derived-from-~a" s))
                              s))))]
         ;; Combine method lists
         [public-names (append pubment-names public-final-names public-normal-names abstract-names)]
         [override-names (append overment-names override-final-names override-normal-names)]
         [augride-names (append augment-names augment-final-names augride-normal-names)]
         [final-names (append public-final-names override-final-names augment-final-names)]
         [augonly-names (append pubment-names overment-names augment-names)]
         ;; Misc utilities
         [no-new-methods? (null? public-names)]
         [no-method-changes? (and (null? public-names)
                                  (null? override-names)
                                  (null? augride-names)
                                  (null? final-names))]
         [no-new-fields? (null? public-field-names)]
         [xappend (lambda (a b) (if (null? b) a (append a b)))])
    
    ;; -- Check interfaces ---
    (for-each
     (lambda (intf)
       (unless (interface? intf)
         (obj-error 'class* "interface expression result is not an interface"
                    "result" intf
                    #:class-name name)))
     interfaces)
    
    ;; -- Check inspectors ---
    (when inspector
      (unless (inspector? inspector)
        (obj-error 'class* "class `inspect' result is not an inspector or #f"
                   "result" inspector
                   #:class-name name)))
    
    ;; -- Match method and field names to indices --
    (let ([method-ht (if no-new-methods?
                         (class-method-ht super)
                         (hash-copy (class-method-ht super)))]
          [field-ht (if no-new-fields?
                        (class-field-ht super)
                        (hash-copy (class-field-ht super)))]
          [super-method-ht (class-method-ht super)]
          [super-method-ids (class-method-ids super)]
          [super-field-ids (class-field-ids super)]
          [super-field-ht (class-field-ht super)]
          [super-abstract-ids (class-abstract-ids super)])
      
      ;; Put new ids in table, with pos (replace field pos with accessor info later)
      (unless no-new-methods?
        (for ([id (in-list public-names)]
              [p (in-naturals (class-method-width super))])
          (when (hash-ref method-ht id #f)
            (obj-error 'class* "superclass already contains method"
                       "superclass" super
                       "method name" (as-write id)
                       #:class-name name))
          (hash-set! method-ht id p)))
      
      ;; Keep check here for early failure, will add to hashtable later in this function.
      (unless no-new-fields?
        (for ([id (in-list public-field-names)])
          (when (hash-ref field-ht id #f)
              (obj-error 'class* "superclass already contains field"
                         "superclass" super
                         "field name" (as-write id)
                         #:class-name name))))
      
      ;; Check that superclass has expected fields
      (for-each (lambda (id)
                  (unless (hash-ref field-ht id #f)
                    (obj-error 'class* "superclass does not provide field" 
                               "superclass" super
                               "field name" (as-write id)
                               (and name "class") name)))
                inherit-field-names)
      
      ;; Check that superclass has expected methods, and get indices
      (let ([get-indices
             (lambda (method-ht what ids)
               (map
                (lambda (id)
                  (hash-ref 
                   method-ht id
                   (lambda ()
                     (obj-error 'class* 
                                (format "~a does not provide an expected method for ~a" 
                                        (if (eq? method-ht super-method-ht) "superclass" "class")
                                        what)
                                (format "~a name" what) (as-write id)
                                #:class-name name))))
                ids))]
            [method-width (+ (class-method-width super) (length public-names))]
            [field-width (+ (class-field-width super) num-fields)]
            [field-pub-width (+ (class-field-pub-width super) (length public-field-names))])
        (let ([inherit-indices (get-indices super-method-ht "inherit" inherit-names)]
              [replace-augonly-indices (get-indices super-method-ht "overment" overment-names)]
              [replace-final-indices (get-indices super-method-ht "override-final" override-final-names)]
              [replace-normal-indices (get-indices super-method-ht "override" override-normal-names)]
              [refine-augonly-indices (get-indices super-method-ht "augment" augment-names)]
              [refine-final-indices (get-indices super-method-ht "augment-final" augment-final-names)]
              [refine-normal-indices (get-indices super-method-ht "augride" augride-normal-names)]
              [rename-super-indices (get-indices super-method-ht "rename-super" rename-super-names)]
              [rename-inner-indices (get-indices method-ht "rename-inner" rename-inner-names)]
              [new-augonly-indices (get-indices method-ht "pubment" pubment-names)]
              [new-final-indices (get-indices method-ht "public-final" public-final-names)]
              [new-normal-indices (get-indices method-ht "public" public-normal-names)]
              [new-abstract-indices (get-indices method-ht "abstract" abstract-names)])
          
          ;; -- Check that all interfaces are satisfied --
          (for-each
           (lambda (intf)
             (for-each
              (lambda (var)
                (unless (hash-ref method-ht var #f)
                  (obj-error 'class* 
                             "missing interface-required method"
                             "method name" (as-write var)
                             (and name "class name") (as-write name)
                             (and (interface-name intf) "interface name") (as-write (interface-name intf)))))
              (interface-public-ids intf)))
           interfaces)
          (let ([c (get-implement-requirement interfaces 'class* #:class-name name)])
            (when (and c (not (subclass? super c)))
              (obj-error 'class* 
                         "interface-required implementation not satisfied"
                         (and name "class name") (as-write name)
                         (and (class-name c) "required class name") (as-write (class-name c)))))
          
          ;; -- For serialization, check that the superclass is compatible --
          (when deserialize-id
            (unless (class-serializer super)
              (obj-error 'class*
                         "superclass is not serialiazable, not transparent, and does not implement externalizable<%>"
                         "superclass" super
                         #:class-name name)))
          
          ;; ---- Make the class and its interface ----
          (let* ([class-make (if name
                                 (make-naming-constructor struct:class name "class")
                                 make-class)]
                 [interface-make (if name
                                     (make-naming-constructor 
                                      struct:interface
                                      (string->symbol (format "interface:~a" name))
                                      #f)
                                     make-interface)]
                 [method-names (append (reverse public-names) super-method-ids)]
                 [field-names (append public-field-names super-field-ids)]
                 ;; Superclass abstracts that have not been concretized
                 [remaining-abstract-names
                  (append abstract-names
                          (remq* override-names super-abstract-ids))]
                 [super-interfaces (cons (class-self-interface super) interfaces)]
                 [i (interface-make name super-interfaces #f method-names (make-immutable-hash) #f null)]
                 [methods (if no-method-changes?
                              (class-methods super)
                              (make-vector method-width))]
                 [super-methods (if no-method-changes?
                                    (class-super-methods super)
                                    (make-vector method-width))]
                 [int-methods (if no-method-changes?
                                  (class-int-methods super)
                                  (make-vector method-width))]
                 [beta-methods (if no-method-changes?
                                   (class-beta-methods super)
                                   (make-vector method-width))]
                 [inner-projs (if no-method-changes?
                                  (class-inner-projs super)
                                  (make-vector method-width))]
                 [dynamic-idxs (if no-method-changes?
                                   (class-dynamic-idxs super)
                                   (make-vector method-width))]
                 [dynamic-projs (if no-method-changes?
                                    (class-dynamic-projs super)
                                    (make-vector method-width))]
                 [meth-flags (if no-method-changes?
                                 (class-meth-flags super)
                                 (make-vector method-width))]
                 [c (class-make name
                                (add1 (class-pos super))
                                (list->vector (append (vector->list (class-supers super)) (list #f)))
                                i
                                (let-values ([(struct: make- ? -ref -set) (make-struct-type 'insp #f 0 0 #f null inspector)])
                                  make-)
                                method-width method-ht method-names remaining-abstract-names
                                (interfaces->contracted-methods (list i))
                                #f
                                methods super-methods int-methods beta-methods meth-flags
                                inner-projs dynamic-idxs dynamic-projs
                                field-width field-pub-width field-ht field-names
                                'struct:object 'object? 'make-object 'field-ref 'field-set!
                                init-args
                                init-mode
                                'init
                                #f #f #f ; serializer is set later
                                (and make-struct:prim #t))]
                 [obj-name (if name
                               (string->symbol (format "object:~a" name))
                               'object)]
                 ;; Used only for prim classes
                 [preparer (lambda (name)
                             ;; Map symbol to number:
                             (hash-ref method-ht name))]
                 [dispatcher (lambda (obj n)
                               ;; Extract method:
                               (vector-ref (class-methods (object-ref obj)) n))])
            
            (setup-all-implemented! i)
            (vector-set! (class-supers c) (add1 (class-pos super)) c)
            (set-class-orig-cls! c c)
            
            ;; --- Make the new object struct ---
            (let*-values ([(prim-object-make prim-object? struct:prim-object)
                           (if make-struct:prim
                               (make-struct:prim c prop:object 
                                                 preparer dispatcher
                                                 (get-properties interfaces))
                               (values #f #f #f))]
                          [(struct:object object-make object? object-field-ref object-field-set!)
                           (if make-struct:prim
                               ;; Use prim struct:
                               (values struct:prim-object prim-object-make prim-object? #f #f)
                               ;; Normal struct creation:
                               (make-struct-type obj-name
                                                 (add-properties (class-struct:object super) interfaces)
                                                 0 ;; No init fields
                                                 ;; Fields for new slots:
                                                 num-fields undefined
                                                 ;; Map object property to class:
                                                 (append
                                                  (list (cons prop:object c))
                                                  (if deserialize-id
                                                      (list
                                                       (cons prop:serializable
                                                             ;; Serialization:
                                                             (make-serialize-info
                                                              (lambda (obj) 
                                                                ((class-serializer c) obj))
                                                              deserialize-id
                                                              (and (not inspector)
                                                                   (not (interface-extension? i externalizable<%>))
                                                                   (eq? #t (class-serializer super)))
                                                              (or (current-load-relative-directory) 
                                                                  (current-directory)))))
                                                      null))
                                                 inspector))])
              (set-class-struct:object! c struct:object)
              (set-class-object?! c object?)
              (set-class-make-object! c object-make)
              (unless (zero? num-fields)
                ;; We need these only if there are fields, used for for public-field
                ;; access or for inspection:
                (set-class-field-ref! c object-field-ref)
                (set-class-field-set!! c object-field-set!))
              
              ;; --- Build field accessors and mutators ---
              ;;  Use public field names to name the accessors and mutators
              (let-values ([(inh-accessors inh-mutators)
                            (for/lists (accs muts) ([id (in-list inherit-field-names)])
                              (let ([fi (hash-ref field-ht id)])
                                (values (field-info-internal-ref fi) (field-info-internal-set! fi))))])
                ;; Add class/index pairs for public fields.
                (unless no-new-fields?
                  (for ([id (in-list public-field-names)]
                        [i (in-naturals)])
                    (hash-set! field-ht id (make-field-info c i))))
                
                ;; -- Extract superclass methods and make rename-inners ---
                (let ([rename-supers (map (lambda (index mname)
                                              ;; While the last part of the vector is indeed the right
                                              ;; method, if there have been super contracts placed since,
                                              ;; they won't be reflected there, only in the super-methods
                                              ;; vector of the superclass.
                                            (let ([vec (vector-ref (class-beta-methods super) index)])
                                              (when (and (positive? (vector-length vec))
                                                         (not (vector-ref vec (sub1 (vector-length vec)))))
                                                (obj-error 'class* 
                                                           (string-append
                                                            "superclass method for override, overment, inherit/super, "
                                                            "or rename-super is not overrideable")
                                                           "superclass" super
                                                           "method name" (as-write mname)
                                                           #:class-name name)))
                                            (vector-ref (class-super-methods super) index))
                                          rename-super-indices
                                          rename-super-names)]
                      [rename-inners (let ([new-augonly (make-vector method-width #f)])
                                       (define (get-depth index)
                                         (+ (if (index . < . (class-method-width super))
                                                (vector-length (vector-ref (class-beta-methods super) 
                                                                           index))
                                                0)
                                            (if (vector-ref new-augonly index) 0 -1)))
                                       ;; To compute `rename-inner' indices, we need to know which methods
                                       ;;  are augonly in this new class.
                                       (for-each (lambda (id)
                                                   (vector-set! new-augonly (hash-ref method-ht id) #t))
                                                 (append pubment-names overment-names))
                                       (let ([check-aug
                                              (lambda (maybe-here?)
                                                (lambda (mname index)
                                                  (let ([aug-ok?
                                                         (or (if (index . < . (class-method-width super))
                                                                 (eq? (vector-ref (class-meth-flags super) index) 'augmentable)
                                                                 #f)
                                                             (and maybe-here?
                                                                  (or (memq mname pubment-names)
                                                                      (memq mname overment-names))))])
                                                    (unless aug-ok?
                                                      (obj-error 'class* 
                                                                 (string-append
                                                                  "superclass method for augride, augment, inherit/inner, "
                                                                  "or rename-inner method is not augmentable")
                                                                 "superclass" super
                                                                 "method name" (as-write mname)
                                                                 #:class-name name)))))])
                                         (for-each (check-aug #f) 
                                                   augride-normal-names
                                                   (get-indices method-ht "augride" augride-normal-names))
                                         (for-each (check-aug #f) 
                                                   augment-final-names
                                                   refine-final-indices)
                                         (for-each (check-aug #t) 
                                                   rename-inner-names
                                                   rename-inner-indices))
                                       ;; Now that checking is done, add `augment':
                                       (for-each (lambda (id)
                                                   (vector-set! new-augonly (hash-ref method-ht id) #t))
                                                 augment-names)
                                       (map (lambda (mname index)
                                              (let ([depth (get-depth index)])
                                                (lambda (obj)
                                                  (vector-ref (vector-ref (class-beta-methods (object-ref obj)) 
                                                                          index)
                                                              depth))))
                                            rename-inner-names
                                            rename-inner-indices))])
                  
                  ;; Have to update these before making the method-accessors, since this is a "static" piece
                  ;; of information (instead of being dynamic => method call time).
                  (unless no-method-changes?
                    (vector-copy! dynamic-idxs 0 (class-dynamic-idxs super))
                    (for-each (lambda (index)
                                (vector-set! dynamic-idxs index 0))
                              (append new-augonly-indices new-final-indices
                                      new-normal-indices new-abstract-indices)))
                  
                  ;; -- Create method accessors --
                  (let ([method-accessors
                         (map (lambda (index)
                                (let ([dyn-idx (vector-ref dynamic-idxs index)])
                                  (lambda (obj)
                                    (vector-ref (vector-ref (class-int-methods (object-ref obj))
                                                            index)
                                                dyn-idx))))
                              (append new-normal-indices replace-normal-indices refine-normal-indices
                                      replace-augonly-indices refine-augonly-indices
                                      replace-final-indices refine-final-indices
                                      new-abstract-indices inherit-indices))])
                    
                    ;; -- Get new methods and initializers --
                    (let-values ([(new-methods override-methods augride-methods init)
                                  (apply make-methods object-field-ref object-field-set!
                                         (append inh-accessors
                                                 inh-mutators
                                                 rename-supers
                                                 rename-inners
                                                 method-accessors))])
                      ;; -- Fill in method tables --
                      ;;  First copy old methods
                      (unless no-method-changes?
                        (vector-copy! methods 0 (class-methods super))
                        (vector-copy! super-methods 0 (class-super-methods super))
                        (vector-copy! int-methods 0 (class-int-methods super))
                        (vector-copy! beta-methods 0 (class-beta-methods super))
                        (vector-copy! meth-flags 0 (class-meth-flags super))
                        (vector-copy! inner-projs 0 (class-inner-projs super))
                        (vector-copy! dynamic-projs 0 (class-dynamic-projs super)))
                      ;; Add new methods:
                      (for-each (lambda (index method)
                                  (vector-set! methods index method)
                                  (vector-set! super-methods index method)
                                  (vector-set! int-methods index (vector method))
                                  (vector-set! beta-methods index (vector))
                                  (vector-set! inner-projs index values)
                                  (vector-set! dynamic-idxs index 0)
                                  (vector-set! dynamic-projs index (vector values)))
                                (append new-augonly-indices new-final-indices
                                        new-abstract-indices new-normal-indices)
                                new-methods)
                      ;; Add only abstracts, making sure the super method just calls (void)
                      (let ([dummy (lambda args (void))])
                        (for-each (lambda (index)
                                    (vector-set! super-methods index dummy))
                                  new-abstract-indices))
                      ;; Override old methods:
                      (for-each (lambda (index method id)
                                  (when (eq? 'final (vector-ref meth-flags index))
                                    (obj-error 'class* 
                                               "cannot override or augment final method"
                                               "method name" (as-write id)
                                               #:class-name name))
                                  (let ([v (vector-ref beta-methods index)])
                                    (if (zero? (vector-length v))
                                        ;; Normal mode - set vtable entry
                                        (begin (vector-set! methods index method)
                                               (vector-set! super-methods index method)
                                               (let* ([dyn-idx (vector-ref dynamic-idxs index)]
                                                      [new-vec (make-vector (add1 dyn-idx))]
                                                      [proj-vec (vector-ref dynamic-projs index)])
                                                 (let loop ([n dyn-idx] [m method])
                                                   (if (< n 0)
                                                       (void)
                                                       (let* ([p (vector-ref proj-vec n)]
                                                              [new-m (make-method (p m) id)])
                                                         (vector-set! new-vec n new-m)
                                                         (loop (sub1 n) new-m)))
                                                 (vector-set! int-methods index new-vec))))
                                        ;; Under final mode - set extended vtable entry
                                        (let ([v (list->vector (vector->list v))])
                                          (vector-set! super-methods index method)
                                          (vector-set! v (sub1 (vector-length v))
                                                       ;; Apply current inner contract projection
                                                       (make-method ((vector-ref inner-projs index) method) id))
                                          (vector-set! beta-methods index v))))
                                  (when (not (vector-ref meth-flags index))
                                    (vector-set! meth-flags index (not make-struct:prim))))
                                (append replace-augonly-indices replace-final-indices replace-normal-indices
                                        refine-augonly-indices refine-final-indices refine-normal-indices)
                                (append override-methods augride-methods)
                                (append override-names augride-names))
                      ;; Update 'augmentable flags:
                      (unless no-method-changes?
                        (for-each (lambda (id)
                                    (vector-set! meth-flags (hash-ref method-ht id) 'augmentable))
                                  (append overment-names pubment-names))
                        (for-each (lambda (id)
                                    (vector-set! meth-flags (hash-ref method-ht id) #t))
                                  augride-normal-names))
                      ;; Expand `rename-inner' vector, adding a #f to indicate that
                      ;;  no rename-inner function is available, so far
                      (for-each (lambda (id)
                                  (let ([index (hash-ref method-ht id)])
                                    (let ([v (list->vector (append (vector->list (vector-ref beta-methods index))
                                                                   (list #f)))])
                                      ;; Since this starts a new part of the chain, reset the projection.
                                      (vector-set! inner-projs index values)
                                      (vector-set! beta-methods index v))))
                                augonly-names)
                      ;; Mark final methods:
                      (for-each (lambda (id)
                                  (let ([index (hash-ref method-ht id)])
                                    (vector-set! meth-flags index 'final)))
                                final-names)
                      ;; Handle interface contracted methods:
                      (for-each (lambda (id)
                                  (let ([index (hash-ref method-ht id)]
                                        [blame `(class ,name)])
                                    ;; Store blame information that will be instantiated later
                                    (define ictc-infos (get-interface-contract-info
                                                         (class-self-interface c) id))
                                    (define meth-entry (vector-ref methods index))
                                    (define meth (if (pair? meth-entry)
                                                     (car meth-entry)
                                                     meth-entry))
                                    (vector-set! methods index
                                                 (list meth
                                                       ;; Replace #f positive parties w/ this class
                                                       (replace-ictc-blame ictc-infos #t blame)))))
                                (class-method-ictcs c))
                      
                      ;; --- Install serialize info into class --
                      (set-class-serializer!
                       c
                       (cond
                         [(interface-extension? i externalizable<%>)
                          (let ([index (car (get-indices method-ht "???" '(externalize)))])
                            (lambda (obj)
                              (vector ((vector-ref methods index) obj))))]
                         [(and (or deserialize-id
                                   (not inspector))
                               (class-serializer super))
                          => (lambda (ss)
                               (lambda (obj)
                                 (vector (cons (ss obj)
                                               (let loop ([i 0])
                                                 (if (= i num-fields)
                                                     null
                                                     (cons (object-field-ref obj i)
                                                           (loop (add1 i)))))))))]
                         [else #f]))
                      
                      (set-class-fixup!
                       c
                       ;; Used only for non-externalizable:
                       (lambda (o args)
                         (if (pair? args)
                             (begin
                               ((class-fixup super) o (vector-ref (car args) 0))
                               (let loop ([i 0][args (cdr args)])
                                 (unless (= i num-fields)
                                   (object-field-set! o i (car args))
                                   (loop (add1 i) (cdr args)))))
                             (begin
                               ((class-fixup super) o args)
                               (let loop ([i 0])
                                 (unless (= i num-fields)
                                   (object-field-set! o i (object-field-ref args i))
                                   (loop (add1 i))))))))
                      
                      ;; --- Install initializer into class ---
                      (set-class-init! c init)
                      
                      ;; -- result is the class, and maybe deserialize-info ---
                      (if deserialize-id
                          (values c (make-deserialize-info
                                     (if (interface-extension? i externalizable<%>)
                                         (lambda (args)
                                           (let ([o (make-object c)])
                                             (send o internalize args)
                                             o))
                                         (lambda (args)
                                           (let ([o (object-make)])
                                             ((class-fixup c) o args)
                                             o)))
                                     (if (interface-extension? i externalizable<%>)
                                         (lambda ()
                                           (obj-error 'deserialize 
                                                      "cannot deserialize instance with cycles"
                                                      #:class-name name))
                                         (lambda ()
                                           (let ([o (object-make)])
                                             (values o
                                                     (lambda (o2)
                                                       ((class-fixup c) o o2))))))))
                          c))))))))))))

;; (listof interface?) -> (listof symbol?)
;; traverse the interfaces and figure out contracted methods
(define (interfaces->contracted-methods loi)
  (define immediate-methods
    (map (λ (ifc) (hash-keys (interface-contracts ifc))) loi))
  (define super-methods
    (map (λ (ifc) (interfaces->contracted-methods (interface-supers ifc))) loi))
  (remove-duplicates (apply append (append immediate-methods super-methods)) eq?))

#|
An example

(define (c1 x) #t)
(define (c2 x) #t)
(define (c3 x) #t)
(define (c4 x) #t)
(define (c5 x) #t)
(define (c6 x) #t)
(define (c7 x) #t)
(define (c8 x) #t)

(define i1
  (interface () [x c1]))
(define i2
  (interface (i1) [x c2]))
(define i3
  (interface (i1) [x c3]))
(define i4
  (interface (i2 i3) [x c4]))
(define i5
  (interface (i3) [x c5]))
(define i6
  (interface (i2) [x c6]))
(define i7
  (interface (i4 i5) [x c7]))
(define i8
  (interface (i6 i7) [x c8]))

(get-interface-contract-info i8 'x)

 '((#<procedure:c8> i8 #f i8) (#<procedure:c6> i6 i8 i6)
   (#<procedure:c2> i2 i6 i2) (#<procedure:c1> i1 i2 #f)

   (#<procedure:c7> i7 i8 i7) (#<procedure:c4> i4 i7 i4)

   (#<procedure:c3> i3 i4 i3)

   (#<procedure:c5> i5 i7 i5))
|#
;; interface symbol -> (listof (list contract name (or blame #f) (or blame #f)))
;; traverse hierarchy to find ctc/blame info for a given method
(define (get-interface-contract-info ifc meth)
  ;; recur on hierarchy
  (define super-infos
    (apply append (map (λ (ifc) (get-interface-contract-info ifc meth))
                       (interface-supers ifc))))
  ;; deduplicate the infos we get
  (define dedup-infos
    (let loop ([infos super-infos])
     (if (null? infos)
         '()
         (cons (car infos)
               (loop (remove* (list (car infos))
                         (cdr infos)
                         (λ (i1 i2) (eq? (car i1) (car i2)))))))))
  (define our-ctc (hash-ref (interface-contracts ifc) meth #f))
  (define our-ctcs (hash-keys (interface-contracts ifc)))
  (define our-name `(interface ,(interface-name ifc)))
  (cond ;; if we don't have the contract, the parent's info is fine
        [(not our-ctc) dedup-infos]
        ;; if the parent's don't contract it, then it's just our ctc
        [(null? dedup-infos) (list (list our-ctc our-name #f #f))]
        ;; our ctc should have a negative party of ourself (for behav. subtyping)
        [else (cons (list our-ctc our-name #f our-name)
                    ;; replace occurrences of #f positive blame with this interface
                    (map (λ (info)
                            (if (not (caddr info))
                                (list (car info) (cadr info) our-name (cadddr info))
                                info))
                         dedup-infos))]))

;; infos bool blame -> infos
;; replace either positive or negative parties that are #f with blame
(define (replace-ictc-blame infos pos? blame)
  (if pos?
      (for/list ([info infos])
        (list (car info) (cadr info) (or (caddr info) blame) (cadddr info)))
      (for/list ([info infos])
        (list (car info) (cadr info) (caddr info) (or (cadddr info) blame)))))

(define (check-still-unique name syms what)
  (let ([ht (make-hasheq)])
    (for-each (lambda (s)
                (when (hash-ref ht s 
                                (lambda ()
                                  (hash-set! ht s #t)
                                  #f))
                  (obj-error 'class* (format "external ~a mapped to overlapping keys"
                                             what)
                             #:class-name name)))
              syms)))

(define (get-properties intfs)
  (if (ormap (lambda (i)
               (pair? (interface-properties i)))
             intfs)
      (let ([ht (make-hash)])
        ;; Hash on gensym to avoid providing the same property multiple
        ;; times when it originated from a single interface.
        (for-each (lambda (i)
                    (for-each (lambda (p)
                                (hash-set! ht (vector-ref p 0) p))
                              (interface-properties i)))
                  intfs)
        (hash-map ht (lambda (k v) (cons (vector-ref v 1)
                                         (vector-ref v 2)))))
      ;; No properties to add:
      null))

(define (add-properties struct-type intfs)
  (let ([props (get-properties intfs)])
    (if (null? props)
        struct-type
        ;; Create a new structure type to house the properties, so
        ;; that they can't see any fields directly via guards:
        (let-values ([(struct: make- ? -ref -set!)
                      (make-struct-type 'props struct-type 0 0 #f props #f)])
          struct:))))

(define-values (prop:object object? object-ref) (make-struct-type-property 'object 'can-impersonate))

;;--------------------------------------------------------------------
;;  class/c
;;--------------------------------------------------------------------

;; Shorthand contracts that treat the implicit object argument as if it were
;; contracted with any/c.
(define-syntax-rule (->m . stx)
  (syntax-parameterize ([making-a-method #t] [method-contract? #t]) (-> . stx)))

(define-syntax-rule (->*m . stx)
  (syntax-parameterize ([making-a-method #t] [method-contract? #t]) (->* . stx)))

(define-syntax-rule (case->m . stx)
  (syntax-parameterize ([making-a-method #t] [method-contract? #t]) (case-> . stx)))

(define-syntax-rule (->dm . stx)
  (syntax-parameterize ([making-a-method #'this-param] [method-contract? #t]) (->d . stx)))

(define (class/c-check-first-order ctc cls fail)
  (unless (class? cls)
    (fail '(expected: "a class" given: "~v") cls))
  (let ([method-ht (class-method-ht cls)]
        [beta-methods (class-beta-methods cls)]
        [meth-flags (class-meth-flags cls)]) 
    (for ([m (class/c-methods ctc)])
      (unless (hash-ref method-ht m #f)
        (fail "no public method ~a" m)))
    (unless (class/c-opaque? ctc)
      (for ([m (class/c-absents ctc)])
        (when (hash-ref method-ht m #f)
          (fail "class already contains public method ~a" m))))
    (when (class/c-opaque? ctc)
      (for ([m (in-hash-keys method-ht)])
        (unless (memq m (class/c-methods ctc))
          (if (not (symbol-interned? m))
              (fail "some local member not specified in contract")
              (fail "method ~a not specified in contract" m)))))
    (for ([m (class/c-inherits ctc)])
      (unless (hash-ref method-ht m #f)
        (fail "no public method ~a" m)))
    (for ([m (class/c-overrides ctc)])
      (let ([index (hash-ref method-ht m #f)])
        (unless index
          (fail "no public method ~a" m))
        (let ([vec (vector-ref beta-methods index)])
          (unless (zero? (vector-length vec))
            (fail "method ~a was previously augmentable" m)))
        (let ([flag (vector-ref meth-flags index)])
          (when (eq? flag 'final)
            (fail "method ~a is final" m)))))
    (for ([m (class/c-augments ctc)])
      (let ([index (hash-ref method-ht m #f)])
        (unless index
          (fail "no public method ~a" m))
        (let* ([vec (vector-ref beta-methods index)])
          (when (zero? (vector-length vec))
            (fail "method ~a has never been augmentable" m))
          (when (vector-ref vec (sub1 (vector-length vec)))
            (fail "method ~a is currently overrideable, not augmentable" m)))))
    (for ([m (class/c-augrides ctc)])
      (let ([index (hash-ref method-ht m #f)])
        (unless index
          (fail "no public method ~a" m))
        (let ([vec (vector-ref beta-methods index)])
          (when (zero? (vector-length vec))
            (fail "method ~a has never been augmentable" m))
          (unless (vector-ref vec (sub1 (vector-length vec)))
            (fail "method ~a is currently augmentable, not overrideable" m)))))
    (for ([s (class/c-supers ctc)])
      (let ([index (hash-ref method-ht s #f)])
        (unless index
          (fail "no public method ~a" s))
        (let ([flag (vector-ref meth-flags index)])
          (when (eq? flag 'final)
            (fail "method ~a is final" s))
          (when (eq? flag 'augmentable)
            (fail "method ~a is augmentable, not overrideable" s)))))
    (for ([i (class/c-inners ctc)])
      (let ([index (hash-ref method-ht i #f)])
        (unless index
          (fail "no public method ~a" i))
        (let ([vec (vector-ref beta-methods index)])
          (when (zero? (vector-length vec))
            (fail "method ~a has never been augmentable" i)))
        (let ([flag (vector-ref meth-flags index)])
          (when (eq? flag 'final)
            (fail "method ~a is final" i)))))
    (let ([field-ht (class-field-ht cls)])
      (for ([f (class/c-fields ctc)])
        (unless (hash-ref field-ht f #f)
          (fail "no public field ~a" f)))
      (unless (class/c-opaque? ctc)
        (for ([f (class/c-absent-fields ctc)])
          (when (hash-ref field-ht f #f)
            (fail "class already contains public field ~a" f))))
      (when (class/c-opaque? ctc)
        (for ([f (in-hash-keys field-ht)])
          (unless (memq f (class/c-fields ctc))
            (if (not (symbol-interned? f))
                (fail "some local member not specified in contract")
                (fail "field ~a not specified in contract" f)))))
      (for ([f (class/c-inherit-fields ctc)])
        (unless (hash-ref field-ht f #f)
          (fail "no public field ~a" f)))))
  #t)

(define (class/c-proj ctc)
  (define ctc-methods (class/c-methods ctc))
  (define dynamic-features
    (append (class/c-overrides ctc)
            (class/c-augments ctc)
            (class/c-augrides ctc)
            (class/c-inherits ctc)))
  (define dynamic-contracts
    (append (class/c-override-contracts ctc)
            (class/c-augment-contracts ctc)
            (class/c-augride-contracts ctc)
            (class/c-inherit-contracts ctc)))
  (λ (blame)
    (define bswap (blame-swap blame))
    (define public-method-projections
      (for/list ([name (in-list ctc-methods)]
                 [c (in-list (class/c-method-contracts ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context blame name)))))
    (define super-projections
      (for/list ([name (in-list (class/c-supers ctc))]
                 [c (in-list (class/c-super-contracts ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context blame name)))))
    (define inner-projections
      (for/list ([name (in-list (class/c-inners ctc))]
                 [c (in-list (class/c-inner-contracts ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context bswap name)))))
    
    (define internal-field-projections
      (for/list ([f (in-list (class/c-fields ctc))]
                 [c (in-list (class/c-field-contracts ctc))])
        (and c
             (let ([p-pos ((contract-projection c) (blame-add-context blame (format "the ~a field in" f)))]
                   [p-neg ((contract-projection c) (blame-add-context bswap (format "the ~a field in" f)))])
               (cons p-pos p-neg)))))
    (define external-field-projections
      (for/list ([f (in-list (class/c-inherit-fields ctc))]
                 [c (in-list (class/c-inherit-field-contracts ctc))])
        (and c
             (let ([p-pos ((contract-projection c) blame)]
                   [p-neg ((contract-projection c) bswap)])
               (cons p-pos p-neg)))))
    
    (define override-projections
      (for/list ([m (in-list (class/c-overrides ctc))]
                 [c (in-list (class/c-override-contracts ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context bswap m)))))
    
    (define augment/augride-projections
      (for/list ([m (in-list (append (class/c-augments ctc)
                                     (class/c-augrides ctc)))]
                 [c (in-list (append (class/c-augment-contracts ctc)
                                     (class/c-augride-contracts ctc)))])
        (and c
             ((contract-projection c) (blame-add-method-context blame m)))))
    
    (define inherit-projections
      (for/list ([m (in-list (class/c-inherits ctc))]
                 [c (in-list (class/c-inherit-contracts ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context blame m)))))
    
    ;; zip the inits and contracts together for ordered selection
    (define inits+contracts 
      (for/list ([init (in-list (class/c-inits ctc))]
                 [ctc (in-list (class/c-init-contracts ctc))])
        (list init ((contract-projection ctc) bswap))))
    
    (λ (cls)
      (class/c-check-first-order ctc cls (λ args (apply raise-blame-error blame cls args)))
      (let* ([name (class-name cls)]
             [never-wrapped? (eq? (class-orig-cls cls) cls)]
             ;; Only add a new slot if we're not projecting an already contracted class.
             [supers (if never-wrapped?
                         (list->vector (append (vector->list (class-supers cls))
                                               (list #f)))                           
                         (list->vector (vector->list (class-supers cls))))]
             [pos (if never-wrapped?
                      (add1 (class-pos cls))
                      (class-pos cls))]
             [method-width (class-method-width cls)]
             [method-ht (class-method-ht cls)]
             [method-ictcs (class-method-ictcs cls)]
             [methods (if (null? ctc-methods)
                          (class-methods cls)
                          (make-vector method-width))]
             [super-methods (if (null? (class/c-supers ctc))
                                (class-super-methods cls)
                                (make-vector method-width))]
             [int-methods (if (null? dynamic-features)
                              (class-int-methods cls)
                              (make-vector method-width))]
             [inner-projs (if (null? (class/c-inners ctc))
                              (class-inner-projs cls)
                              (make-vector method-width))]
             [dynamic-idxs (if (null? dynamic-features)
                               (class-dynamic-idxs cls)
                               (make-vector method-width))]
             [dynamic-projs (if (null? dynamic-features)
                                (class-dynamic-projs cls)
                                (make-vector method-width))]
             [field-pub-width (class-field-pub-width cls)]
             [no-field-ctcs? (and (null? (class/c-fields ctc))
                                  (null? (class/c-inherit-fields ctc)))]
             [field-ht (if no-field-ctcs?
                           (class-field-ht cls)
                           (hash-copy (class-field-ht cls)))]
             [init (class-init cls)]
             [class-make (if name
                             (make-naming-constructor struct:class name "class")
                             make-class)]
             [c (class-make name
                            pos
                            supers
                            (class-self-interface cls)
                            void ;; No inspecting
                            
                            method-width
                            method-ht
                            (class-method-ids cls)
                            (class-abstract-ids cls)
                            (remq* ctc-methods method-ictcs)
                            
                            #f
                            
                            methods
                            super-methods
                            int-methods
                            (class-beta-methods cls)
                            (class-meth-flags cls)
                            
                            inner-projs
                            dynamic-idxs
                            dynamic-projs
                            
                            (class-field-width cls)
                            field-pub-width
                            field-ht
                            (class-field-ids cls)
                            
                            'struct:object 'object? 'make-object
                            'field-ref 'field-set!
                            
                            ;; class/c introduced subclasses do not consume init args
                            null
                            'normal
                            #f
                            
                            (class-orig-cls cls)
                            #f #f ; serializer is never set
                            #f)]
             [obj-name (if name
                           (string->symbol (format "object:~a" name))
                           'object)])
        (define (make-method proc meth-name)
          (procedure-rename
           (procedure->method proc)
           (string->symbol
            (format "~a method~a~a"
                    meth-name
                    (if name " in " "")
                    (or name "")))))
        
        (vector-set! supers pos c)
        
        ;; --- Make the new object struct ---
        (let-values ([(struct:object object-make object? object-field-ref object-field-set!)
                      (make-struct-type obj-name
                                        (class-struct:object cls)
                                        0 ;; No init fields
                                        0 ;; No new fields in this class replacement
                                        undefined
                                        ;; Map object property to class:
                                        (list (cons prop:object c)))])
          (set-class-struct:object! c struct:object)
          (set-class-object?! c object?)
          (set-class-make-object! c object-make)
          (set-class-field-ref! c object-field-ref)
          (set-class-field-set!! c object-field-set!))
        
        ;; Handle public method contracts
        (unless (null? ctc-methods)
          ;; First, fill in from old methods
          (vector-copy! methods 0 (class-methods cls))
          ;; Concretize any interface contracts handled by this ctc
          (unless (null? (class-method-ictcs cls))
            (for ([m (in-list (class-method-ictcs cls))])
              ;; only concretize if class/c takes responsibility for it
              (when (memq m ctc-methods)
                (define i (hash-ref method-ht m))
                (define entry (vector-ref methods i))
                ;; we're passing through a contract boundary, so the positive blame (aka
                ;; value server) is taking responsibility for any interface-contracted
                ;; methods)
                (define info (replace-ictc-blame (cadr entry) #f (blame-positive blame)))
                (vector-set! methods i (concretize-ictc-method m (car entry) info)))))
          ;; Now apply projections
          (for ([m (in-list ctc-methods)]
                [p (in-list public-method-projections)])
            (when p
              (define i (hash-ref method-ht m))
              (define mp (vector-ref methods i))
              (vector-set! methods i (make-method (p mp) m)))))
        
        ;; Handle super contracts
        (unless (null? (class/c-supers ctc))
          ;; First, fill in from old (possibly contracted) super methods
          (vector-copy! super-methods 0 (class-super-methods cls))
          ;; Now apply projections.
          (for ([m (in-list (class/c-supers ctc))]
                [p (in-list super-projections)])
            (when p
              (define i (hash-ref method-ht m))
              (define mp (vector-ref super-methods i))
              (vector-set! super-methods i (make-method (p mp) m)))))
        
        ;; Add inner projections
        (unless (null? (class/c-inners ctc))
          (vector-copy! inner-projs 0 (class-inner-projs cls))
          (for ([m (in-list (class/c-inners ctc))]
                [p (in-list inner-projections)])
            (when p
              (define i (hash-ref method-ht m))
              (define old-proj (vector-ref inner-projs i))
              (vector-set! inner-projs i (λ (v) (old-proj (p v)))))))
        
        ;; Handle both internal and external field contracts
        (unless no-field-ctcs?
          (for ([f (in-list (class/c-fields ctc))]
                [p-pr (in-list internal-field-projections)])
            (when p-pr
              (define fi (hash-ref field-ht f))
              (define p-pos (car p-pr))
              (define p-neg (cdr p-pr))
              (hash-set! field-ht f (field-info-extend-external fi p-pos p-neg))))
          (for ([f (in-list (class/c-inherit-fields ctc))]
                [p-pr (in-list external-field-projections)])
            (when p-pr
              (define fi (hash-ref field-ht f))
              (define p-pos (car p-pr))
              (define p-neg (cdr p-pr))
              (hash-set! field-ht f (field-info-extend-internal fi p-pos p-neg)))))
        
        ;; Now the trickiest of them all, internal dynamic dispatch.
        ;; First we update any dynamic indexes, as applicable.
        (let ([old-idxs (class-dynamic-idxs (class-orig-cls cls))])
          (unless (null? dynamic-features)
            ;; Go ahead and do all the copies here.
            (vector-copy! dynamic-projs 0 (class-dynamic-projs cls))
            (vector-copy! int-methods 0 (class-int-methods cls))
            (vector-copy! dynamic-idxs 0 (class-dynamic-idxs cls))
            (for ([m (in-list dynamic-features)]
                  [c (in-list dynamic-contracts)])
              (when c
                (let* ([i (hash-ref method-ht m)]
                       [old-idx (vector-ref old-idxs i)]
                       [new-idx (vector-ref dynamic-idxs i)])
                  ;; We need to extend all the vectors, so let's do that here.
                  (when (= old-idx new-idx)
                    (let* ([new-idx (add1 old-idx)]
                           [new-proj-vec (make-vector (add1 new-idx))]
                           [old-proj-vec (vector-ref dynamic-projs i)]
                           [new-int-vec (make-vector (add1 new-idx))]
                           [old-int-vec (vector-ref int-methods i)])
                      (vector-set! dynamic-idxs i new-idx)
                      (vector-copy! new-proj-vec 0 old-proj-vec)
                      (vector-set! new-proj-vec new-idx values)
                      (vector-set! dynamic-projs i new-proj-vec)
                      (vector-copy! new-int-vec 0 old-int-vec)
                      ;; Just copy over the last entry here.  We'll
                      ;; update it appropriately later.
                      (vector-set! new-int-vec new-idx
                                   (vector-ref old-int-vec old-idx))
                      (vector-set! int-methods i new-int-vec)))))))
          
          ;; Now we handle updating override contracts... here we just
          ;; update the projections, and not the methods (which we must
          ;; do during class composition).
          (unless (null? (class/c-overrides ctc))
            (for ([m (in-list (class/c-overrides ctc))]
                  [p (in-list override-projections)])
              (when p
                (let* ([i (hash-ref method-ht m)]
                       [old-idx (vector-ref old-idxs i)]
                       [proj-vec (vector-ref dynamic-projs i)]
                       [old-proj (vector-ref proj-vec old-idx)])
                  (vector-set! proj-vec old-idx (λ (v) (old-proj (p v))))))))
          
          ;; For augment and augride contracts, we both update the projection
          ;; and go ahead and apply the projection to the last slot (which will
          ;; only be used by later classes).
          (unless (and (null? (class/c-augments ctc))
                       (null? (class/c-augrides ctc)))
            (for ([m (in-list (append (class/c-augments ctc)
                                      (class/c-augrides ctc)))]
                  [p (in-list augment/augride-projections)])
              (when p
                (let* ([i (hash-ref method-ht m)]
                       [old-idx (vector-ref old-idxs i)]
                       [new-idx (vector-ref dynamic-idxs i)]
                       [proj-vec (vector-ref dynamic-projs i)]
                       [int-vec (vector-ref int-methods i)]
                       [old-proj (vector-ref proj-vec old-idx)])
                  (vector-set! proj-vec old-idx (λ (v) (p (old-proj v))))
                  (vector-set! int-vec new-idx
                               (make-method (p (vector-ref int-vec new-idx)) m))))))
          
          ;; Now (that things have been extended appropriately) we handle
          ;; inherits.
          (unless (null? (class/c-inherits ctc))
            (for ([m (in-list (class/c-inherits ctc))]
                  [p (in-list inherit-projections)])
              (when p
                (let* ([i (hash-ref method-ht m)]
                       [new-idx (vector-ref dynamic-idxs i)]
                       [int-vec (vector-ref int-methods i)])
                  (vector-set! int-vec new-idx
                               (make-method (p (vector-ref int-vec new-idx)) m)))))))
        
        ;; Unlike the others, we always want to do this, even if there are no init contracts,
        ;; since we still need to handle either calling the previous class/c's init or
        ;; calling continue-make-super appropriately.
        (let ()
          ;; grab all the inits+contracts that involve the same init arg
          ;; (assumes that inits and contracts were sorted in class/c creation)
          (define (grab-same-inits lst)
            (if (null? lst)
                (values null null)
                (let loop ([inits/c (cdr lst)]
                           [prefix (list (car lst))])
                  (cond
                    [(null? inits/c) 
                     (values (reverse prefix) inits/c)]
                    [(eq? (list-ref (car inits/c) 0) (list-ref (car prefix) 0))
                     (loop (cdr inits/c)
                           (cons (car inits/c) prefix))]
                    [else (values (reverse prefix) inits/c)]))))
          ;; run through the list of init-args and apply contracts for same-named
          ;; init args
          (define (apply-contracts inits/c init-args)
            (let loop ([init-args init-args]
                       [inits/c inits/c]
                       [handled-args null])
              (cond
                [(null? init-args)
                 (reverse handled-args)]
                [(null? inits/c)
                 (append (reverse handled-args) init-args)]
                [(eq? (list-ref (car inits/c) 0) (car (car init-args)))
                 (let ([init-arg (car init-args)]
                       [p (list-ref (car inits/c) 1)])
                   (loop (cdr init-args)
                         (cdr inits/c)
                         (cons (cons (car init-arg) (p (cdr init-arg)))
                               handled-args)))]
                [else (loop (cdr init-args)
                            inits/c
                            (cons (car init-args) handled-args))])))
          (set-class-init! 
           c
           (lambda (the-obj super-go si_c si_inited? si_leftovers init-args)
             (let ([init-args
                    (let loop ([inits/c inits+contracts]
                               [handled-args init-args])
                      (if (null? inits/c)
                          handled-args
                          (let-values ([(prefix suffix) (grab-same-inits inits/c)])
                            (loop suffix
                                  (apply-contracts prefix init-args)))))])
               ;; Since we never consume init args, we can ignore si_leftovers
               ;; since init-args is the same.
               (if never-wrapped?
                   (super-go the-obj si_c si_inited? init-args null null)
                   (init the-obj super-go si_c si_inited? init-args init-args))))))
        
        c))))

(define (blame-add-method-context blame name)
  (cond
    [(symbol? name)
     (blame-add-context blame 
                        (format "the ~a method in" name)
                        #:important
                        name)]
    [(not name)
     (blame-add-context blame "an unnamed method in")]
    [else (error 'blame-add-method-context "uhoh ~s" name)]))

(define-struct class/c 
  (methods method-contracts fields field-contracts inits init-contracts
   inherits inherit-contracts inherit-fields inherit-field-contracts
   supers super-contracts inners inner-contracts
   overrides override-contracts augments augment-contracts
   augrides augride-contracts absents absent-fields opaque? name)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection class/c-proj
   #:name 
   (λ (ctc)
     (or (class/c-name ctc)
         (let* ([pair-ids-ctcs
                 (λ (is ctcs)
                   (for/list ([i (in-list is)]
                              [ctc (in-list ctcs)])
                     (if (not ctc)
                         i
                         (build-compound-type-name i ctc))))]
                [handle-optional
                 (λ (name is ctcs)
                   (if (null? is)
                       null
                       (list (cons name (pair-ids-ctcs is ctcs)))))]
                [handle-absents
                 (λ (meths fields)
                   (cond
                     [(and (null? meths) (null? fields))
                      null]
                     [(null? fields)
                      (list (cons 'absent meths))]
                     [else
                      (list (list* 'absent (cons 'field fields) meths))]))]
                [handled-methods
                 (for/list ([i (in-list (class/c-methods ctc))]
                            [ctc (in-list (class/c-method-contracts ctc))])
                   (cond
                     [ctc (build-compound-type-name i ctc)]
                     [else i]))])
           (apply build-compound-type-name
                  'class/c 
                  (append
                   handled-methods
                   (handle-optional 'init (class/c-inits ctc) (class/c-init-contracts ctc))
                   (handle-optional 'field (class/c-fields ctc) (class/c-field-contracts ctc))
                   (handle-optional 'inherit (class/c-inherits ctc) (class/c-inherit-contracts ctc))
                   (handle-optional 'inherit-field (class/c-inherit-fields ctc) (class/c-inherit-field-contracts ctc))
                   (handle-optional 'super (class/c-supers ctc) (class/c-super-contracts ctc))
                   (handle-optional 'inner (class/c-inners ctc) (class/c-inner-contracts ctc))
                   (handle-optional 'override (class/c-overrides ctc) (class/c-override-contracts ctc))
                   (handle-optional 'augment (class/c-augments ctc) (class/c-augment-contracts ctc))
                   (handle-optional 'augride (class/c-augrides ctc) (class/c-augride-contracts ctc))
                   (handle-absents (class/c-absents ctc) (class/c-absent-fields ctc)))))))
   #:first-order
   (λ (ctc)
     (λ (cls)
       (let/ec ret
         (class/c-check-first-order ctc cls (λ args (ret #f))))))))

(define-for-syntax (parse-class/c-specs forms object/c?)
  (define parsed-forms (make-hasheq))
  (define form-name (if object/c? 'object/c 'class/c))
  (define (parse-name-ctc stx)
    (syntax-case stx ()
      [x
       (identifier? #'x)
       (with-syntax ([id (localize #'x)])
         (values #'`id #f))]
      [(x ctc)
       (identifier? #'x)
       (with-syntax ([id (localize #'x)])
         (values #'`id
                 #`(coerce-contract '#,form-name (let ([x ctc]) x))))]
      [_
       (raise-syntax-error form-name "expected identifier or (id contract)" stx)]))
  (define (parse-names-ctcs stx)
    (for/fold ([names null]
               [ctcs null])
      ([stx (in-list (syntax->list stx))])
      (let-values ([(name ctc) (parse-name-ctc stx)])
        (values (cons name names) (cons ctc ctcs)))))
  (define (parse-absents stx)
    (for/fold ([meths null]
               [fields null])
      ([stx (in-list (syntax->list stx))])
      (syntax-case stx (field)
        [(field f-id ...)
         (let ([symbols (for/list ([id (in-list (syntax->list #'(f-id ...)))])
                          (unless (identifier? id)
                            (raise-syntax-error 'class/c "expected identifier" stx))
                          (with-syntax ([id (localize id)])
                            #'`id))])
           (values meths (append (reverse symbols) fields)))]
        [id
         (identifier? #'id)
         (with-syntax ([id (localize #'id)])
           (values (cons #'`id meths) fields))]
        [_
         (raise-syntax-error 'class/c "expected identifier or (field id ...)" stx)])))
  (define (parse-spec stx)
    (syntax-case stx (field inherit inherit-field init init-field super inner override augment augride absent)
      [(field f-spec ...)
       (let-values ([(names ctcs) (parse-names-ctcs #'(f-spec ...))])
         (hash-set! parsed-forms 'fields
                    (append names (hash-ref parsed-forms 'fields null)))
         (hash-set! parsed-forms 'field-contracts
                    (append ctcs (hash-ref parsed-forms 'field-contracts null))))]
      [(init i-spec ...)
       (let-values ([(names ctcs) (parse-names-ctcs #'(i-spec ...))])
         (hash-set! parsed-forms 'inits
                    (append names (hash-ref parsed-forms 'inits null)))
         (hash-set! parsed-forms 'init-contracts
                    (append ctcs (hash-ref parsed-forms 'init-contracts null))))]
      [(init-field i-spec ...)
       (let-values ([(names ctcs) (parse-names-ctcs #'(i-spec ...))])
         (hash-set! parsed-forms 'inits
                    (append names (hash-ref parsed-forms 'inits null)))
         (hash-set! parsed-forms 'init-contracts
                    (append ctcs (hash-ref parsed-forms 'init-contracts null)))
         (hash-set! parsed-forms 'fields
                    (append names (hash-ref parsed-forms 'fields null)))
         (hash-set! parsed-forms 'field-contracts
                    (append ctcs (hash-ref parsed-forms 'field-contracts null))))]
      [(inherit m-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "inherit contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(m-spec ...))])
           (hash-set! parsed-forms 'inherits
                      (append names (hash-ref parsed-forms 'inherits null)))
           (hash-set! parsed-forms 'inherit-contracts
                      (append ctcs (hash-ref parsed-forms 'inherit-contracts null)))))]
      [(inherit-field f-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "inherit-field contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(f-spec ...))])
           (hash-set! parsed-forms 'inherit-fields
                      (append names (hash-ref parsed-forms 'inherit-fields null)))
           (hash-set! parsed-forms 'inherit-field-contracts
                      (append ctcs (hash-ref parsed-forms 'inherit-field-contracts null)))))]
      [(super s-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "super contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(s-spec ...))])
           (hash-set! parsed-forms 'supers
                      (append names (hash-ref parsed-forms 'supers null)))
           (hash-set! parsed-forms 'super-contracts
                      (append ctcs (hash-ref parsed-forms 'super-contracts null)))))]
      [(inner i-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "inner contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(i-spec ...))])
           (hash-set! parsed-forms 'inners
                      (append names (hash-ref parsed-forms 'inners null)))
           (hash-set! parsed-forms 'inner-contracts
                      (append ctcs (hash-ref parsed-forms 'inner-contracts null)))))]
      [(override o-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "override contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(o-spec ...))])
           (hash-set! parsed-forms 'overrides
                      (append names (hash-ref parsed-forms 'overrides null)))
           (hash-set! parsed-forms 'override-contracts
                      (append ctcs (hash-ref parsed-forms 'override-contracts null)))))]
      [(augment a-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "augment contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(a-spec ...))])
           (hash-set! parsed-forms 'augments
                      (append names (hash-ref parsed-forms 'augments null)))
           (hash-set! parsed-forms 'augment-contracts
                      (append ctcs (hash-ref parsed-forms 'augment-contracts null)))))]
      [(augride a-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "augride contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(a-spec ...))])
           (hash-set! parsed-forms 'augrides
                      (append names (hash-ref parsed-forms 'augrides null)))
           (hash-set! parsed-forms 'augride-contracts
                      (append ctcs (hash-ref parsed-forms 'augride-contracts null)))))]
      [(absent a-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "absent specification not allowed in object/c" stx))
         (let-values ([(meths fields) (parse-absents #'(a-spec ...))])
           (hash-set! parsed-forms 'absents
                      (append meths (hash-ref parsed-forms 'absents null)))
           (hash-set! parsed-forms 'absent-fields
                      (append fields (hash-ref parsed-forms 'absent-fields null)))))]
      [m-spec
       (let-values ([(name ctc1) (parse-name-ctc #'m-spec)])
         (hash-set! parsed-forms 'methods
                    (cons name (hash-ref parsed-forms 'methods null)))
         (hash-set! parsed-forms 'method-contracts
                    (cons ctc1 (hash-ref parsed-forms 'method-contracts null))))]
      [else
       (raise-syntax-error form-name "expected class/c subform" stx)]))
  (for ([form (in-list forms)])
    (parse-spec form))
  parsed-forms)

;; check keyword and pass off to -class/c
(define-syntax (class/c stx)

  (define-splicing-syntax-class opaque-keyword
    (pattern (~seq #:opaque) #:with opaque? #'#t)
    (pattern (~seq) #:with opaque? #'#f))

  (syntax-parse stx
    [(_ kwd:opaque-keyword form ...)
     (syntax/loc stx (-class/c kwd.opaque? form ...))]))

(define-syntax (-class/c stx)
  (syntax-case stx ()
    [(_ opaque? form ...)
     (let ([parsed-forms (parse-class/c-specs (syntax->list #'(form ...)) #f)])
       (with-syntax ([methods #`(list #,@(reverse (hash-ref parsed-forms 'methods null)))]
                     [method-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'method-contracts null)))]
                     [fields #`(list #,@(reverse (hash-ref parsed-forms 'fields null)))]
                     [field-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'field-contracts null)))]
                     [(i ...) (reverse (hash-ref parsed-forms 'inits null))]
                     [(i-c ...) (reverse (hash-ref parsed-forms 'init-contracts null))]
                     [inherits #`(list #,@(reverse (hash-ref parsed-forms 'inherits null)))]
                     [inherit-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'inherit-contracts null)))]
                     [inherit-fields #`(list #,@(reverse (hash-ref parsed-forms 'inherit-fields null)))]
                     [inherit-field-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'inherit-field-contracts null)))]
                     [supers #`(list #,@(reverse (hash-ref parsed-forms 'supers null)))]
                     [super-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'super-contracts null)))]
                     [inners #`(list #,@(reverse (hash-ref parsed-forms 'inners null)))]
                     [inner-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'inner-contracts null)))]
                     [overrides #`(list #,@(reverse (hash-ref parsed-forms 'overrides null)))]
                     [override-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'override-contracts null)))]
                     [augments #`(list #,@(reverse (hash-ref parsed-forms 'augments null)))]
                     [augment-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'augment-contracts null)))]
                     [augrides #`(list #,@(reverse (hash-ref parsed-forms 'augrides null)))]
                     [augride-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'augride-contracts null)))]
                     [absents #`(list #,@(reverse (hash-ref parsed-forms 'absents null)))]
                     [absent-fields #`(list #,@(reverse (hash-ref parsed-forms 'absent-fields null)))])
         (with-syntax ([name 
                        ;; same as syntax-local-infer-name, except doesn't
                        ;; make a name up from the src loc; in that case,
                        ;; we just use the big ole (class/c  ...)-based name
                        (or (let loop ([prop (syntax-property stx 'inferred-name)])
                              (cond
                                [(symbol? prop) prop]
                                [(pair? prop) (or (loop (car prop))
                                                  (loop (cdr prop)))]
                                [else #f]))
                            (syntax-local-name))])
           (syntax/loc stx
             (let-values ([(inits init-ctcs) (sort-inits+contracts (list (cons i i-c) ...))])
               (make-class/c methods method-ctcs
                             fields field-ctcs
                             inits init-ctcs
                             inherits inherit-ctcs
                             inherit-fields inherit-field-ctcs
                             supers super-ctcs
                             inners inner-ctcs
                             overrides override-ctcs
                             augments augment-ctcs
                             augrides augride-ctcs
                             absents absent-fields
                             opaque?
                             'name))))))]))

(define (sort-inits+contracts lst)
  (define sorted
    (sort lst
          string<?
          #:key (compose symbol->string car)))
  (values (map car sorted) (map cdr sorted)))

(define (check-object-contract obj methods fields fail)
  (unless (object? obj)
    (fail '(expected: "an object" given: "~e") obj))
  (let ([cls (object-ref obj)])
    (let ([method-ht (class-method-ht cls)])
      (for ([m methods])
        (unless (hash-ref method-ht m #f)
          (fail "no public method ~a" m))))
    (let ([field-ht (class-field-ht cls)])
      (for ([m fields])
        (unless (hash-ref field-ht m #f)
          (fail "no public field ~a" m)))))
  #t)

(define (object/c-proj ctc)
  (λ (blame)
    (λ (obj)
      (make-wrapper-object ctc obj blame 
                           (base-object/c-methods ctc) (base-object/c-method-contracts ctc)
                           (base-object/c-fields ctc) (base-object/c-field-contracts ctc)))))

(define (object/c-first-order ctc)
  (λ (obj)
    (let/ec ret
      (check-object-contract obj (base-object/c-methods ctc) (base-object/c-fields ctc) (λ args (ret #f))))))

(define-struct base-object/c (methods method-contracts fields field-contracts)
  #:property prop:contract
  (build-contract-property 
   #:projection object/c-proj
   #:name
   (λ (ctc)
     (let* ([pair-ids-ctcs
             (λ (is ctcs)
               (map (λ (i ctc)
                      (build-compound-type-name i ctc))
                    is ctcs))]
            [handle-optional
             (λ (name is ctcs)
               (if (null? is)
                   null
                   (list (cons name (pair-ids-ctcs is ctcs)))))])
       (apply build-compound-type-name
              'object/c 
              (append
               (pair-ids-ctcs (base-object/c-methods ctc) (base-object/c-method-contracts ctc))
               (handle-optional 'field (base-object/c-fields ctc) (base-object/c-field-contracts ctc))))))
   #:first-order object/c-first-order))

(define-syntax (object/c stx)
  (syntax-case stx ()
    [(_ form ...)
     (let ([parsed-forms (parse-class/c-specs (syntax->list #'(form ...)) #t)])
       (with-syntax ([methods #`(list #,@(reverse (hash-ref parsed-forms 'methods null)))]
                     [method-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'method-contracts null)))]
                     [fields #`(list #,@(reverse (hash-ref parsed-forms 'fields null)))]
                     [field-ctcs #`(list #,@(reverse (hash-ref parsed-forms 'field-contracts null)))])
         (syntax/loc stx
           (make-base-object/c methods method-ctcs fields field-ctcs))))]))

(define (instanceof/c-proj ctc)
  (let ([proj (contract-projection (base-instanceof/c-class-ctc ctc))])
    (λ (blame)
      (let ([p (proj blame)])
        (λ (val)
          (unless (object? val)
            (raise-blame-error blame val '(expected: "an object" given: "~e") val))
          (let ([original-obj (if (has-original-object? val) (original-object val) val)]
                [new-cls (p (object-ref val))])
            (impersonate-struct val object-ref (λ (o c) new-cls)
                                impersonator-prop:contracted ctc
                                impersonator-prop:original-object original-obj)))))))

(define (instanceof/c-first-order ctc)
  (let ([cls-ctc (base-instanceof/c-class-ctc ctc)])
    (λ (val)
      (and (object? val)
           (contract-first-order-passes? cls-ctc (object-ref val))))))

(define-struct base-instanceof/c (class-ctc)
  #:property prop:contract
  (build-contract-property 
   #:projection instanceof/c-proj
   #:name
   (λ (ctc)
     (build-compound-type-name 'instanceof/c (base-instanceof/c-class-ctc ctc)))
   #:first-order instanceof/c-first-order))

(define (instanceof/c cctc)
  (let ([ctc (coerce-contract 'instanceof/c cctc)])
    (make-base-instanceof/c ctc)))

;;--------------------------------------------------------------------
;;  interfaces
;;--------------------------------------------------------------------

;; >> Simplistic implementation for now <<

(define-for-syntax do-interface
  (lambda (stx m-stx)
    (syntax-case m-stx ()
      [((interface-expr ...) ([prop prop-val] ...) var ...)
       (let ([name (syntax-local-infer-name stx)])
         (define-values (vars ctcs)
           (for/fold ([vars '()] [ctcs '()])
                     ([v (syntax->list #'(var ...))])
             (syntax-case v ()
               [id
                (identifier? #'id)
                (values (cons #'id vars) (cons #f ctcs))]
               [(id ctc)
                (identifier? #'id)
                (values (cons #'id vars) (cons #'ctc ctcs))]
               [_ (raise-syntax-error #f "not an identifier or identifier-contract pair"
                                      stx v)])))
         (let ([dup (check-duplicate-identifier vars)])
           (when dup
             (raise-syntax-error #f
                                 "duplicate name"
                                 stx
                                 dup)))
         (with-syntax ([name (datum->syntax #f name #f)]
                       [(var ...) (map localize vars)]
                       [((v c) ...) (filter (λ (p) (cadr p)) (map list vars ctcs))])
           (syntax/loc
               stx
             (compose-interface
              'name
              (list interface-expr ...)
              `(var ...)
              (make-immutable-hash (list (cons 'v c) ...))
              (list prop ...)
              (list prop-val ...)))))])))

(define-syntax (_interface stx)
  (syntax-case stx ()
    [(_ (interface-expr ...) var ...)
     (do-interface stx #'((interface-expr ...) () var ...))]))

(define-syntax (interface* stx)
  (syntax-case stx ()
    [(_ (interface-expr ...) ([prop prop-val] ...) var ...)
     (do-interface stx #'((interface-expr ...) ([prop prop-val] ...) var ...))]
    [(_ (interface-expr ...) (prop+val ...) var ...)
     (for-each (lambda (p+v)
                 (syntax-case p+v ()
                   [(p v) (void)]
                   [_ (raise-syntax-error #f
                                          "expected `[<prop-expr> <val-expr>]'"
                                          stx
                                          p+v)]))
               (syntax->list #'(prop+val ...)))]
    [(_ (interface-expr ...) prop+vals . _)
     (raise-syntax-error #f
                         "expected `([<prop-expr> <val-expr>] ...)'"
                         stx
                         #'prop+vals)]))

(define-struct interface 
  (name             ; symbol
   supers           ; (listof interface)
   [all-implemented ; hash-table: interface -> #t
    #:mutable]
   public-ids       ; (listof symbol) (in any order?!?)
   contracts        ; (hashof symbol? contract?)
   [class           ; (union #f class) -- means that anything implementing
       #:mutable]      ; this interface must be derived from this class
   properties)      ; (listof (vector gensym prop val))
  #:inspector insp)

(define (compose-interface name supers vars ctcs props vals)
  (for-each
   (lambda (intf)
     (unless (interface? intf)
       (obj-error 'interface 
                  "superinterface expression result is not an interface" 
                  "result" intf
                  #:intf-name name)))
   supers)
  (for-each
   (lambda (p)
     (unless (struct-type-property? p)
       (obj-error 'interface
                  "property expression result is not a property"
                  "result" p
                  #:intf-name name)))
   props)
  (let ([ht (make-hasheq)])
    (for-each
     (lambda (var)
       (hash-set! ht var #t))
     vars)
    ;; Check that vars don't already exist in supers:
    (for-each
     (lambda (super)
       (for-each
        (lambda (var)
          (when (and (hash-ref ht var #f)
                     (not (hash-ref ctcs var #f)))
            (obj-error 'interface "variable already in superinterface" 
                       "variable name" (as-write var)
                       (and (interface-name super) "already in") (as-write (interface-name super))
                       #:intf-name name)))
        (interface-public-ids super)))
     supers)
    ;; merge properties:
    (let ([prop-ht (make-hash)])
      ;; Hash on gensym to avoid providing the same property multiple
      ;; times when it originated from a single interface.
      (for-each (lambda (i)
                  (for-each (lambda (p)
                              (hash-set! prop-ht (vector-ref p 0) p))
                            (interface-properties i)))
                supers)
      (for-each (lambda (p v)
                  (let ([g (gensym)])
                    (hash-set! prop-ht g (vector g p v))))
                props vals)
      ;; Check for [conflicting] implementation requirements
      (let ([class (get-implement-requirement supers 'interface #:intf-name name)]
            [interface-make (if name
                                (make-naming-constructor struct:interface 
                                                         name
                                                         "interface")
                                make-interface)])
        ;; Add supervars to table:
        (for-each
         (lambda (super)
           (for-each
            (lambda (var) (hash-set! ht var #t))
            (interface-public-ids super)))
         supers)
        ;; Done
        (let* ([new-ctcs (for/hash ([(k v) (in-hash ctcs)])
                           (values k (coerce-contract 'interface v)))]
               [i (interface-make name supers #f (hash-map ht (lambda (k v) k))
                                  new-ctcs class (hash-map prop-ht (lambda (k v) v)))])
          (setup-all-implemented! i)
          i)))))

;; setup-all-implemented! : interface -> void
;;  Creates the hash table for all implemented interfaces
(define (setup-all-implemented! i)
  (let ([ht (make-hasheq)])
    (hash-set! ht i #t)
    (for-each (lambda (si)
                (hash-for-each
                 (interface-all-implemented si)
                 (lambda (k v)
                   (hash-set! ht k #t))))
              (interface-supers i))
    (set-interface-all-implemented! i ht)))

(define (get-implement-requirement interfaces where 
                                   #:class-name [class-name #f]
                                   #:intf-name [intf-name #f])
  (let loop ([class #f]
             [supers interfaces])
    (if (null? supers)
        class
        (let ([c (interface-class (car supers))])
          (loop
           (cond
             [(not c) class]
             [(not class) c]
             [(subclass? c class) class]
             [(subclass? class c) c]
             [else
              (obj-error 
               where
               "conflicting class implementation requirements in superinterfaces"
               #:class-name class-name
               #:intf-name intf-name)])
           (cdr supers))))))

;;--------------------------------------------------------------------
;;  object%
;;--------------------------------------------------------------------

(define (make-naming-constructor type name prefix)
  (define (writeer obj port mode)
    (write-string "#<" port)
    (when prefix
      (write-string prefix port)
      (write-string ":" port))
    (write-string (symbol->string name) port)
    (write-string ">" port))
  (define props (list (cons prop:custom-write writeer)))
  (define-values (struct: make- ? -accessor -mutator)
    (make-struct-type name type 0 0 #f props insp))
  make-)

(define object<%> ((make-naming-constructor struct:interface 'interface:object% #f)
                   'object% null #f null (make-immutable-hash) #f null))
(setup-all-implemented! object<%>)
(define object% ((make-naming-constructor struct:class 'object% "class")
                 'object%
                 0 (vector #f) 
                 object<%>
                 void ; never inspectable
                 
                 0 (make-hasheq) null null null
                 #f
                 (vector) (vector) (vector) (vector) (vector)

                 (vector) (vector) (vector)
                 
                 0 0 (make-hasheq) null
                 
                 'struct:object object? 'make-object
                 'field-ref-not-needed 'field-set!-not-needed
                 
                 null
                 'normal
                 
                 (lambda (this super-init si_c si_inited? si_leftovers args) 
                   (unless (null? args)
                     (unused-args-error this args))
                   (void))
                 
                 #f
                 (lambda (obj) #(()))        ; serialize
                 (lambda (obj args) (void))  ; deserialize-fixup
                 
                 #t)) ; no super-init

(vector-set! (class-supers object%) 0 object%)
(set-class-orig-cls! object% object%)
(let*-values ([(struct:obj make-obj obj? -get -set!)
               (make-struct-type 'object #f 0 0 #f (list (cons prop:object object%)) #f)])
  (set-class-struct:object! object% struct:obj)
  (set-class-make-object! object% make-obj))
(set-class-object?! object% object?) ; don't use struct pred; it wouldn't work with prim classes

(set-interface-class! object<%> object%)

;;--------------------------------------------------------------------
;;  instantiation
;;--------------------------------------------------------------------

(define-syntax (new stx)
  (syntax-case stx ()
    [(_ cls (id arg) ...)
     (andmap identifier? (syntax->list (syntax (id ...))))
     (quasisyntax/loc stx
       (instantiate cls () (id arg) ...))]
    [(_ cls (id arg) ...)
     (for-each (lambda (id)
                 (unless (identifier? id)
                   (raise-syntax-error 'new "expected identifier" stx id)))
               (syntax->list (syntax (id ...))))]
    [(_ cls pr ...)
     (for-each
      (lambda (pr)
        (syntax-case pr ()
          [(x y) (void)]
          [else (raise-syntax-error 'new "expected name and value binding" stx pr)]))
      (syntax->list (syntax (pr ...))))]))

(define ((make-object/proc blame) class . args)
  (do-make-object blame class args null))

(define-syntax make-object
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx ()
             [id
              (identifier? #'id)
              (quasisyntax/loc stx
                (make-object/proc (current-contract-region)))]
             [(_ class arg ...)
              (quasisyntax/loc stx
                (do-make-object
                 (current-contract-region)
                 class (list arg ...) (list)))]
             [(_) (raise-syntax-error 'make-object "expected class" stx)]))))

(define-syntax (instantiate stx)
  (syntax-case stx ()
    [(form class (arg ...) . x)
     (with-syntax ([orig-stx stx])
       (quasisyntax/loc stx 
         (-instantiate do-make-object orig-stx #t (class) (list arg ...) . x)))]))

;; Helper; used by instantiate and super-instantiate
(define-syntax -instantiate
  (lambda (stx)
    (syntax-case stx ()
      [(_ do-make-object orig-stx first? (maker-arg ...) args (kw arg) ...)
       (andmap identifier? (syntax->list (syntax (kw ...))))
       (with-syntax ([(kw ...) (map localize (syntax->list (syntax (kw ...))))]
                     [(blame ...) (if (syntax-e #'first?) #'((current-contract-region)) null)])
         (syntax/loc stx
           (do-make-object blame ...
                           maker-arg ...
                           args
                           (list (cons `kw arg)
                                 ...))))]
      [(_ super-make-object orig-stx first? (make-arg ...) args kwarg ...)
       ;; some kwarg must be bad:
       (for-each (lambda (kwarg)
                   (syntax-case kwarg ()
                     [(kw arg)
                      (identifier? (syntax kw))
                      'ok]
                     [(kw arg)
                      (raise-syntax-error
                       #f
                       "by-name argument does not start with an identifier"
                       (syntax orig-stx)
                       kwarg)]
                     [_else
                      (raise-syntax-error
                       #f
                       "ill-formed by-name argument"
                       (syntax orig-stx)
                       kwarg)]))
                 (syntax->list (syntax (kwarg ...))))])))

(define (alist->sexp alist)
  (map (lambda (pair) (list (car pair) (cdr pair))) alist))

;; class blame -> class
;; takes a class and concretize interface ctc methods
(define (fetch-concrete-class cls blame)
  (cond [(null? (class-method-ictcs cls)) cls]
        [(and (class-ictc-classes cls)
              (hash-ref (class-ictc-classes cls) blame #f))
         => values]
        [else
         ;; if there are contracted methods to concretize, do so
         (let* ([name (class-name cls)]
                [ictc-meths (class-method-ictcs cls)]
                [method-width (class-method-width cls)]
                [method-ht (class-method-ht cls)]
                [meths (if (null? ictc-meths)
                           (class-methods cls)
                           (make-vector method-width))]
                [field-pub-width (class-field-pub-width cls)]
                [field-ht (class-field-ht cls)]
                [class-make (if name
                                (make-naming-constructor struct:class name "class")
                                make-class)]
                [c (class-make name
                               (class-pos cls)
                               (list->vector (vector->list (class-supers cls)))
                               (class-self-interface cls)
                               void ;; No inspecting

                               method-width
                               method-ht
                               (class-method-ids cls)
                               null
                               null

                               #f

                               meths
                               (class-super-methods cls)
                               (class-int-methods cls)
                               (class-beta-methods cls)
                               (class-meth-flags cls)

                               (class-inner-projs cls)
                               (class-dynamic-idxs cls)
                               (class-dynamic-projs cls)

                               (class-field-width cls)
                               field-pub-width
                               field-ht
                               (class-field-ids cls)

                               'struct:object 'object? 'make-object
                               'field-ref 'field-set!

                               (class-init-args cls)
                               (class-init-mode cls)
                               (class-init cls)

                               (class-orig-cls cls)
                               #f #f    ; serializer is never set
                               #f)]
                [obj-name (if name
                              (string->symbol (format "wrapper-object:~a" name))
                              'object)])

           (vector-set! (class-supers c) (class-pos c) c)

           ;; --- Make the new object struct ---
           (let-values ([(struct:object object-make object? object-field-ref object-field-set!)
                         (make-struct-type obj-name
                                           (class-struct:object cls)
                                           0 ;; No init fields
                                           0 ;; No new fields in this class replacement
                                           undefined
                                           ;; Map object property to class:
                                           (list (cons prop:object c)))])
             (set-class-struct:object! c struct:object)
             (set-class-object?! c object?)
             (set-class-make-object! c object-make)
             (set-class-field-ref! c object-field-ref)
             (set-class-field-set!! c object-field-set!))

           ;; Don't concretize if all concrete
           (unless (null? ictc-meths)
             ;; First, fill up since we're empty
             (vector-copy! meths 0 (class-methods cls))
             ;; Then apply the projections to get the concrete methods
             (for ([m (in-list ictc-meths)])
               (define index (hash-ref method-ht m))
               (define entry (vector-ref meths index))
               (define meth (car entry))
               (define ictc-infos (replace-ictc-blame (cadr entry) #f blame))
               (define wrapped-meth (concretize-ictc-method m meth ictc-infos))
               (vector-set! meths index wrapped-meth)))

           ;; initialize if not yet initialized
           (unless (class-ictc-classes cls)
             (set-class-ictc-classes! cls (make-weak-hasheq)))

           ;; cache the concrete class
           (hash-set! (class-ictc-classes cls) blame c)
           c)]))

;; name method info -> method
;; appropriately wraps the method with interface contracts
(define (concretize-ictc-method m meth info)
  (for/fold ([meth meth])
            ([info (in-list info)])
    (define ctc (car info))
    (define pos-blame (caddr info))
    (define neg-blame (cadddr info))
    (contract ctc meth pos-blame neg-blame m #f)))

(define (do-make-object blame class by-pos-args named-args)
  (unless (class? class)
    (raise-argument-error 'instantiate "class?" class))
  ;; make sure the class isn't abstract
  (unless (null? (class-abstract-ids class))
    (obj-error 'instantiate
               "cannot instantiate class with abstract methods"
               "class" class
               "abstract methods" (as-write-list (class-abstract-ids class))))
  ;; Generate correct class by concretizing methods w/interface ctcs
  (let* ([class (fetch-concrete-class class blame)]
         [o ((class-make-object class))])
    (continue-make-object o class by-pos-args named-args #t)
    o))

(define (get-field-alist obj)
  (map (lambda (id) (cons id (get-field/proc id obj)))
       (field-names obj)))

(define (continue-make-object o c by-pos-args named-args explict-named-args?)
  (let ([by-pos-only? (not (class-init-args c))])
    ;; When a superclass has #f for init-args (meaning "by-pos args with no names"),
    ;; some propagated named args may have #f keys; move them to by-position args.
    (let-values ([(by-pos-args named-args)
                  (if by-pos-only?
                      (let ([l (filter (lambda (x) (not (car x))) named-args)])
                        (if (pair? l)
                            (values (append by-pos-args (map cdr l))
                                    (filter car named-args))
                            (values by-pos-args named-args)))
                      (values by-pos-args named-args))])
      ;; Primitive class with by-pos arguments?
      (when by-pos-only?
        (unless (null? named-args)
          (if explict-named-args?
              (obj-error 
               'instantiate
               "class has only by-position initializers, but given by-name arguments" 
               "arguments" (as-lines (make-named-arg-string named-args))
               #:class-name (class-name c))
              ;; If args were implicit from subclass, should report as unused:
              (unused-args-error o named-args))))
      ;; Merge by-pos into named args:
      (let* ([named-args (if (not by-pos-only?)
                             ;; Normal merge
                             (do-merge by-pos-args (class-init-args c) c named-args by-pos-args c)
                             ;; Non-merge for by-position initializers
                             by-pos-args)]
             [leftovers (if (not by-pos-only?)
                            (get-leftovers named-args (class-init-args c))
                            null)])
        ;; In 'list mode, make sure no by-name arguments are left over
        (when (eq? 'list (class-init-mode c))
          (unless (or (null? leftovers)
                      (not (ormap car leftovers)))
            (unused-args-error o (filter car leftovers))))
        (unless (and (eq? c object%)
                     (null? named-args))
          (let ([inited? (box (class-no-super-init? c))])
            ;; ----- Execute the class body -----
            ((class-init c)
             o 
             continue-make-super
             c inited? leftovers ; merely passed through to continue-make-super
             named-args)
            (unless (unbox inited?)
              (obj-error 'instantiate 
                         "superclass initialization not invoked by initialization"
                         #:class-name (class-name c)))))))))

(define (continue-make-super o c inited? leftovers by-pos-args new-named-args)
  (when (unbox inited?)
    (obj-error 'instantiate 
               "superclass already initialized by class initialization"
               #:class-name (class-name c)))
  (set-box! inited? #t)
  (let ([named-args (if (eq? 'list (class-init-mode c))
                        ;; all old args must have been used up
                        new-named-args
                        ;; Normal mode: merge leftover keyword-based args with new ones
                        (append
                         new-named-args
                         leftovers))])
    (continue-make-object o
                          (vector-ref (class-supers c) (sub1 (class-pos c))) 
                          by-pos-args 
                          named-args
                          (pair? new-named-args))))

(define (do-merge al nl ic named-args by-pos-args c)
  (cond
    [(null? al) named-args]
    [(null? nl)
     ;; continue mapping with superclass init args, if allowed
     (let ([super (and (eq? 'normal (class-init-mode ic))
                       (positive? (class-pos ic))
                       (vector-ref (class-supers ic) (sub1 (class-pos ic))))])
       (cond
         [super 
          (if (class-init-args super)
              (do-merge al (class-init-args super) super named-args by-pos-args c)
              ;; Like 'list mode:
              (append (map (lambda (x) (cons #f x)) al)
                      named-args))]
         [(eq? 'list (class-init-mode ic))
          ;; All unconsumed named-args must have #f
          ;;  "name"s, otherwise an error is raised in
          ;;  the leftovers checking.
          (if (null? al)
              named-args
              (append (map (lambda (x) (cons #f x)) al)
                      named-args))]
         [else
          (obj-error 'instantiate 
                     "too many initialization arguments" 
                     "arguments" (as-lines (make-pos-arg-string by-pos-args))
                     #:class-name (class-name c))]))]
    [else (cons (cons (car nl) (car al))
                (do-merge (cdr al) (cdr nl) ic named-args by-pos-args c))]))

(define (get-leftovers l names)
  (cond
    [(null? l) null]
    [(memq (caar l) names)
     (get-leftovers (cdr l) (remq (caar l) names))]
    [else (cons (car l) (get-leftovers (cdr l) names))]))

(define (extract-arg class-name name arguments default)
  (if (symbol? name)
      ;; Normal mode
      (let ([a (assq name arguments)])
        (cond
          [a (cdr a)]
          [default (default)]
          [else (missing-argument-error class-name name)]))
      ;; By-position mode
      (cond
        [(< name (length arguments))
         (cdr (list-ref arguments name))]
        [default (default)]
        [else (obj-error 'instantiate "too few initialization arguments")])))

(define (extract-rest-args skip arguments)
  (if (< skip (length arguments))
      (map cdr (list-tail arguments skip))
      null))

(define (make-pos-arg-string args)
  (let ([len (length args)])
    (apply string-append
           (map (lambda (a)
                  (format " ~e" a))
                args))))

(define (make-named-arg-string args)
  (apply
   string-append
   (let loop ([args args][count 0])
     (cond
      [(null? args) null]
      [(= count 3) '("\n   ...")]
      [else (let ([rest (loop (cdr args) (add1 count))])
              (cons (format "\n   [~a ~e]"
                            (caar args)
                            (cdar args))
                    rest))]))))

(define (unused-args-error this args)
  (let ([arg-string (make-named-arg-string args)])
    (obj-error 'instantiate "unused initialization arguments" 
               "unused arguments" (as-lines arg-string)
               #:class-name (class-name (object-ref this))
               #:which-class "instantiated ")))

(define (missing-argument-error class-name name)
  (obj-error 'instantiate 
             "no argument for required init variable"
             "init variable name" (as-write name)
             #:class-name class-name
             #:which-class "instantiated "))

;;--------------------------------------------------------------------
;;  methods and fields
;;--------------------------------------------------------------------

(define-syntaxes (send send/apply send/keyword-apply)
  (let ()
    
    (define (do-method stx form obj name args rest-arg? kw-args)
      (with-syntax ([(sym method receiver)
                     (generate-temporaries (syntax (1 2 3)))])
        (quasisyntax/loc stx
          (let*-values ([(sym) (quasiquote (unsyntax (localize name)))]
                        [(receiver) (unsyntax obj)]
                        [(method) (find-method/who '(unsyntax form) receiver sym)])
            (unsyntax
             (make-method-call
              stx
              (syntax/loc stx receiver)
              (syntax/loc stx method)
              (syntax/loc stx sym)
              args
              rest-arg?
              kw-args))))))
    
    (define (core-send apply? kws?)
      (lambda (stx)
        (syntax-case stx ()
          [(form obj name . args)
           (identifier? (syntax name))
           (if (stx-list? (syntax args))
               ;; (send obj name arg ...) or (send/apply obj name arg ...)
               (do-method stx #'form #'obj #'name 
                          (if kws? (cddr (syntax->list #'args)) #'args)
                          apply? 
                          (and kws? 
                               (let ([l (syntax->list #'args)])
                                 (list (car l) (cadr l)))))
               (if apply?
                   ;; (send/apply obj name arg ... . rest)
                   (raise-syntax-error
                    #f "bad syntax (illegal use of `.')" stx)
                   ;; (send obj name arg ... . rest)
                   (do-method stx #'form #'obj #'name
                              (flatten-args #'args) #t #f)))]
          [(form obj name . args)
           (raise-syntax-error
            #f "method name is not an identifier" stx #'name)]
          [(form obj)
           (raise-syntax-error
            #f "expected a method name" stx)])))

    (define (send/keyword-apply stx)
      (syntax-case stx ()
        [(form obj name)
         (identifier? (syntax name))
         (raise-syntax-error #f "missing expression for list of keywords" stx)]
        [(form obj name a)
         (identifier? (syntax name))
         (raise-syntax-error #f "missing expression for list of keyword arguments" stx)]
        [else ((core-send #t #t) stx)]))
    
    (values
     ;; send
     (core-send #f #f)
     ;; send/apply
     (core-send #t #f)
     ;; send/keyword-apply
     send/keyword-apply)))

(define dynamic-send
  (make-keyword-procedure
   (lambda (kws kw-vals obj method-name . args)
     (unless (object? obj) (raise-argument-error 'dynamic-send "object?" obj))
     (unless (symbol? method-name) (raise-argument-error 'dynamic-send "symbol?" method-name))
     (keyword-apply (find-method/who 'dynamic-send obj method-name) kws kw-vals obj args))))

;; imperative chained send
(define-syntax (send* stx)
  (syntax-case stx ()
    [(form obj clause ...)
     (quasisyntax/loc stx
       (let* ([o obj])
         (unsyntax-splicing
          (map
           (lambda (clause-stx)
             (syntax-case clause-stx ()
               [(meth . args)
                (quasisyntax/loc stx
                  (send o meth . args))]
               [_ (raise-syntax-error
                   #f "bad method call" stx clause-stx)]))
           (syntax->list (syntax (clause ...)))))))]))

;; functional chained send
(define-syntax (send+ stx)
  (define-syntax-class send-clause
    #:description "method clause"
    (pattern [name:id . args]))
  (syntax-parse stx
    [(_ obj:expr clause-0:send-clause clause:send-clause ...)
     (quasisyntax/loc stx
       (let ([o (send obj clause-0.name . clause-0.args)])
         (send+ o clause ...)))]
    [(_ obj:expr) (syntax/loc stx obj)]))

;; find-method/who : symbol[top-level-form/proc-name]
;;                   any[object] 
;;                   symbol[method-name] 
;;               -> method-proc
;; returns the method's procedure
(define (find-method/who who in-object name)
  (let ([cls (object-ref in-object #f)])
    (if cls
        (let ([pos (hash-ref (class-method-ht cls) name #f)])
          (if pos
              (vector-ref (class-methods cls) pos)
              (obj-error who 
                         "no such method"
                         "method name" (as-write name)
                         #:class-name (class-name cls))))
        (obj-error who "target is not an object"
                   "target" in-object 
                   "method name" (as-write name)))))

(define-values (make-class-field-accessor make-class-field-mutator)
  (let ([check-and-get-index
         (λ (who class name)
           (unless (class? class)
             (raise-argument-error who "class?" class))
           (unless (symbol? name)
             (raise-argument-error who "symbol?" name))
           (hash-ref (class-field-ht class) name
                     (lambda ()
                       (obj-error who "no such field"
                                  "field-name" (as-write name)
                                  #:class-name (class-name class)))))])
    (values (λ (class name)
              (let* ([fi (check-and-get-index 'class-field-accessor class name)]
                     [ref (field-info-external-ref fi)])
                (λ (o) (if (object? o)
                           (ref o)
                           (raise-argument-error 'class-field-accessor "object?" o)))))
            (λ (class name)
              (let* ([fi (check-and-get-index 'class-field-mutator class name)]
                     [setter! (field-info-external-set! fi)])
                (λ (o v) (if (object? o)
                             (setter! o v)
                             (raise-argument-error 'class-field-mutator "object?" o))))))))

(define-struct generic (name applicable))

;; Internally, make-generic comes from the struct def.
;; Externally, make-generic is the following procedure.
;; The extra `let' gives it the right name.
(define make-generic/proc
  (let ([make-generic
         (lambda (class name)
           (unless (or (class? class) (interface? class))
             (raise-argument-error 'make-generic "(or/c class? interface?)" class))
           (unless (symbol? name)
             (raise-argument-error 'make-generic "symbol?" name))
           (make-generic
            name
            (if (interface? class)
                (let ([intf class])
                  (unless (method-in-interface? name intf)
                    (obj-error 'make-generic "no such method"
                               "method name" (as-write name)
                               #:intf-name (interface-name intf)))
                  (lambda (obj)
                    (unless (is-a? obj intf)
                      (obj-error 
                       (string->symbol (format "generic:~a" name))
                       "target is not an instance of the generic's interface"
                       "target" obj
                       #:intf-name (interface-name intf)))
                    (find-method/who 'make-generic obj name)))
                (let* ([pos (hash-ref (class-method-ht class) name
                                      (lambda ()
                                        (obj-error 'make-generic "no such method"
                                                   "method name" (as-write name)
                                                   #:class-name (class-name class))))]
                       [instance? (class-object? (class-orig-cls class))]
                       [dynamic-generic
                        (lambda (obj)
                          (unless (instance? obj)
                            (obj-error 
                             (string->symbol (format "generic:~a" name))
                             "target is not an instance of the generic's class"
                             "target" obj
                             #:class-name (class-name class)))
                          (vector-ref (class-methods (object-ref obj)) pos))])
                  (if (eq? 'final (vector-ref (class-meth-flags class) pos))
                      (let ([method (vector-ref (class-methods class) pos)])
                        (lambda (obj)
                          (unless (instance? obj)
                            (dynamic-generic obj))
                          method))
                      dynamic-generic)))))])
    make-generic))

(define-syntax (send-generic stx)
  (syntax-case stx ()
    [(_ object generic . args)
     (let* ([args-stx (syntax args)]
            [proper? (stx-list? args-stx)]
            [flat-stx (if proper? args-stx (flatten-args args-stx))])
       (with-syntax ([(gen obj)
                      (generate-temporaries (syntax (generic object)))])
         (quasisyntax/loc stx
           (let* ([obj object]
                  [gen generic])
             (unsyntax
              (make-method-call
               stx
               (syntax obj)
               (syntax/loc stx ((generic-applicable gen) obj))
               (syntax/loc stx (generic-name gen))
               flat-stx
               (not proper?)
               #f))))))]))

(define-syntaxes (class-field-accessor class-field-mutator generic/form)
  (let ([mk
         (lambda (make targets)
           (lambda (stx)
             (syntax-case stx ()
               [(_ class-expr name)
                (let ([name (syntax name)])
                  (unless (identifier? name)
                    (raise-syntax-error
                     #f
                     "expected an indentifier"
                     stx
                     name))
                  (with-syntax ([name (localize name)]
                                [make make])
                    (syntax/loc stx (make class-expr `name))))]
               [(_ class-expr)
                (raise-syntax-error
                 #f
                 (format "expected a field name after the ~a expression"
                         targets)
                 stx)])))])
    (values
     (mk (quote-syntax make-class-field-accessor) "class")
     (mk (quote-syntax make-class-field-mutator) "class")
     (mk (quote-syntax make-generic/proc) "class or interface"))))

(define-syntax (set-field! stx)
  (syntax-case stx ()
    [(_ name obj val)
     (identifier? #'name)
     (with-syntax ([localized (localize #'name)])
       (syntax/loc stx (set-field!/proc `localized obj val)))]
    [(_ name obj val)
     (raise-syntax-error
      'set-field! "expected a field name as first argument"
      stx #'name)]))

(define (set-field!/proc id obj val)
  (do-set-field! 'set-field! id obj val))

(define (do-set-field! who id obj val)
  (unless (object? obj)
    (raise-argument-error 'set-field!
                          "object?"
                          obj))
  (let* ([cls (object-ref obj)]
          [field-ht (class-field-ht cls)]
          [fi (hash-ref field-ht id #f)])
     (if fi
         ((field-info-external-set! fi) obj val)
         (obj-error 'get-field
                    "given object does not have the requested field"
                    "field name" (as-write id)
                    "object" obj))))

(define (dynamic-set-field! id obj val)
  (unless (symbol? id) (raise-argument-error 'dynamic-get-field "symbol?" id))
  (do-set-field! 'dynamic-set-field! id obj val))

(define-syntax (get-field stx)
  (syntax-case stx ()
    [(_ name obj)
     (identifier? (syntax name))
     (with-syntax ([localized (localize (syntax name))])
       (syntax (get-field/proc `localized obj)))]
    [(_ name obj)
     (raise-syntax-error
      'get-field "expected a field name as first argument"
      stx (syntax name))]))

(define (get-field/proc id obj)
  (do-get-field 'get-field id obj))

(define (do-get-field who id obj)
  (unless (object? obj)
    (raise-argument-error who
                          "object?"
                          obj))
  (let* ([cls (object-ref obj)]
          [field-ht (class-field-ht cls)]
          [fi (hash-ref field-ht id #f)])
     (if fi
         ((field-info-external-ref fi) obj)
         (obj-error who
                    "given object does not have the requested field"
                    "field name" (as-write id)
                    "object" obj))))

(define (dynamic-get-field id obj)
  (unless (symbol? id) (raise-argument-error 'dynamic-get-field "symbol?" id))
  (do-get-field 'dynamic-get-field id obj))

(define-syntax (field-bound? stx)
  (syntax-case stx ()
    [(_ name obj)
     (identifier? (syntax name))
     (with-syntax ([localized (localize (syntax name))])
       (syntax (field-bound?/proc `localized obj)))]
    [(_ name obj)
     (raise-syntax-error
      'field-bound? "expected a field name as first argument"
      stx (syntax name))]))

(define (field-bound?/proc id obj)
  (unless (object? obj)
    (raise-argument-error 'field-bound?
                          "object?"
                          obj))
  (let loop ([obj obj])
     (let* ([cls (object-ref obj)]
            [field-ht (class-field-ht cls)])
       (and (hash-ref field-ht id #f)
            #t)))) ;; ensure that only #t and #f leak out, not bindings in ht

(define (field-names obj)
  (unless (object? obj)
    (raise-argument-error 'field-names
                          "object?"
                          obj))
  (let loop ([obj obj])
     (let* ([cls (object-ref obj)]
            [field-ht (class-field-ht cls)]
            [flds (filter interned? (hash-map field-ht (lambda (x y) x)))])
       flds)))

(define-syntax (with-method stx)
  (syntax-case stx ()
    [(_ ([id (obj-expr name)] ...) body0 body1 ...)
     (let ([ids (syntax->list (syntax (id ...)))]
           [names (syntax->list (syntax (name ...)))])
       (for-each (lambda (id name)
                   (unless (identifier? id)
                     (raise-syntax-error #f
                                         "not an identifier for binding"
                                         stx
                                         id))
                   (unless (identifier? name)
                     (raise-syntax-error #f
                                         "not an identifier for method name"
                                         stx
                                         name)))
                 ids names)
       (with-syntax ([(method ...) (generate-temporaries ids)]
                     [(method-obj ...) (generate-temporaries ids)]
                     [(name ...) (map localize names)])
         (syntax/loc stx (let-values ([(method method-obj)
                                       (let ([obj obj-expr])
                                         (values (find-method/who 'with-method obj `name)
                                                 obj))]
                                      ...)
                           (letrec-syntaxes+values ([(id) (make-with-method-map
                                                           (quote-syntax set!)
                                                           (quote-syntax id)
                                                           (quote-syntax method)
                                                           (quote-syntax method-obj))]
                                                    ...)
                             ()
                             body0 body1 ...)))))]
    ;; Error cases:
    [(_ (clause ...) . body)
     (begin
       (for-each (lambda (clause)
                   (syntax-case clause ()
                     [(id (obj-expr name))
                      (and (identifier? (syntax id))
                           (identifier? (syntax name)))
                      'ok]
                     [_else
                      (raise-syntax-error 
                       #f
                       "binding clause is not of the form (identifier (object-expr method-identifier))"
                       stx
                       clause)]))
                 (syntax->list (syntax (clause ...))))
       ;; If we get here, the body must be bad
       (if (stx-null? (syntax body))
           (raise-syntax-error 
            #f
            "empty body"
            stx)
           (raise-syntax-error 
            #f
            "bad syntax (illegal use of `.')"
            stx)))]
    [(_ x . rest)
     (raise-syntax-error 
      #f
      "not a binding sequence"
      stx
      (syntax x))]))


;;--------------------------------------------------------------------
;;  class, interface, and object properties
;;--------------------------------------------------------------------

(define (is-a? v c)
  (cond
    [(class? c) (and (object? v) ((class-object? (class-orig-cls c)) v))]
    [(interface? c) (and (object? v) (implementation? (object-ref v) c))]
    [else (raise-argument-error 'is-a? "(or/c class? interface?)" 1 v c)]))

(define (subclass? v c)
  (unless (class? c)
    (raise-argument-error 'subclass? "class?" 1 v c))
  (and (class? v)
       (let* ([c (class-orig-cls c)]
              [v (class-orig-cls v)]
              [p (class-pos c)])
         (and (<= p (class-pos v))
              (eq? c (vector-ref (class-supers v) p))))))

(define (object-interface o)
  (unless (object? o)
    (raise-argument-error 'object-interface "object?" o))
  (class-self-interface (object-ref o)))

(define (object-method-arity-includes? o name cnt)
  (unless (object? o)
    (raise-argument-error 'object-method-arity-includes? "object?" o))
  (unless (symbol? name)
    (raise-argument-error 'object-method-arity-includes? "symbol?" name))
  (unless (and (integer? cnt)
               (exact? cnt)
               (not (negative? cnt)))
    (raise-argument-error 'object-method-arity-includes? "exact-nonnegative-integer?" cnt))
  (let loop ([o o])
     (let* ([c (object-ref o)]
            [pos (hash-ref (class-method-ht c) name #f)])
       (cond
         [pos (procedure-arity-includes? (vector-ref (class-methods c) pos) 
                                         (add1 cnt))]
         [else #f]))))

(define (implementation? v i)
  (unless (interface? i)
    (raise-argument-error 'implementation? "interface?" 1 v i))
  (and (class? v)
       (interface-extension? (class-self-interface v) i)))

(define (interface-extension? v i)
  (unless (interface? i)
    (raise-argument-error 'interface-extension? "interface?" 1 v i))
  (and (interface? i)
       (hash-ref (interface-all-implemented v) i #f)))

(define (method-in-interface? s i)
  (unless (symbol? s)
    (raise-argument-error 'method-in-interface? "symbol?" 0 s i))
  (unless (interface? i)
    (raise-argument-error 'method-in-interface? "interface?" 1 s i))
  (and (memq s (interface-public-ids i)) #t))

(define (class->interface c)
  (unless (class? c)
    (raise-argument-error 'class->interface "class?" c))
  (class-self-interface c))

(define (interned? sym)
  (eq? sym (string->symbol (symbol->string sym))))

(define (interface->method-names i)
  (unless (interface? i)
    (raise-argument-error 'interface->method-names "interface?" i))
  (filter interned? (interface-public-ids i)))


(define (object-info o)
  (unless (object? o)
    (raise-argument-error 'object-info "object?" o))
  (let ([o* (if (has-original-object? o) (original-object o) o)])
    (let loop ([c (object-ref o*)]
               [skipped? #f])
      (if (struct? ((class-insp-mk c)))
          ;; current objec can inspect this object
          (values c skipped?)
          (if (zero? (class-pos c))
              (values #f #t)
              (loop (vector-ref (class-supers c) (sub1 (class-pos c))) #t))))))

(define (to-sym s)
  (if (string? s)
      (string->symbol s)
      s))

(define (class-info c)
  (unless (class? c)
    (raise-argument-error 'class-info "class?" c))
  (if (struct? ((class-insp-mk c)))
      (let ([super (vector-ref (class-supers c) (sub1 (class-pos c)))])
        (let loop ([next super][skipped? #f])
          (if (or (not next)
                  (struct? ((class-insp-mk next))))
              (values (to-sym (class-name c))
                      (- (class-field-width c) (class-field-width super))
                      (filter interned? (class-field-ids c))
                      (class-field-ref c)
                      (class-field-set! c)
                      next
                      skipped?)
              (if (zero? (class-pos next))
                  (loop #f #t)
                  (loop (vector-ref (class-supers next) (sub1 (class-pos next))) #t)))))
      (raise-arguments-error 'class-info "current inspector cannot inspect class" 
                             "class" c)))

(define object->vector
  (lambda (in-o [opaque-v '...])
    (unless (object? in-o)
      (raise-argument-error 'object->vector "object?" in-o))
    (let ([o in-o])
      (list->vector
       (cons
        (string->symbol (format "object:~a" (class-name (object-ref o))))
        (reverse
         (let-values ([(c skipped?) (object-info o)])
           (let loop ([c c][skipped? skipped?])
             (cond
               [(not c) (if skipped? (list opaque-v) null)]
               [else (let-values ([(name num-fields field-ids field-ref
                                         field-set next next-skipped?)
                                   (class-info c)])
                       (let ([rest (loop next next-skipped?)]
                             [here (let loop ([n num-fields])
                                     (if (zero? n)
                                         null
                                         (cons (field-ref o (sub1 n))
                                               (loop (sub1 n)))))])
                         (append (if skipped? (list opaque-v) null)
                                 here
                                 rest)))])))))))))

(define (object=? o1 o2)
  (unless (object? o1)
    (raise-argument-error 'object=? "object?" o1))
  (unless (object? o2)
    (raise-argument-error 'object=? "object?" o2))
  (let ([orig-o1 (if (has-original-object? o1) (original-object o1) o1)]
        [orig-o2 (if (has-original-object? o2) (original-object o2) o2)])
    (eq? orig-o1 orig-o2)))

;;--------------------------------------------------------------------
;;  primitive classes
;;--------------------------------------------------------------------

(define (make-primitive-class 
         make-struct:prim     ; see below
         prim-init            ; primitive initializer: takes obj and list of name-arg pairs
         name                 ; symbol
         super                ; superclass
         intfs                ; interfaces
         init-arg-names       ; #f or list of syms and sym--value lists
         override-names       ; overridden method names
         new-names            ; new (public) method names
         override-methods     ; list of methods
         new-methods)         ; list of methods
  
  ; The `make-struct:prim' function takes prop:object, a class,
  ;  a preparer, a dispatcher function, an unwrap property,
  ;  an unwrapper, and a property assoc list, and produces:
  ;    * a struct constructor (must have prop:object)
  ;    * a struct predicate
  ;    * a struct type for derived classes (mustn't have prop:object)
  ;
  ; The supplied preparer takes a symbol and returns a num.
  ; 
  ; The supplied dispatcher takes an object and a num and returns a method.
  ;
  ; The supplied unwrap property is used for adding the unwrapper
  ;  as a property value on new objects.
  ;
  ; The supplied unwrapper takes an object and returns the unwrapped
  ;  version (or the original object).
  ;
  ; When a primitive class has a superclass, the struct:prim maker
  ;  is responsible for ensuring that the returned struct items match
  ;  the supertype predicate.
  
  (compose-class name
                 (or super object%)
                 intfs
                 #f
                 #f
                 #f
                 
                 0 null null ; no fields
                 
                 null ; no rename-supers
                 null ; no rename-inners
                 null null new-names
                 null null override-names
                 null null null ; no augrides
                 null ; no inherits
                 
                 ; #f => init args by position only
                 ; sym => required arg
                 ; sym--value list => optional arg
                 (and init-arg-names  
                      (map (lambda (s)
                             (if (symbol? s) s (car s)))
                           init-arg-names))
                 'stop
                 
                 (lambda ignored
                   (values
                    new-methods
                    override-methods
                    null ; no augride-methods
                    (lambda (this super-go/ignored si_c/ignored si_inited?/ignored si_leftovers/ignored init-args)
                      (apply prim-init this 
                             (if init-arg-names
                                 (extract-primitive-args this name init-arg-names init-args)
                                 init-args)))))
                 
                 make-struct:prim))

(define (extract-primitive-args this class-name init-arg-names init-args)
  (let loop ([names init-arg-names][args init-args])
    (cond
      [(null? names)
       (unless (null? args)
         (unused-args-error this args))
       null]
      [else (let* ([name (car names)]
                   [id (if (symbol? name)
                           name
                           (car name))])
              (let ([arg (assq id args)])
                (cond
                  [arg 
                   (cons (cdr arg) (loop (cdr names) (remq arg args)))]
                  [(symbol? name)
                   (missing-argument-error class-name name)]
                  [else
                   (cons (cadr name) (loop (cdr names) args))])))])))

;;--------------------------------------------------------------------
;;  wrapper for contracts
;;--------------------------------------------------------------------

(define (make-wrapper-class cls blame methods method-contracts fields field-contracts)
  (let* ([name (class-name cls)]
         [method-width (class-method-width cls)]
         [method-ht (class-method-ht cls)]
         [meths (if (null? methods)
                    (class-methods cls)
                    (make-vector method-width))]
         [field-pub-width (class-field-pub-width cls)]
         [field-ht (if (null? fields)
                       (class-field-ht cls)
                       (hash-copy (class-field-ht cls)))]
         [class-make (if name
                         (make-naming-constructor struct:class name "class")
                         make-class)]
         [c (class-make name
                        (class-pos cls)
                        (list->vector (vector->list (class-supers cls)))
                        (class-self-interface cls)
                        void ;; No inspecting
                        
                        method-width
                        method-ht
                        (class-method-ids cls)
                        (class-abstract-ids cls)
                        (class-method-ictcs cls)

                        (class-ictc-classes cls)
                        
                        meths
                        (class-super-methods cls)
                        (class-int-methods cls)
                        (class-beta-methods cls)
                        (class-meth-flags cls)
                        
                        (class-inner-projs cls)
                        (class-dynamic-idxs cls)
                        (class-dynamic-projs cls)
                        
                        (class-field-width cls)
                        field-pub-width
                        field-ht
                        (class-field-ids cls)
                        
                        'struct:object 'object? 'make-object
                        'field-ref 'field-set!
                        
                        (class-init-args cls)
                        (class-init-mode cls)
                        (class-init cls)
                        
                        (class-orig-cls cls)
                        #f #f ; serializer is never set
                        #f)]
         [obj-name (if name
                       (string->symbol (format "wrapper-object:~a" name))
                       'object)])
    (define (make-method proc meth-name)
      (procedure-rename
       (procedure->method proc)
       (string->symbol
        (format "~a method~a~a"
                meth-name
                (if name " in " "")
                (or name "")))))

    (vector-set! (class-supers c) (class-pos c) c)
    
    ;; --- Make the new object struct ---
    (let-values ([(struct:object object-make object? object-field-ref object-field-set!)
                  (make-struct-type obj-name
                                    (class-struct:object cls)
                                    0 ;; No init fields
                                    0 ;; No new fields in this class replacement
                                    undefined
                                    ;; Map object property to class:
                                    (list (cons prop:object c)))])
      (set-class-struct:object! c struct:object)
      (set-class-object?! c object?)
      (set-class-make-object! c object-make)
      (set-class-field-ref! c object-field-ref)
      (set-class-field-set!! c object-field-set!))
    
    ;; Handle public method contracts
    (unless (null? methods)
      ;; First, fill in from old methods
      (vector-copy! meths 0 (class-methods cls))
      ;; Now apply projections
      (for ([m (in-list methods)]
            [c (in-list method-contracts)])
        (when c
          (let ([i (hash-ref method-ht m)]
                [p ((contract-projection c) (blame-add-context blame (format "the ~a method in" m)
                                                               #:important m))])
            (vector-set! meths i (make-method (p (vector-ref meths i)) m))))))
    
    ;; Handle external field contracts
    (unless (null? fields)
      (for ([f (in-list fields)]
            [c (in-list field-contracts)])
        (when c
          (define fld-context (format "the ~a field in" f))
          (define bset (blame-add-context blame fld-context #:swap? #t))
          (let ([fi (hash-ref field-ht f)]
                [p-pos ((contract-projection c) (blame-add-context blame fld-context))]
                [p-neg ((contract-projection c) bset)])
            (hash-set! field-ht f (field-info-extend-external fi p-pos p-neg))))))
    
    c))

(define-values (impersonator-prop:original-object has-original-object? original-object)
  (make-impersonator-property 'impersonator-prop:original-object))

;; make-wrapper-object: contract object blame (listof symbol) (listof contract?) (listof symbol) (listof contract?)
(define (make-wrapper-object ctc obj blame methods method-contracts fields field-contracts)
  (check-object-contract obj methods fields (λ args (apply raise-blame-error blame obj args)))
  (let ([original-obj (if (has-original-object? obj) (original-object obj) obj)]
        [new-cls (make-wrapper-class (object-ref obj) 
                                     blame
                                     methods method-contracts fields field-contracts)])
    (impersonate-struct obj object-ref (λ (o c) new-cls)
                        impersonator-prop:contracted ctc
                        impersonator-prop:original-object original-obj)))

;;--------------------------------------------------------------------
;;  misc utils
;;--------------------------------------------------------------------

(define undefined (letrec ([x x]) x))

(define-struct (exn:fail:object exn:fail) () #:inspector insp)

(struct as-write (content))
(struct as-write-list (content))
(struct as-value-list (content))
(struct as-lines (content))

(define (obj-error where 
                   msg
                   #:class-name [class-name #f]
                   #:intf-name [intf-name #f]
                   #:which-class [which-class ""]
                   . fields)
  (define all-fields
    (append fields
            (if class-name
                (list (string-append which-class "class name")
                      (as-write class-name))
                null)
            (if intf-name
                (list "interface name"
                      (as-write intf-name))
                null)))
  (raise (make-exn:fail:object
          (format "~a: ~a~a" where msg
                  (apply
                   string-append
                   (let loop ([fields all-fields])
                     (cond
                      [(null? fields) null]
                      [else
                       (define field (car fields))
                       (define val (cadr fields))
                       (list*
                        "\n  "
                        field
                        (if (or (as-write-list? val)
                                (as-lines? val))
                            ":"
                            ": ")
                        (cond
                         [(or (as-write-list? val)
                              (as-value-list? val))
                          (apply string-append
                                 (for/list ([v (in-list (as-write-list-content val))])
                                   (format (if (as-write-list? val)
                                               "\n   ~s"
                                               "\n   ~e")
                                           v)))]
                         [(as-write? val)
                          (format "~s" (as-write-content val))]
                         [(as-lines? val)
                          (as-lines-content val)]
                         [else
                          (format "~e" val)])
                        (loop (cddr fields)))]))))
          (current-continuation-marks))))

(define (for-class name)
  (if name (format " for class: ~a" name) ""))
(define (for-class/which which name)
  (if name (format " for ~a class: ~a" which name) ""))
(define (for-intf name)
  (if name (format " for interface: ~a" name) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mixin
;;

(define (check-mixin-super mixin-name super% from-ids)
  (let ([mixin-name (or mixin-name 'mixin)])
    (unless (class? super%)
      (obj-error mixin-name 
                 "argument is not a class"
                 "argument" super%))
    (for-each (lambda (from-id)
                (unless (implementation? super% from-id)
                  (obj-error mixin-name 
                             "argument class does not implement interface" 
                             "argument" super% 
                             "interface name" (as-write from-id))))
              from-ids)))

(define (check-mixin-from-interfaces all-from)
  (for-each (lambda (from-id)
              (unless (interface? from-id)
                (obj-error 'mixin
                           "given value for from-interface is not an interface"
                           "given" from-id
                           "all given" (as-value-list all-from))))
            all-from))

(define (check-mixin-to-interfaces all-to)
  (for-each (lambda (to-id)
              (unless (interface? to-id)
                (obj-error 'mixin
                           "given values for from-interface is not an interface"
                           "given" to-id
                           "all given" (as-value-list all-to))))
            all-to))


(define (check-interface-includes xs from-ids)
  (for-each
   (lambda (x)
     (unless (ormap (lambda (i) (method-in-interface? x i)) from-ids)
       (obj-error 'mixin
              "method was referenced in definition, but is not in any of the from-interfaces"
              "method name" (as-write x)
              "from-interfaces" (as-write-list from-ids))))
   xs))

(define-syntax (mixin stx)
  (syntax-case stx ()
    [(_ (from ...) (to ...) clauses ...)
     (let ([extract-renamed-names
            (λ (x)
              (map (λ (x) 
                     (localize
                      (syntax-case x ()
                        [(internal-name external-name) (syntax external-name)]
                        [else x])))
                   (syntax->list x)))])
       (define (get-super-names stx)
         (syntax-case stx (inherit rename 
                                   override overment override-final
                                   define/override define/overment define/override-final
                                   augment augride augment-final
                                   define/augment define/augride define/augment-final)
           [(inherit names ...) (extract-renamed-names (syntax (names ...)))]
           [(rename [x names] ...) (syntax->list (syntax (names ...)))]
           [(override names ...) (extract-renamed-names (syntax (names ...)))]
           [(overment names ...) (extract-renamed-names (syntax (names ...)))]
           [(override-final names ...) (extract-renamed-names (syntax (names ...)))]
           [(augment names ...) (extract-renamed-names (syntax (names ...)))]
           [(augride names ...) (extract-renamed-names (syntax (names ...)))]
           [(augment-final names ...) (extract-renamed-names (syntax (names ...)))]
           
           [(define/augment (name . names) . rest) (extract-renamed-names (syntax (name)))]
           [(define/augment name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
           [(define/augride (name . names) . rest) (extract-renamed-names (syntax (name)))]
           [(define/augride name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
           [(define/augment-final (name . names) . rest) (extract-renamed-names (syntax (name)))]
           [(define/augment-final name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
           [(define/override (name . names) . rest) (extract-renamed-names (syntax (name)))]
           [(define/override name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
           [(define/overment (name . names) . rest) (extract-renamed-names (syntax (name)))]
           [(define/overment name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
           [(define/override-final (name . names) . rest) (extract-renamed-names (syntax (name)))]
           [(define/override-final name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
           [else null]))
       (with-syntax ([(from-ids ...) (generate-temporaries (syntax (from ...)))]
                     [(to-ids ...) (generate-temporaries (syntax (to ...)))]
                     [(super-vars ...)
                      (apply
                       append
                       (map get-super-names
                            (syntax->list (syntax (clauses ...)))))]
                     [mixin-name (or (with-syntax ([tmp (syntax-local-name)])
                                       (syntax (quote tmp)))
                                     (syntax (quote mixin)))])
         
         ;; Build the class expression first, to give it a good src location:
         (with-syntax ([class-expr
                        (with-syntax ([orig-stx stx])
                          (syntax/loc stx
                            (class/derived orig-stx [#f super% (to-ids ...) #f]
                                           clauses ...)))])
           
           ;; Now build mixin proc, again to give it a good src location:
           (with-syntax ([mixin-expr
                          (syntax/loc stx
                            (λ (super%)
                              (check-mixin-super mixin-name super% (list from-ids ...))
                              class-expr))])
             
             ;; Finally, build the complete mixin expression:
             (syntax/loc stx
               (let ([from-ids from] ...)
                 (let ([to-ids to] ...)
                   (check-mixin-from-interfaces (list from-ids ...))
                   (check-mixin-to-interfaces (list to-ids ...))
                   (check-interface-includes (list (quasiquote super-vars) ...)
                                             (list from-ids ...))
                   mixin-expr)))))))]))

(define externalizable<%>
  (_interface () externalize internalize))

(define writable<%>
  (interface* () 
              ([prop:custom-write (lambda (obj port mode)
                                    (if mode
                                        (send obj custom-write port)
                                        (send obj custom-display port)))])
              custom-write custom-display))

(define printable<%>
  (interface* () 
              ([prop:custom-write (lambda (obj port mode)
                                    (case mode
                                      [(#t) (send obj custom-write port)]
                                      [(#f) (send obj custom-display port)]
                                      [else (send obj custom-print port mode)]))])
              custom-write custom-display custom-print))

(define equal<%>
  (interface* () 
              ([prop:equal+hash (list
                                 (lambda (obj obj2 base-equal?)
                                   (send obj equal-to? obj2 base-equal?))
                                 (lambda (obj base-hash-code)
                                   (send obj equal-hash-code-of base-hash-code))
                                 (lambda (obj base-hash2-code)
                                   (send obj equal-secondary-hash-code-of base-hash2-code)))])
              equal-to? equal-hash-code-of equal-secondary-hash-code-of))

;; Providing normal functionality:
(provide (protect-out make-wrapper-object
                      check-object-contract
                      get-field/proc)
         
         (rename-out [_class class]) class* class/derived
         define-serializable-class define-serializable-class*
         class?
         mixin
         (rename-out [_interface interface]) interface* interface?
         object% object? object=? externalizable<%> printable<%> writable<%> equal<%>
         new make-object instantiate
         get-field set-field! field-bound? field-names
         dynamic-get-field dynamic-set-field!
         send send/apply send/keyword-apply send* send+ dynamic-send
         class-field-accessor class-field-mutator with-method
         private* public*  pubment*
         override* overment*
         augride* augment*
         public-final* override-final* augment-final*
         define/private define/public define/pubment 
         define/override define/overment
         define/augride define/augment
         define/public-final define/override-final define/augment-final
         define-local-member-name define-member-name 
         member-name-key generate-member-key member-name-key? member-name-key=? member-name-key-hash-code
         (rename-out [generic/form generic]) (rename-out [make-generic/proc make-generic]) send-generic generic?
         is-a? subclass? implementation? interface-extension?
         object-interface object-info object->vector
         object-method-arity-includes?
         method-in-interface? interface->method-names class->interface class-info
         (struct-out exn:fail:object)
         make-primitive-class
         class/c ->m ->*m ->dm case->m object/c instanceof/c)
