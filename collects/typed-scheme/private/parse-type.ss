#lang scheme/base

(require "parse-type2.ss")
(provide (all-from-out "parse-type2.ss"))

#|

(require (except-in "../utils/utils.ss" extend))
(require (except-in (rep type-rep) make-arr)
         (rename-in (types convenience union utils) [make-arr* make-arr])
         (utils tc-utils)
         syntax/stx (prefix-in c: scheme/contract)
         (except-in stxclass) stxclass/util
         (env type-environments type-name-env type-alias-env lexical-env)
         (prefix-in t: "base-types-extra.ss")
         scheme/match 
         (for-syntax scheme/base stxclass stxclass/util)
         (for-template scheme/base "base-types-extra.ss"))

(define (atom? v)
  (or (number? v) (string? v) (boolean? v) (symbol? v) (keyword? v) (char? v) (bytes? v) (regexp? v)))

(define-syntax-class (3d pred)
  (pattern s           
           #:with datum (syntax-e #'s)
           #:when (pred #'datum)))

(define-syntax (parse/get stx)
  (syntax-parse stx
    [(_ arg:expr attr:id pat)
     (let* ([i (generate-temporary)]
            [get-i (datum->syntax 
		    i 
		    (string->symbol 
		     (string-append (symbol->string (syntax-e i)) 
				    "."
				    (symbol->string #'attr.datum))))])
       (quasisyntax/loc stx
         (syntax-parse arg 
           [#,i #:declare #,i pat #'#,get-i])))]))



(define-pred-stxclass atom atom?)
(define-pred-stxclass byte-pregexp byte-pregexp?)
(define-pred-stxclass byte-regexp byte-regexp?)
(define-pred-stxclass regexp regexp?)
(define-pred-stxclass bytes bytes?)

(p/c [parse-type (syntax? . c:-> . Type/c)]
     [parse-type/id (syntax? c:any/c . c:-> . Type/c)] 
     [parse-tc-results (syntax? . c:-> . tc-results?)] 
     [parse-tc-results/id (syntax? c:any/c . c:-> . tc-results?)] 
     [parse-type* (syntax? . c:-> . Type/c)])

(define enable-mu-parsing (make-parameter #t))


(define ((parse/id p) loc datum)
  #;(printf "parse-type/id id : ~a~n ty: ~a~n" (syntax-object->datum loc) (syntax-object->datum stx))
  (let* ([stx* (datum->syntax loc datum loc loc)])
    (p stx*)))

(define (stx-cadr stx) (stx-car (stx-cdr stx)))

(define-syntax-class star
  #:description "*"
  (pattern star:id
           #:when (eq? '* #'star.datum)))

(define-syntax-class ddd
  #:description "..."
  (pattern ddd:id
           #:when (eq? '... #'ddd.datum)))

(define-syntax-class tvar
  #:description "type variable"
  (pattern v:id
           #:with val (lookup (current-tvars) #'v.datum (lambda (_) #f))
           #:with name #'v.datum
           #:with datum #'v.datum
           #:when #'val))

(define-syntax-class dotted-tvar
  #:description "type variable bound with ..."
  (pattern v:tvar
           #:when (Dotted? #'v.val)
           #:with t (Dotted-t #'v.val)
           #:with val #'v.val
           #:with name #'v.datum
           #:with datum #'v.datum))

(define-syntax-class dotted-both-tvar
  #:transparent
  (pattern v:dotted-tvar
           #:when (DottedBoth? #'v.val)
           #:with t (Dotted-t #'v.val)
           #:with val #'v.val
           #:with name #'v.datum
           #:with datum #'v.datum))


(define-syntax-class (type/tvars syms var-tys)
  (pattern ty
           #:with t (parameterize ([current-tvars (extend-env syms 
                                                              var-tys
                                                              (current-tvars))])
                      (parse-type* #'ty))))

(define-syntax-class (type/tvar sym var-ty)
  (pattern ty
           #:declare ty (type/tvars (list sym) (list var-ty))
           #:with t #'ty.t))


(define-syntax-class fun-ty
  #:literals (t:-> :)
  #:transparent
  #:description "function type"
  ;; FIXME - shouldn't have to use syntax->datum  
  (pattern (dom*:type t:-> rng:type : pred:type)
           #:when (add-type-name-reference #'t:->)
           #:with t (make-pred-ty (list #'dom*.t) #'rng.t #'pred.t)
           #:with (dom ...) (list #'dom*))
  (pattern (dom:type ... rest:type _:star t:-> rng:type)
           #:when (add-type-name-reference #'t:->)     
           #:with t (->* (syntax->datum #'(dom.t ...)) #'rest.t #'rng.t))
  (pattern (dom:type ... rest _:ddd bound:dotted-tvar t:-> rng:type)
           #:with rest.t (parse/get #'rest t (type/tvar #'bound.name (make-DottedBoth (make-F #'bound.name))))
           #:when (add-type-name-reference #'t:->)
           #:with t
           (let ([var #'bound.val])
             (make-Function
              (list
               (make-arr-dots (syntax->datum #'(dom.t ...))
                              #'rng.t
                              #'rest.t
                              #'bound.name)))))
  (pattern (dom:type ...  t:-> rng:type)  
           #:when (add-type-name-reference #'t:->)
           #:with t (->* (syntax->datum #'(dom.t ...)) #'rng.t)))

(define-syntax-class fun-ty/one
  (pattern f:fun-ty
           #:with arr (match #'f.t [(Function: (list a)) a])))


(define-syntax-class values-ty
  #:literals (values)
  (pattern (values ts:type ... rest _:ddd bound:dotted-tvar)
           #:with rest.t (parse/get #'rest t (type/tvar #'bound.name (make-DottedBoth (make-F #'bound.name))))
           #:with t
           (make-ValuesDots (syntax->datum #'(ts.t ...))
                            #'rest.t
                            #'bound.name))
  (pattern (values ts:type ...) 
           #:with t (-values (syntax->datum #'(ts.t ...)))))

(define-syntax-class type-name
  #:description "type name"
 (pattern i:id
          #:when (lookup-type-name #'i (lambda () #f))
          #:with t #'(make-Name #'i)
          #:when (add-type-name-reference #'i)))

(define-syntax-class type-alias
  #:description "type alias"
  (pattern i:id
           #:with t (lookup-type-alias #'i parse-type* (lambda () #f))
           #:when #'t
           #:when (add-type-name-reference #'i)))

(define-syntax-class all-ddd-formals
  #:description "\na sequence of identifiers with a ... after the last identifier\n"
  (pattern (v:id ... v-last:id _:ddd)))

(define-syntax-class all-formals
  #:description "\na sequence of identifiers\n"
  (pattern (v:id ...)))

(define-syntax-class all-type 
  #:description "All type"
  #:transparent
  #:literals (t:All)
  (pattern (t:All :all-ddd-formals b)  
           #:with b.t (parse/get #'b t (type/tvars (cons #'v-last.datum (syntax->datum #'(v ...)))
                                                   (cons (make-Dotted (make-F #'v-last.datum))
                                                         (map make-F (syntax->datum #'(v ...))))))
           #:when (add-type-name-reference #'All)
           #:with t (make-PolyDots (syntax->datum #'(v ... v-last)) #'b.t))
  (pattern (t:All :all-formals b)
           #:with b.t (parse/get #'b t (type/tvars (syntax->datum #'(v ...)) (map make-F (syntax->datum #'(v ...)))))
           #:when (add-type-name-reference #'All)
           #:with t (make-Poly (syntax->datum #'(v ...)) #'b.t)))

(define-syntax-class type-app
  #:attributes (t)
  (pattern (i arg:type args:type ...)
           #:declare i type
           #:when (identifier? #'i)
           #:with t
           (let loop 
             ([rator #'i.t] [args (syntax->datum #'(arg.t args.t ...))])
             (match rator
               [(Name: _)
                ;; FIXME - need orig stx
                (make-App rator args #'here)]
               [(Poly: ns _)
                (if (not (= (length args) (length ns)))
                    (begin
                      (tc-error/delayed "Wrong number of arguments to type ~a, expected ~a but got ~a" rator (length ns) (length args))
                      (instantiate-poly rator (map (lambda _ Err) ns)))
                    (instantiate-poly rator args))]
               [(Mu: _ _) (loop (unfold rator) args)]
               [(Error:) Err]
               [_ (tc-error/delayed "Type ~a cannot be applied, arguments were: ~a" rator args)
                  Err]))))

(define-syntax-class not-kw-id
  #:attributes (datum)
  (pattern i:id
           #:when (not (for/or ([e (syntax->list 
                                    #'(quote t:pred t:Tuple case-lambda t:U t:Rec t:Opaque t:Parameter t:Class t:Instance
                                             t:-> t:All))])
                               (free-identifier=? e #'i)))
           #:when (not (memq #'i.datum '(* ...)))
           #:with datum #'i.datum))

(define-syntax-class type
  #:transparent
  #:attributes (t)
  #:literals (quote t:pred t:Tuple case-lambda t:U t:Rec t:Opaque t:Parameter t:Class t:Instance)  
  (pattern ty
           #:declare ty (3d Type?)
           #:with t #'ty.datum)
  (pattern i:dotted-both-tvar
           #:with t #'i.t)
  #;
  (pattern i:dotted-tvar
           #:when (tc-error/stx #'i "Type variable ~a must be used with ..." #'i.datum)
           #:with t Err)
  (pattern i:tvar
           #:when (not (Dotted? #'i.val))
           #:with t #'i.val)
  (pattern i:type-alias
           #:with t #'i.t)
  (pattern i:type-name
           #:with t #'i.t)  
  #;
  (pattern i:not-kw-id
           #:with t Err
           #:when (tc-error/stx #'i "Unbound type name ~a" #'i.datum)
           )
  (pattern ty:all-type
           #:with t #'ty.t)
  (pattern (t:Rec x:id b)
           #:when (enable-mu-parsing)
           #:with b.t (parse/get #'b t (type/tvar #'x.datum (make-F #'x.datum)))
           #:when (add-type-name-reference #'t:Rec)
           #:with t (if (memq #'x.datum (fv #'b.t))
                        (make-Mu #'x.datum #'b.t)
                        #'b.t))
  (pattern (t:pred ty:type)
           #:when (add-type-name-reference #'t:pred)
           #:with t (make-pred-ty #'ty.t))
  (pattern (t:Parameter ty:type)
           #:when (add-type-name-reference #'t:Paramter)
           #:with t (-Param #'ty.t #'ty.t))
  (pattern (t:Parameter t1:type t2:type)
           #:when (add-type-name-reference #'t:Paramter)
           #:with t (-Param #'t1.t #'t2.t))
  (pattern (t:Opaque p?:id)
           #:when (add-type-name-reference #'t:Opaque)
           #:with t (make-Opaque #'p? (syntax-local-certifier)))
  (pattern (t:U ty:type ...)
           #:with t (apply Un (syntax->datum #'(ty.t ...))))
  (pattern (t:Tuple ty:type ...)
           #:with t (-Tuple (syntax->datum #'(ty.t ...))))
  (pattern fty:fun-ty
           #:with t #'fty.t)
  (pattern vt:values-ty
           #:with t #'vt.t)
  (pattern (fst:type . rst:type)
           #:with t (-pair #'fst.t #'rst.t))
  (pattern (quote v:atom)
           #:with t (-val #'v.datum))
  (pattern (case-lambda f:fun-ty/one ...)
           #:with t (make-Function (syntax->datum #'(f.arr ...))))
  
  (pattern (t:Class (pos-args:type ...) ([fname:id fty:type (~or (rest:boolean) #:opt) ...] ...) ([mname:id mty:type] ...))
           #:with t
           (make-Class
            (syntax->datum #'(pos-args.t ...))
            (syntax->datum #'([fname.datum fty.t rest.datum] ...))
            (syntax->datum #'([mname.datum mty.t] ...))))
  
  (pattern (t:Instance ty:type)
           #:with t
           (if (not (or (Mu? #'ty.t) (Class? #'ty.t) (Union? #'ty.t) (Error? #'ty.t)))
               (begin (tc-error/delayed "Argument to Instance must be a class type, got ~a" #'ty.t)
                      (make-Instance Err))
               (make-Instance #'ty.t)))
  
  (pattern tapp:type-app
           #:with t #'tapp.t)
  
  (pattern v:atom
           #:when (not (symbol? #'v.datum))
           #:with t (-val #'v.datum)))

(define (parse-type* stx)
  (parameterize ([current-orig-stx stx])
    (parse/get stx t type)))

(define (parse-all-type stx parse-type)
  (syntax-parse stx
    [(All (vars ... v dd) t)
     #:when (eq? (syntax-e #'dd) '...)
     #:when (andmap identifier? (syntax->list #'(v vars ...)))
     (let* ([vars (map syntax-e (syntax->list #'(vars ...)))]
            [tvars (map make-F vars)]
            [v (syntax-e #'v)]
            [tv (make-Dotted (make-F v))])
       (add-type-name-reference #'All)
       (parameterize ([current-tvars (extend-env (cons v vars) (cons tv tvars) (current-tvars))])
         (make-PolyDots (append vars (list v)) (parse-type #'t))))]
    [(All (vars ...) t) 
     #:when (andmap identifier? (syntax->list #'(vars ...)))
     (let* ([vars (map syntax-e (syntax->list #'(vars ...)))]
            [tvars (map make-F vars)])
       (add-type-name-reference #'All)
       (parameterize ([current-tvars (extend-env vars tvars (current-tvars))])
         (make-Poly vars (parse-type #'t))))]
    [(All . rest) (tc-error "All: bad syntax")]))

(define (parse-type stx)    
  (parameterize ([current-orig-stx stx])    
    (syntax-case* stx ()
      symbolic-identifier=?  
      [t
       (Type? (syntax-e #'t))
       (syntax-e #'t)]
      [(fst . rst)
       (not (syntax->list #'rst))
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(Class (pos-args ...) ([fname fty . rest] ...) ([mname mty] ...))
       (eq? (syntax-e #'Class) 'Class)
       (make-Class
        (map parse-type (syntax->list #'(pos-args ...)))
        (map list
             (map syntax-e (syntax->list #'(fname ...)))
             (map parse-type (syntax->list #'(fty ...)))
             (map (lambda (e) (syntax-case e ()
                                [(#t) #t]
                                [_ #f]))
                  (syntax->list #'(rest ...))))
        (map list
             (map syntax-e (syntax->list #'(mname ...)))
             (map parse-type (syntax->list #'(mty ...)))))]
      [(Refinement p?)
       (and (eq? (syntax-e #'Refinement) 'Refinement)
            (identifier? #'p?))
       (match (lookup-type/lexical #'p?)
              [(and t (Function: (list (arr: (list dom) _ #f #f '()))))
               (make-Refinement dom #'p? (syntax-local-certifier))]
              [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]
      [(Instance t)
       (eq? (syntax-e #'Instance) 'Instance)
       (let ([v (parse-type #'t)])
         (if (not (or (Mu? v) (Class? v) (Union? v) (Error? v)))
             (begin (tc-error/delayed "Argument to Instance must be a class type, got ~a" v)
                    (make-Instance (Un)))
             (make-Instance v)))]
      [(Tuple ts ...)
       (or (eq? (syntax-e #'Tuple) 'Tuple)
           (eq? (syntax-e #'Tuple) 'List))
       (begin
         (add-type-name-reference (stx-car stx))
         (-Tuple (map parse-type (syntax->list #'(ts ...)))))]
      [(cons fst rst)
       (eq? (syntax-e #'cons) 'cons)
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(pred t) 
       (eq? (syntax-e #'pred) 'pred)
       (make-pred-ty (parse-type #'t))]
      ;; function types
      [(dom -> rng : pred-ty)
       (and 
        (eq? (syntax-e #'->) '->)
        (eq? (syntax-e #':) ':))
       (begin
         (add-type-name-reference (stx-cadr stx))
         ;; use parse-type instead of parse-values-type because we need to add the filters from the pred-ty
         (make-pred-ty (list (parse-type #'dom)) (parse-type #'rng) (parse-type #'pred-ty)))]
      [(dom ... rest ::: -> rng)
       (and (eq? (syntax-e #'->) '->) 
            (eq? (syntax-e #':::) '*))
       (begin
         (add-type-name-reference #'->)
         (->* (map parse-type (syntax->list #'(dom ...))) (parse-type #'rest) (parse-values-type #'rng)))]
      [(dom ... rest ::: bound -> rng)
       (and (eq? (syntax-e #'->) '->) 
            (eq? (syntax-e #':::) '...)
            (identifier? #'bound))
       (begin
         (add-type-name-reference #'->)
         (let ([var (lookup (current-tvars) (syntax-e #'bound) (lambda (_) #f))])
           (if (not (Dotted? var))
               (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." (syntax-e #'bound))
               (make-Function
                (list
                 (make-arr-dots (map parse-type (syntax->list #'(dom ...)))
                                (parse-values-type #'rng)
                                (parameterize ([current-tvars (extend-env (list (syntax-e #'bound)) 
                                                                          (list (make-DottedBoth (make-F (syntax-e #'bound))))
                                                                          (current-tvars))])
                                  (parse-type #'rest))
                                (syntax-e #'bound)))))))]
      [(dom ... rest ::: -> rng)
       (and (eq? (syntax-e #'->) '->) 
            (eq? (syntax-e #':::) '...))
       (begin
         (add-type-name-reference #'->)
         (let ([bounds (filter (compose Dotted? cdr) (env-keys+vals (current-tvars)))])
           (when (null? bounds)
             (tc-error/stx stx "No type variable bound with ... in scope for ... type"))
           (unless (null? (cdr bounds))
             (tc-error/stx stx "Cannot infer bound for ... type"))
           (match-let ([(cons var (struct Dotted (t))) (car bounds)])
             (make-Function
              (list
               (make-arr-dots (map parse-type (syntax->list #'(dom ...)))
                              (parse-values-type #'rng)
                              (parameterize ([current-tvars (extend-env (list var)
                                                                        (list (make-DottedBoth t))
                                                                        (current-tvars))])
                                (parse-type #'rest))
                              var))))))]
      ;; has to be below the previous one
      [(dom ... -> rng) 
       (eq? (syntax-e #'->) '->)
       (begin
         (add-type-name-reference #'->)
         (->* (map parse-type (syntax->list #'(dom ...))) (parse-values-type #'rng)))]
      
      [(case-lambda tys ...) 
       (eq? (syntax-e #'case-lambda) 'case-lambda)
       (make-Function 
        (for/list ([ty (syntax->list #'(tys ...))])
          (let ([t (parse-type ty)])
            (match t
              [(Function: (list arr)) arr]
              [_ (tc-error/stx ty "Component of case-lambda type was not a function clause")]))))]
      [(Vectorof t) 
       (eq? (syntax-e #'Vectorof) 'Vectorof)
       (begin
         (add-type-name-reference #'Vectorof)
         (make-Vector (parse-type #'t)))]
      [(mu x t) 
       (and (identifier? #'x)
            (memq (syntax-e #'mu) '(mu Rec))
            (enable-mu-parsing))
       (let* ([var (syntax-e #'x)]
              [tvar (make-F var)])
         (add-type-name-reference #'mu)           
         (parameterize ([current-tvars (extend-env (list var) (list tvar) (current-tvars))])
           (let ([t (parse-type #'t)])
             (if (memq var (fv t))
                 (make-Mu var t)
                 t))))]
      [(U ts ...)
       (eq? (syntax-e #'U) 'U)
       (begin
         (add-type-name-reference #'U)
         (apply Un (map parse-type (syntax->list #'(ts ...)))))]
      [(Un-pat ts ...) 
       (eq? (syntax-e #'Un-pat) 'Un)
       (apply Un (map parse-type (syntax->list #'(ts ...))))]
      [(quot t)
       (eq? (syntax-e #'quot) 'quote)
       (-val (syntax-e #'t))]
      [(All . rest)
       (or (eq? (syntax-e #'All) 'All)
           (eq? (syntax-e #'All) '∀))
       (parse-all-type stx parse-type)]
      [(Opaque p?) 
       (eq? (syntax-e #'Opaque) 'Opaque)
       (begin
         (add-type-name-reference #'Opaque)
         (make-Opaque #'p? (syntax-local-certifier)))]
      [(Parameter t) 
       (eq? (syntax-e #'Parameter) 'Parameter)
       (let ([ty (parse-type #'t)])
         (add-type-name-reference #'Parameter)
         (-Param ty ty))]
      [(Parameter t1 t2)
       (eq? (syntax-e #'Parameter) 'Parameter)
       (begin
         (add-type-name-reference #'Parameter)
         (-Param (parse-type #'t1) (parse-type #'t2)))]
      
      [id
       (identifier? #'id)
       (cond 
         ;; if it's a type variable, we just produce the corresponding reference (which is in the HT)
         [(lookup (current-tvars) (syntax-e #'id) (lambda (_) #f))
          =>
          (lambda (e) (cond [(DottedBoth? e) (Dotted-t e)]
                            [(Dotted? e)
                             (tc-error "Type variable ~a must be used with ..." (syntax-e #'id))]
                            [else e]))]
         ;; if it's a type alias, we expand it (the expanded type is stored in the HT)
         [(lookup-type-alias #'id parse-type (lambda () #f))
          =>
          (lambda (t)
            ;(printf "found a type alias ~a~n" #'id)
            (add-type-name-reference #'id)
            t)]
         ;; if it's a type name, we just use the name
         [(lookup-type-name #'id (lambda () #f))
          (add-type-name-reference #'id)
          ;(printf "found a type name ~a~n" #'id)
          (make-Name #'id)]
         [(eq? '-> (syntax-e #'id))
          (tc-error/delayed "Incorrect use of -> type constructor")
          Err]
         [else
          (tc-error/delayed "Unbound type name ~a" (syntax-e #'id))
          Err])]      
      [(Opaque . rest) (eq? (syntax-e #'Opaque) 'Opqaue) (tc-error "Opaque: bad syntax")]
      [(U . rest) (eq? (syntax-e #'U) 'U) (tc-error "Union: bad syntax")]
      [(Vectorof . rest) (eq? (syntax-e #'Vectorof) 'Vectorof) (tc-error "Vectorof: bad syntax")]
      [(mu . rest) (eq? (syntax-e #'mu) 'mu) (tc-error "mu: bad syntax")]
      [(Un . rest) (eq? (syntax-e #'Un) 'Un) (tc-error "Union: bad syntax")]
      [(t ... -> . rest) (eq? (syntax-e #'->) '->) (tc-error "->: bad syntax")]
      

      [(id arg args ...)
       (let loop 
         ([rator (parse-type #'id)]
          [args (map parse-type (syntax->list #'(arg args ...)))])
         (match rator
           [(Name: _)
            (make-App rator args stx)]
           [(Poly: ns _)
            (unless (= (length args) (length ns))
              (tc-error "Wrong number of arguments to type ~a, expected ~a but got ~a" rator (length ns) (length args)))
            (instantiate-poly rator args)]
           [(Mu: _ _) (loop (unfold rator) args)]
           [(Error:) Err]
           [_ (tc-error/delayed "Type ~a cannot be applied, arguments were: ~a" rator args)
              Err]))
       #;
       (let ([ty (parse-type #'id)])
         #;(printf "ty is ~a" ty)
         (unless (Poly? ty)
           (tc-error "not a polymorphic type: ~a" (syntax-e #'id)))
         (unless (= (length (syntax->list #'(arg args ...))) (Poly-n ty))
           (tc-error "wrong number of arguments to type constructor ~a: expected ~a, got ~a" 
                     (syntax-e #'id)
                     (Poly-n ty)
                     (length (syntax->list #'(arg args ...)))))
         (instantiate-poly ty (map parse-type (syntax->list #'(arg args ...)))))]
      [t
       (or (boolean? (syntax-e #'t)) (number? (syntax-e #'t))
           (string? (syntax-e #'t)))
       (-val (syntax-e #'t))]
      [_ (tc-error "not a valid type: ~a" (syntax->datum stx))])))

(define (parse-values-type stx)
  (parameterize ([current-orig-stx stx])        
    (syntax-parse stx
      [(values tys ... dty :ddd bound:id)
       #:when (eq? (syntax-e #'values) 'values)
       (let ([var (lookup (current-tvars) (syntax-e #'bound) (lambda (_) #f))])
         (if (not (Dotted? var))
             (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." (syntax-e #'bound))             
             (make-ValuesDots (map parse-type (syntax->list #'(tys ...)))
                              (parameterize ([current-tvars (extend-env (list (syntax-e #'bound)) 
                                                                        (list (make-DottedBoth (make-F (syntax-e #'bound))))
                                                                        (current-tvars))])
                                (parse-type #'dty))
                              (syntax-e #'bound))))]
      [(values tys ... dty :ddd)
       #:when (and (eq? (syntax-e #'values) 'values))
       (add-type-name-reference #'values)
       (let ([bounds (filter (compose Dotted? cdr) (env-keys+vals (current-tvars)))])
         (when (null? bounds)
           (tc-error/stx stx "No type variable bound with ... in scope for ... type"))
         (unless (null? (cdr bounds))
           (tc-error/stx stx "Cannot infer bound for ... type"))
         (match-let ([(cons var (struct Dotted (t))) (car bounds)])
           (make-ValuesDots (map parse-type (syntax->list #'(tys ...)))
                            (parameterize ([current-tvars (extend-env (list var) 
                                                                      (list (make-DottedBoth t))
                                                                      (current-tvars))])
                              (parse-type #'dty))
                            var)))]
      [(values tys ...) 
       #:when (eq? (syntax-e #'values) 'values)
       (-values (map parse-type (syntax->list #'(tys ...))))]
      [(All . rest)
       #:when (or (eq? (syntax-e #'All) 'All)
                  (eq? (syntax-e #'All) '∀))
       (parse-all-type stx parse-values-type)]
      [t
       (-values (list (parse-type #'t)))])))

(define (parse-tc-results stx)
  (syntax-parse stx
    [(values t ...)
     #:when (eq? 'values (syntax-e #'values))
     (ret (map parse-type (syntax->list #'(t ...))) 
          (map (lambda (x) (make-NoFilter)) (syntax->list #'(t ...)))
          (map (lambda (x) (make-NoObject)) (syntax->list #'(t ...))))]
    [t (ret (parse-type #'t) (make-NoFilter) (make-NoObject))]))

(define parse-tc-results/id (parse/id parse-tc-results))

(define parse-type/id (parse/id parse-type))
|#