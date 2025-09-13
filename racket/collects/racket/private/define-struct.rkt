;; Based on
;;  (planet "struct.ss" ("ryanc" "macros.plt" 1 0)))

(module define-struct '#%kernel
  (#%require "define-et-al.rkt" "qq-and-or.rkt" "define.rkt" "../stxparam.rkt"
             "generic-methods.rkt"
             (for-syntax '#%kernel "define.rkt"
                         "procedure-alias.rkt"
                         "member.rkt"
                         "stx.rkt" "stxcase-scheme.rkt" "qq-and-or.rkt" "cond.rkt"
                         "define-et-al.rkt"
                         "stxloc.rkt" "qqstx.rkt"
                         "struct-info.rkt"
                         "struct-util.rkt"))

  (#%provide define-struct*
             define-struct/derived
             struct/derived
             struct-field-index
             struct-copy
             (for-syntax
              (rename checked-struct-info-rec? checked-struct-info?)))

  (define-values-for-syntax
    (struct:struct-field-info
     make-struct-field-info
     struct-field-info-rec?
     struct-field-info-ref
     struct-field-info-set!)
    (make-struct-type 'struct-field-info struct:struct-info
                      1 0 #f
                      (list (cons prop:struct-field-info
                                  (lambda (rec)
                                    (struct-field-info-ref rec 0))))))

  (define-values-for-syntax
    (struct:struct-auto-info 
     make-struct-auto-info 
     struct-auto-info-rec?
     struct-auto-info-ref
     struct-auto-info-set!)
    (make-struct-type 'struct-auto-info struct:struct-field-info
                      1 0 #f
                      (list (cons prop:struct-auto-info
                                  (lambda (rec)
                                    (struct-auto-info-ref rec 0))))))
                      

  (define-values-for-syntax
    (struct:checked-struct-info 
     make-checked-struct-info 
     checked-struct-info-rec?
     checked-struct-info-ref
     checked-struct-info-set!)
    (make-struct-type 'checked-struct-info struct:struct-auto-info
                      0 0 #f
                      null (current-inspector)
                      (lambda (v stx)
                        (raise-syntax-error
                         #f
                         "bad syntax;\n identifier for static struct-type information cannot be used as an expression"
                         stx))
                      null
                      (lambda (proc fields autos info)
                        (if (and (procedure? proc)
                                 (procedure-arity-includes? proc 0))
                            (values proc fields autos)
                            (raise-argument-error 'make-struct-info
                                                  "(procedure-arity-includes/c 0)"
                                                  proc)))))
  
  (define-for-syntax (self-ctor-transformer orig stx)
    (define (transfer-srcloc orig stx)
      (datum->syntax orig (syntax-e orig) stx orig))
    (syntax-case stx ()
      [(self arg ...) (datum->syntax stx
                                     (cons 
                                      (syntax-property
                                       (syntax-property (transfer-srcloc orig #'self)
                                                        'constructor-for
                                                        (syntax-local-introduce #'self))
                                       alias-of (syntax-local-introduce #'self))
                                      (syntax-e (syntax (arg ...))))
                                     stx
                                     stx)]
      [self (identifier? #'self)
       (syntax-property
        (syntax-property (transfer-srcloc orig #'self)
                         'constructor-for
                         (syntax-local-introduce #'self))
        alias-of (syntax-local-introduce #'self))]))
  
  (define-values-for-syntax (make-self-ctor-struct-info)
    (letrec-values ([(struct: make- ? ref set!)
                     (make-struct-type 'self-ctor-struct-info struct:struct-auto-info
                                       1 0 #f
                                       (list (cons prop:procedure
                                                   (lambda (v stx)
                                                     (self-ctor-transformer ((ref v 0)) stx))))
                                       (current-inspector) #f '(0))])
      make-))
  (define-values-for-syntax (make-self-ctor-checked-struct-info)
    (letrec-values ([(struct: make- ? ref set!)
                     (make-struct-type 'self-ctor-checked-struct-info struct:checked-struct-info
                                       1 0 #f
                                       (list (cons prop:procedure
                                                   (lambda (v stx)
                                                     (self-ctor-transformer ((ref v 0)) stx))))
                                       (current-inspector) #f '(0))])
      make-))

  (define-syntax-parameter struct-field-index
    (lambda (stx)
      (raise-syntax-error #f "allowed only within a structure type definition" stx)))

  (define-for-syntax (make-struct-field-index fields)
    (lambda (stx)
      (syntax-case stx ()
        [(_ id)
         (identifier? #'id)
         (let loop ([pos 0] [fields (syntax->list fields)])
           (cond
             [(null? fields)
              (raise-syntax-error #f "no such field" stx #'name)]
             [(free-identifier=? #'id (car fields))
              (datum->syntax #'here pos stx)]
             [else (loop (add1 pos) (cdr fields))]))])))

  (define (check-struct-type name what)
    (when what
      (unless (struct-type? what)
        (raise-argument-error name "(or/c struct-type? #f)" what)))
    what)

  (define (check-inspector name what)
    (when what
      (unless (inspector? what)
        (raise-argument-error name "(or/c inspector? #f)" what)))
    what)

  (define (check-reflection-name name what)
    (unless (symbol? what)
      (raise-argument-error name "symbol?" what))
    what)

  (define (check-property-alist name what)
    (unless (and (list? what)
                 (andmap (lambda (elem)
                           (and (pair? elem)
                                (struct-type-property? (car elem))))
                         what))
      (raise-argument-error name
                            "(listof (cons struct-type-property? any/c))"
                            what))
    what)

  (define-syntax (define-struct* stx)
    (syntax-case stx ()
      [(_ . rest)
       (with-syntax ([stx stx])
         #'(define-struct/derived stx . rest))]))

  (define-syntax (define-struct/derived full-stx)
    (define make-field list)
    (define field-id car)
    (define field-default-value cadr)
    (define field-auto? caddr)
    (define field-mutable? cadddr)

    (define (build-name id . parts)
      (datum->syntax
       id
       (string->symbol
        (apply string-append
               (map (lambda (p)
                      (if (syntax? p)
                          (symbol->string (syntax-e p))
                          p))
                    parts)))
       id))

    (define (bad why kw where . alt)
      (raise-syntax-error
       #f
       (format "~a ~a specification~a"
               why
               (if (string? kw)
                   kw
                   (syntax-e kw))
               where)
       stx
       (if (null? alt) kw (car alt))))

    (define (check-exprs orig-n ps what)
      (let loop ([nps (cdr ps)][n orig-n])
        (unless (zero? n)
          (unless (and (pair? nps)
                       (not (keyword? (syntax-e (car nps)))))
            (raise-syntax-error
             #f
             (format "bad syntax;\n expected ~a ~a~a after keyword~a"
                     orig-n
                     (or what "expression")
                     (if (= orig-n 1) "" "s")
                     (if (pair? nps)
                         ", found a keyword"
                         ""))
             stx
             (car ps)))
          (loop (cdr nps) (sub1 n)))))
    
    ;; Parse one field with a sequence of keyword-based specs:
    (define (parse-field f)
      (syntax-case f ()
        [id
         (identifier? #'id)
         (make-field #'id #f #f #f)]
        [(id p ...)
         (identifier? #'id)
         (let loop ([ps (syntax->list #'(p ...))]
                    [def-val #f]
                    [auto? #f]
                    [mutable? #f])
           (cond
            [(null? ps) (make-field #'id def-val auto? mutable?)]
            [(eq? '#:mutable (syntax-e (car ps)))
             (when mutable?
               (bad "redundant" (car ps) " for field"))
             (loop (cdr ps) def-val auto? #t)]
            #;
            [(eq? #:default (syntax-e (car ps)))
             (check-exprs 1 ps #f)
             (when def-val
               (bad "multiple" (car ps) " for field"))
             (loop (cddr ps) (cadr ps) auto? mutable?)]
            [(eq? '#:auto (syntax-e (car ps)))
             (when auto?
               (bad "redundant" (car ps) " for field"))
             (loop (cdr ps) def-val #t mutable?)]
            [else
             (raise-syntax-error
              #f
              (if (keyword? (syntax-e (car ps)))
                  "unrecognized field-specification keyword"
                  "expected a field-specification keyword")
              stx
              (car ps))]))]
        [_else
         (raise-syntax-error
          #f
          "bad syntax;\n expected a field identifier or a parenthesized identifier and field-specification sequence"
          stx
          f)]))

    (define (lookup config s)
      (cdr (assq s config)))

    (define (extend-config config s val)
      (cond
       [(null? config) (error 'struct "internal error: can't find config element: ~s" s)]
       [(eq? (caar config) s) (cons (cons s val) (cdr config))]
       [else (cons (car config) (extend-config (cdr config) s val))]))

    (define insp-keys
      "#:inspector, #:transparent, or #:prefab")

    ;; Parse sequence of keyword-based struct specs
    (define (parse-props fm p super-id)
      (let loop ([p p]
                 [config '((#:super . #f)
                           (#:inspector . #f)
                           (#:auto-value . #f)
                           (#:props . ())
                           (#:proplists . ())
                           (#:mutable . #f)
                           (#:guard . #f)
                           (#:constructor-name . #f)
                           (#:only-constructor? . #f)
                           (#:reflection-name . #f)
                           (#:name . #f)
                           (#:only-name? . #f)
                           (#:authentic . #f)
                           (#:sealed . #f)
                           (#:omit-define-values . #f)
                           (#:omit-define-syntaxes . #f))]
                 [nongen? #f])
        (cond
         [(null? p) config]
         [(eq? '#:super (syntax-e (car p)))
          (check-exprs 1 p #f)
          (when (lookup config '#:super)
            (bad "multiple" (car p) "s"))
          (when super-id
            (raise-syntax-error
             #f
             (string-append
              "bad syntax;\n"
              " #:super specification disallowed because a struct supertype id was\n"
              " supplied with the struct type id")
             stx
             (car p)))
          (loop (cddr p)
                (extend-config config '#:super (cadr p))
                nongen?)]
         [(memq (syntax-e (car p))
                '(#:guard #:auto-value))
          (let ([key (syntax-e (car p))])
            (check-exprs 1 p #f)
            (when (lookup config key)
              (bad "multiple" (car p) "s"))
            (when (and nongen?
                       (eq? key '#:guard))
              (bad "cannot provide" (car p) " for prefab structure type"))
            (loop (cddr p)
                  (extend-config config key (cadr p))
                  nongen?))]
         [(eq? '#:property (syntax-e (car p)))
          (check-exprs 2 p #f)
          (when nongen?
            (bad "cannot use" (car p) " for prefab structure type"))
          (loop (cdddr p)
                (extend-config config
                               '#:props
                               (cons (cons (cadr p) (caddr p))
                                     (lookup config '#:props)))
                nongen?)]
         [(eq? '#:properties (syntax-e (car p)))
          (check-exprs 1 p #f)
          (when nongen?
            ;; no error, since `#:properties null` should be allowed for prefab
            (void))
          (loop (cddr p)
                (extend-config config
                               '#:proplists
                               (cons #`(check-property-alist '#,fm #,(cadr p))
                                     (lookup config '#:proplists)))
                nongen?)]
         [(eq? '#:methods (syntax-e (car p)))
          ;; #:methods gen:foo [(define (meth1 x ...) e ...) ...]
          (check-exprs 2 p "argument")
          (define gen-id (cadr p))
          (define gen-defs (caddr p))
          (define args (cdddr p))
          (define gen-val
            (and (identifier? gen-id)
                 (syntax-local-value gen-id (lambda () #f))))
          (unless (generic-info? gen-val)
            (bad "the first argument to the"
                 (car p)
                 " is not a name for a generic interface"
                 (cadr p)))
          (unless (list? (syntax-e gen-defs))
            (bad "the second argument to the"
                 (car p)
                 " is not a parenthesized sequence of method definitions"
                 gen-defs))
          (loop (list* #'#:property
                       (quasisyntax/loc gen-id
                         (generic-property #,gen-id))
                       (quasisyntax/loc gen-id
                         (generic-method-table #,gen-id #,@gen-defs))
                       args)
                config
                nongen?)]
         [(eq? '#:inspector (syntax-e (car p)))
          (check-exprs 1 p #f)
          (when (lookup config '#:inspector)
            (bad "multiple" insp-keys "s" (car p)))
          (loop (cddr p)
                (extend-config config '#:inspector 
                               #`(check-inspector '#,fm #,(cadr p)))
                nongen?)]
         [(eq? '#:transparent (syntax-e (car p)))
          (when (lookup config '#:inspector)
            (bad "multiple" insp-keys "s" (car p)))
          (loop (cdr p)
                (extend-config config '#:inspector #'#f)
                nongen?)]
         [(eq? '#:authentic (syntax-e (car p)))
          (when nongen?
            (bad "cannot use" (car p) " for prefab structure type"))
          (when (lookup config '#:authentic)
            (bad "multiple" "#:authentic" "s" (car p)))
          (loop (cdr p)
                (extend-config config '#:authentic #'#t)
                nongen?)]
         [(eq? '#:sealed (syntax-e (car p)))
          (when nongen?
            (bad "cannot use" (car p) " for prefab structure type"))
          (when (lookup config '#:sealed)
            (bad "multiple" "#:sealed" "s" (car p)))
          (loop (cdr p)
                (extend-config config '#:sealed #'#t)
                nongen?)]
         [(or (eq? '#:constructor-name (syntax-e (car p)))
              (eq? '#:extra-constructor-name (syntax-e (car p))))
          (check-exprs 1 p "identifier")
          (when (lookup config '#:constructor-name)
            (bad "multiple" "#:constructor-name or #:extra-constructor-name" "s" (car p)))
          (unless (identifier? (cadr p))
            (bad "need an identifier after" (car p) "" (cadr p)))
          (loop (cddr p)
                (extend-config (extend-config config '#:constructor-name (cadr p))
                               '#:only-constructor?
                               (eq? '#:constructor-name (syntax-e (car p))))
                nongen?)]
         [(or (eq? '#:name (syntax-e (car p)))
              (eq? '#:extra-name (syntax-e (car p))))
          (check-exprs 1 p "identifier")
          (when (lookup config '#:name)
            (bad "multiple" "#:name or #:extra-name" "s" (car p)))
          (unless (identifier? (cadr p))
            (bad "need an identifier after" (car p) "" (cadr p)))
          (loop (cddr p)
                (extend-config (extend-config config '#:name (cadr p))
                               '#:only-name?
                               (eq? '#:name (syntax-e (car p))))
                nongen?)]
         [(eq? '#:reflection-name (syntax-e (car p)))
          (check-exprs 1 p "expression")
          (when (lookup config '#:reflection-name)
            (bad "multiple" "#:reflection-name keys" (car p)))
          (loop (cddr p)
                (extend-config config '#:reflection-name (cadr p))
                nongen?)]
         [(eq? '#:prefab (syntax-e (car p)))
          (when (lookup config '#:inspector)
            (bad "multiple" insp-keys "s" (car p)))
          (when (pair? (lookup config '#:props))
            (bad "cannot use" (car p) " for a structure type with properties"))
          (when (lookup config '#:sealed)
            (bad "cannot use" (car p) " for a sealed structure type"))
          (when (lookup config '#:guard)
            (bad "cannot use" (car p) " for a structure type with a guard"))
          (when (lookup config '#:authentic)
            (bad "cannot use" (car p) " for an authentic structure type"))
          (loop (cdr p)
                (extend-config config '#:inspector #''prefab)
                #t)]
         [(memq (syntax-e (car p))
                '(#:mutable #:omit-define-values #:omit-define-syntaxes))
          (let ([key (syntax-e (car p))])
            (when (lookup config key)
              (bad "redundant" (car p) ""))
            (loop (cdr p)
                  (extend-config config key #t)
                  nongen?))]
         [else
          (raise-syntax-error
           #f
           (if (keyword? (syntax-e (car p)))
               "unrecognized struct-specification keyword"
               "expected a struct-specification keyword")
           stx
           (car p))])))

    (define stx (syntax-case full-stx ()
                  [(_ stx . _) #'stx]))
    
    (syntax-case full-stx ()
      [(_ (fm . _) id (field ...) prop ...)
       (let-values ([(id super-id)
                     (if (identifier? #'id)
                         (values #'id #f)
                         (syntax-case #'id ()
                           [(id super-id) 
                            (and (identifier? #'id)
                                 (identifier? #'super-id))
                            (values #'id #'super-id)]
                           [_
                            (raise-syntax-error 
                             #f
                             "bad syntax;\n expected <id> for structure-type name or (<id> <id>) for name and supertype\n name"
                             stx
                             #'id)]))])
         (let-values ([(super-info super-autos super-info-checked?)
		       (if super-id
			   (let ([v (syntax-local-value super-id (lambda () #f))])
			     (if (struct-info? v)
				 (values (extract-struct-info v) 
                                         (if (struct-auto-info? v)
                                             (struct-auto-info-lists v)
                                             (list null null))
                                         (checked-struct-info-rec? v))
				 (raise-syntax-error
				  #f
				  (format "parent struct type not defined~a"
					  (if v
					      ";\n identifier does not name struct type information"
					      ""))
				  stx
				  super-id)))
			   ;; if there's no super type, it's like it was checked
			   (values #f #f #t))])
           (when (and super-info
                      (not (car super-info)))
             (raise-syntax-error
              #f
              "no structure type descriptor available for supertype"
              stx
              super-id))
           (let* ([field-stxes (syntax->list #'(field ...))]
                  [fields (map parse-field field-stxes)]
                  [dup (check-duplicate-identifier (map field-id fields))])
             (when dup
               (raise-syntax-error
                #f
                "duplicate field identifier"
                stx
                dup))
             (let ([auto-count
                    (let loop ([fields fields] [field-stxes field-stxes] [auto? #f])
                      (cond
                       [(null? fields) 0]
                       [(field-auto? (car fields))
                        (+ 1 (loop (cdr fields) (cdr field-stxes) #t))]
                       [auto?
                        (raise-syntax-error
                         #f
                         "non-auto field after an auto field disallowed"
                         stx
                         (car field-stxes))]
                       [else
                        (loop (cdr fields) (cdr field-stxes) #f)]))])
               (let*-values ([(inspector super-expr props proplists auto-val guard ctor-name ctor-only?
                                         reflect-name-expr mutable?
                                         omit-define-values? omit-define-syntaxes?
                                         info-name name-only?)
                              (let ([config (parse-props #'fm (syntax->list #'(prop ...)) super-id)])
                                (values (lookup config '#:inspector)
                                        (lookup config '#:super)
                                        (let ([l (lookup config '#:props)]
                                              [a? (lookup config '#:authentic)]
                                              [s? (lookup config '#:sealed)])
                                          (let ([l (if a?
                                                       (cons (cons #'prop:authentic #'#t)
                                                             l)
                                                       l)])
                                            (if s?
                                                (cons (cons #'prop:sealed #'#t)
                                                      l)
                                                l)))
                                        (lookup config '#:proplists)
                                        (lookup config '#:auto-value)
                                        (lookup config '#:guard)
                                        (lookup config '#:constructor-name)
                                        (lookup config '#:only-constructor?)
                                        (lookup config '#:reflection-name)
                                        (lookup config '#:mutable)
                                        (lookup config '#:omit-define-values)
                                        (lookup config '#:omit-define-syntaxes)
                                        (lookup config '#:name)
                                        (lookup config '#:only-name?)))]
                             [(self-ctor?)
                              (and ctor-name (or (and (not name-only?)
                                                      (bound-identifier=? id ctor-name))
                                                 (and info-name
                                                      (bound-identifier=? info-name ctor-name))))]
                             [(name-as-ctor?) (or self-ctor? (not ctor-only?))])
                 (when mutable?
                   (for-each (lambda (f f-stx)
                               (when (field-mutable? f)
                                 (raise-syntax-error
                                  #f
                                  "redundant #:mutable specification in field"
                                  stx
                                  f-stx)))
                             fields field-stxes))
                 (let ([struct: (build-name id "struct:" id)]
                       [make- (if ctor-name
                                  (if self-ctor?
                                      (if omit-define-syntaxes?
                                          ctor-name
                                          (car (generate-temporaries (list id))))
                                      ctor-name)
                                  (build-name id "make-" id))]
                       [? (build-name id id "?")]
                       [sels (map (lambda (f)
                                    (build-name id ; (field-id f) 
                                                id "-" (field-id f)))
                                  fields)]
                       [super-struct: (if super-info
                                          (or (car super-info)
                                              (raise-syntax-error
                                               #f
                                               "no structure type descriptor available for supertype"
                                               stx
                                               super-id))
                                          (and super-expr
                                               #`(let ([the-super #,super-expr])
                                                   (if (struct-type? the-super)
                                                       the-super
                                                       (check-struct-type 'fm the-super)))))]
                       [prune (lambda (stx)
                                (syntax-protect
                                 (identifier-prune-lexical-context stx
                                                                   (list (syntax-e stx) '#%top))))]
                       [reflect-name-expr (if reflect-name-expr
                                              (syntax-case reflect-name-expr (quote)
                                                [(quote id)
                                                 (identifier? #'id)
                                                 ;; No need to generate run-time test for a symbol:
                                                 reflect-name-expr]
                                                [else
                                                 (quasisyntax (check-reflection-name 'fm #,reflect-name-expr))])
                                              (quasisyntax '#,id))])
                   
                   (define struct-name-size (string-length (symbol->string (syntax-e id))))
                   (define struct-name/locally-introduced (syntax-local-introduce id))
                   (define struct-name-to-predicate-directive
                     (vector (syntax-local-introduce ?)
                             0
                             struct-name-size
                             struct-name/locally-introduced
                             0
                             struct-name-size))
                   
                   (define struct-name-to-old-style-maker-directive
                     (if ctor-name
                         #f
                         (vector (syntax-local-introduce make-)
                                 5
                                 struct-name-size
                                 struct-name/locally-introduced
                                 0
                                 struct-name-size)))
                   
                   (define (struct-name-to-selector/mutator-directive id-stx selector?)
                     (vector (syntax-local-introduce id-stx)
                             (if selector? 0 4)
                             struct-name-size
                             struct-name/locally-introduced
                             0
                             struct-name-size))
                   (define (field-to-selector/mutator-directive field id-stx selector?)
                     (define fld-size (string-length (symbol->string (syntax-e (field-id field)))))
                     (vector (syntax-local-introduce id-stx)
                             (+ (if selector? 1 5) struct-name-size)
                             fld-size
                             (syntax-local-introduce (field-id field))
                             0
                             fld-size))
                   
                   (define-values (sets field-to-mutator-directives sets-auto-count)
                     (let loop ([fields fields])
                       (cond
                         [(null? fields) (values null null 0)]
                         [(not (or mutable? (field-mutable? (car fields))))
                          (loop (cdr fields))]
                         [else
                          (define-values (other-sets other-directives count)
                            (loop (cdr fields)))
                          (define count* (if (field-auto? (car fields))
                                             (+ count 1)
                                             count))
                          (define this-set
                            (build-name id ; (field-id (car fields))
                                        "set-"
                                        id
                                        "-"
                                        (field-id (car fields))
                                        "!"))
                          (values (cons this-set other-sets)
                                  (cons (field-to-selector/mutator-directive (car fields)
                                                                             this-set
                                                                             #f)
                                        other-directives)
                                  count*)])))
                   
                   (define all-directives
                     (append 
                      (list struct-name-to-predicate-directive)
                      (if struct-name-to-old-style-maker-directive
                          (list struct-name-to-old-style-maker-directive)
                          '())
                      field-to-mutator-directives
                      (map (λ (field sel)
                             (field-to-selector/mutator-directive field sel #t))
                           fields
                           sels)
                      (map (λ (sel)
                             (struct-name-to-selector/mutator-directive
                              sel
                              #t))
                           sels)
                      (map (λ (mut)
                             (struct-name-to-selector/mutator-directive
                              mut
                              #f))
                           sets)))
                   
                   (let* ([run-time-defns
                          (lambda ()
                            (quasisyntax/loc stx
                              (define-values (#,struct: #,make- #,? #,@sels #,@sets)
                                (let-values ([(struct: make- ? -ref -set!)
                                              (syntax-parameterize ([struct-field-index
                                                                     (make-struct-field-index (quote-syntax #,(map field-id fields)))])
                                                (make-struct-type #,reflect-name-expr
                                                                  #,super-struct:
                                                                  #,(- (length fields) auto-count)
                                                                  #,auto-count
                                                                  #,auto-val
                                                                  #,(cond
                                                                     [(and (null? props) (null? proplists))
                                                                      #'null]
                                                                     [(null? proplists)
                                                                      #`(list #,@(map (lambda (p)
                                                                                        #`(cons #,(car p) #,(cdr p)))
                                                                                      props))]
                                                                     [else
                                                                      #`(list* #,@(map (lambda (p)
                                                                                         #`(cons #,(car p) #,(cdr p)))
                                                                                       props)
                                                                               (append #,@proplists))])
                                                                  #,(or inspector
                                                                        #`(current-inspector))
                                                                  #f
                                                                  '#,(let loop ([i 0]
                                                                                [fields fields])
                                                                       (cond
                                                                        [(null? fields) null]
                                                                        [(field-auto? (car fields)) null]
                                                                        [(not (or mutable? (field-mutable? (car fields))))
                                                                         (cons i (loop (add1 i) (cdr fields)))]
                                                                        [else (loop (add1 i) (cdr fields))]))
                                                                  #,guard
                                                                  '#,(if ctor-only? ctor-name id)))])
                                  (values struct: make- ?
                                          #,@(let loop ([i 0][fields fields])
                                               (if (null? fields)
                                                   null
                                                   (cons #`(make-struct-field-accessor -ref #,i '#,(field-id (car fields)))
                                                         (loop (add1 i) (cdr fields)))))
                                          #,@(let loop ([i 0][fields fields])
                                               (if (null? fields)
                                                   null
                                                   (if (not (or mutable? (field-mutable? (car fields))))
                                                       (loop (add1 i) (cdr fields))
                                                       (cons #`(make-struct-field-mutator -set! #,i '#,(field-id (car fields)))
                                                             (loop (add1 i) (cdr fields)))))))))))]
                         [compile-time-defns
                          (lambda (body-only?)
                            (let* ([protect (lambda (sel)
                                              (and sel
                                                   (if (syntax-e sel)
                                                       #`(quote-syntax #,(prune sel))
                                                       sel)))]
                                   [include-autos? (or super-info-checked?
                                                       name-as-ctor?
                                                       (and super-autos
                                                            (or (pair? (car super-autos))
                                                                (pair? (cadr super-autos))))
                                                       (positive? auto-count))]
                                   [mk-info (if super-info-checked?
                                                (if name-as-ctor?
                                                    #'make-self-ctor-checked-struct-info
                                                    #'make-checked-struct-info)
                                                (if name-as-ctor?
                                                    #'make-self-ctor-struct-info
                                                    (if include-autos?
                                                        #'make-struct-auto-info
                                                        #'make-struct-field-info)))]
                                   [define-syntax-body
                                     #`(#,mk-info
                                        (lambda ()
                                          (list
                                           (quote-syntax #,(prune struct:))
                                           (quote-syntax #,(prune (if (and ctor-name self-ctor?)
                                                                      ctor-name
                                                                      make-)))
                                           (quote-syntax #,(prune ?))
                                           (list
                                            #,@(map protect (reverse sels))
                                            #,@(if super-info
                                                   (map protect (list-ref super-info 3))
                                                   (if super-expr
                                                       '(#f)
                                                       null)))
                                           (list
                                            #,@(reverse
                                                (let loop ([fields fields][sets sets])
                                                  (cond
                                                    [(null? fields) null]
                                                    [(not (or mutable? (field-mutable? (car fields))))
                                                     (cons #f (loop (cdr fields) sets))]
                                                    [else
                                                     (cons (protect (car sets))
                                                           (loop (cdr fields) (cdr sets)))])))
                                            #,@(if super-info
                                                   (map protect (list-ref super-info 4))
                                                   (if super-expr
                                                       '(#f)
                                                       null)))
                                           #,(if super-id
                                                 (protect super-id)
                                                 (if super-expr
                                                     #f
                                                     #t))))
                                        '#,(map field-id (reverse fields))
                                        #,@(if include-autos?
                                               (list #`(list (list #,@(map protect 
                                                                           (list-tail sels (- (length sels) auto-count)))
                                                                   #,@(if super-autos
                                                                          (map protect (car super-autos))
                                                                          null))
                                                             (list #,@(map protect
                                                                           (list-tail sets (max 0 (- (length sets) sets-auto-count))))
                                                                   #,@(if super-autos
                                                                          (map protect (cadr super-autos))
                                                                          null))))
                                               null)
                                        #,@(if name-as-ctor?
                                               (list #`(lambda () (quote-syntax #,make-)))
                                               null))])
                              (if body-only?
                                  define-syntax-body
                                  (quasisyntax/loc stx
                                    (define-syntaxes (#,(if name-only? info-name id))
                                      #,define-syntax-body)))))]
                         [extra-compile-time-defs
                          (lambda ()
                            (cond
                              [(and info-name (not name-only?))
                               (when omit-define-syntaxes?
                                 (raise-syntax-error #f "#:extra-name cannot be combined with #:omit-define-syntaxes" stx))
                               ; reuse existing value
                               (list #`(define-syntaxes (#,info-name) (syntax-local-value #'#,id)))]
                              [else null]))])
                     (let ([result
                            (cond
                             [(and (not omit-define-values?) (not omit-define-syntaxes?))
                              (if (eq? (syntax-local-context) 'top-level)
                                  ;; Top level: declare names to be bound by `define',
                                  ;; but put run-time expressions after `define-syntaxes'
                                  ;; to they can refer to bindings that are bound by
                                  ;; `define-syntaxes' (e.g. use of the constructor name
                                  ;; in the body of a property value that is a procedure)
                                  #`(begin 
                                      (define-syntaxes (#,struct: #,make- #,? #,@sels #,@sets) (values))
                                      #,(compile-time-defns #f)
                                      #,@(extra-compile-time-defs)
                                      #,(run-time-defns))
                                  ;; Other contexts: order should't matter:
                                  #`(begin 
                                      #,(run-time-defns) 
                                      #,(compile-time-defns #f)
                                      #,@(extra-compile-time-defs)))]
                             [omit-define-syntaxes?
                              #`(begin
                                  #,(run-time-defns)
                                  #,@(extra-compile-time-defs))]
                             [omit-define-values?
                              #`(begin
                                  #,(compile-time-defns #f)
                                  #,@(extra-compile-time-defs))]
                             [else #'(begin)])])
                       (syntax-protect
                        (syntax-property
                         (if super-id
                             (syntax-property result 
                                              'disappeared-use 
                                              (syntax-local-introduce super-id))
                             result)
                         'sub-range-binders
                          all-directives))))))))))]
      [(_ _ id . _)
       (not (or (identifier? #'id)
                (and (syntax->list #'id)
                     (= 2 (length (syntax->list #'id)))
                     (andmap identifier? (syntax->list #'id)))))
       (raise-syntax-error
        #f
        "bad syntax;\n expected <id> for structure-type name or (<id> <id>) for name and supertype\n name"
        stx
        #'id)]
      [(_ _ id (field ...) . _)
       (begin
         (for-each parse-field (syntax->list #'(field ...)))
         (raise-syntax-error
          #f
          "bad syntax after field sequence"
          stx))]
      [(_ _ id fields . _)
       (raise-syntax-error
        #f
        "bad syntax;\n expected a parenthesized sequence of field descriptions"
        stx
        #'fields)]
      [(_ _ id)
       (raise-syntax-error
        #f
        "bad syntax;\n missing fields"
        stx)]
      [_
       (raise-syntax-error
        #f
        "bad syntax"
        stx)]))
        
  (define-syntax (struct/derived stx)
    (define (config-has-name? config)
      (cond
        [(syntax? config) (config-has-name? (syntax-e config))]
        [(pair? config) (or (eq? (syntax-e (car config)) '#:constructor-name)
                            (eq? (syntax-e (car config)) '#:extra-constructor-name)
                            (config-has-name? (cdr config)))]
        [else #f]))
    (syntax-case stx ()
      [(_ orig id super-id (fields ...) config ...)
       (and (identifier? #'id)
            (identifier? #'super-id))
       (if (not (config-has-name? #'(config ...)))
           (syntax/loc stx 
             (define-struct/derived orig 
               (id super-id)
               (fields ...)
               #:constructor-name id
               config ...))
           (syntax/loc stx 
             (define-struct/derived orig 
               (id super-id)
               (fields ...) 
               config ...)))]
      [(_ orig id (fields ...) config ...)
       (identifier? #'id)
       (if (not (config-has-name? #'(config ...)))
           (syntax/loc stx 
             (define-struct/derived orig 
               id
               (fields ...) 
               #:constructor-name id 
               config ...))
           (syntax/loc stx 
             (define-struct/derived orig 
               id
               (fields ...) 
               config ...)))]
      [(_ orig id . rest)
       (identifier? #'id)
       (syntax/loc stx
         (define-struct/derived orig id . rest))]
      [(_ thing . _) (raise-syntax-error #f
                                         "expected an identifier for the structure type name"
                                         stx
                                         #'thing)]))

  ;; findf :: (a -> boolean?) -> (listof a) -> (or/c a #f)
  (define-for-syntax (findf f xs)
    (cond
      [(null? xs) #f]
      [else (define e (car xs))
            (if (f e) e (findf f (cdr xs)))]))

  ;; take :: (listof a) -> number? -> (listof a)
  (define-for-syntax (take xs n)
    (cond
      [(= n 0) '()]
      [(null? xs) xs]
      [else (cons (car xs) (take (cdr xs) (sub1 n)))]))

  (define-for-syntax (find-accessor/no-field-info the-struct-info fld stx)
    (define accessors (list-ref the-struct-info 3))
    (define parent (list-ref the-struct-info 5))
    (define num-fields (length accessors))
    (define num-super-fields
      (if (identifier? parent)
          (let-values ([(parent-struct-info _) (id->struct-info parent stx)])
            (length (cadddr parent-struct-info)))
          0))
    (define num-own-fields (- num-fields num-super-fields))
    (define own-accessors (take accessors num-own-fields))
    (define struct-name (predicate->struct-name #f stx (list-ref the-struct-info 2)))
    (define accessor-name (string->symbol (format "~a-~a" struct-name (syntax-e fld))))
    (or (findf (λ (a) (eq? accessor-name (syntax-e a))) own-accessors)
        (raise-syntax-error
         #f "field name not associated with the given structure type"
         stx fld)))

  (define-for-syntax (find-accessor/field-info the-struct-info the-field-info fld stx)
    (define accessors (list-ref the-struct-info 3))
    (define num-own-fields (length the-field-info))
    (define own-accessors (take accessors num-own-fields))
    (car
     (or (findf (λ (a) (eq? (syntax-e fld) (cdr a))) (map cons own-accessors the-field-info))
         (raise-syntax-error
          #f "field name not associated with the given structure type"
          stx fld))))

  (define-for-syntax (find-accessor the-struct-info maybe-field-info fld stx)
    (if maybe-field-info
        (find-accessor/field-info the-struct-info maybe-field-info fld stx)
        (find-accessor/no-field-info the-struct-info fld stx)))

  (define-for-syntax (id->struct-info id stx)
    (define compile-time-info (syntax-local-value id (lambda () #f)))
    (unless (struct-info? compile-time-info)
      (raise-syntax-error #f "identifier is not bound to a structure type" stx id))
    (values (extract-struct-info compile-time-info)
            (and (struct-field-info? compile-time-info)
                 (struct-field-info-list compile-time-info))))

  (define-for-syntax (struct-copy-core stx)
    (with-syntax ([(form-name info struct-expr field+val ...) stx])
      (define ans (syntax->list #'(field+val ...)))
      ;; Check syntax:
      (unless (identifier? #'info)
        (raise-syntax-error #f "not an identifier for structure type" stx #'info))

      (for-each (lambda (an)
                  (syntax-case an ()
                    [(field val)
                     (unless (identifier? #'field)
                       (raise-syntax-error #f
                                           "not an identifier for field name"
                                           stx
                                           #'field))]
                    [(field #:parent p val)
                     (unless (identifier? #'field)
                       (raise-syntax-error #f
                                           "not an identifier for field name"
                                           stx
                                           #'field))
                     (unless (identifier? #'p)
                       (raise-syntax-error #f
                                           "not an identifier for parent struct name"
                                           stx
                                           #'field))]
                    [_
                     (raise-syntax-error #f
                                         (string-append
                                          "bad syntax;\n"
                                          " expected a field update of the form (<field-id> <expr>)\n"
                                          " or (<field-id> #:parent <parent-id> <expr>)")
                                         stx
                                         an)]))
                ans)

      (define-values (the-struct-info maybe-field-info) (id->struct-info #'info stx))
      (define construct (cadr the-struct-info))
      (define pred (caddr the-struct-info))
      (define accessors (cadddr the-struct-info))
      (define parent (list-ref the-struct-info 5))

      (define (ensure-really-parent id)
        (let loop ([parent parent])
          (cond
            [(eq? parent #t)
             (raise-syntax-error #f "identifier not bound to a parent struct" stx id)]
            [(not parent)
             (raise-syntax-error #f "parent struct information not known" stx id)]
            [(free-identifier=? id parent) (void)]
            [else
             (let ([v (syntax-local-value parent (lambda () #f))])
               (unless (struct-info? v)
                 ;; this is possible: https://gist.github.com/deeglaze/6ae7424cc2d093661df2
                 (raise-syntax-error #f "unknown parent struct" stx id))
               (let ([v (extract-struct-info v)])
                 (loop (list-ref v 5))))])))

      (define new-fields
        (map (lambda (an)
               (syntax-case an ()
                 [(field expr)
                  (list (find-accessor the-struct-info maybe-field-info #'field stx)
                        #'expr
                        (car (generate-temporaries (list #'field))))]
                 [(field #:parent id expr)
                  (begin
                    (ensure-really-parent #'id)
                    (let-values ([(the-struct-info maybe-field-info) (id->struct-info #'id stx)])
                      (list (find-accessor the-struct-info maybe-field-info #'field stx)
                            #'expr
                            (car (generate-temporaries (list #'field))))))]))
             ans))

      ;; new-binding-for : syntax[field-name] -> (union syntax[expression] #f)
      (define (new-binding-for f)
        (ormap (lambda (new-field)
                 (and (free-identifier=? (car new-field) f)
                      (caddr new-field)))
               new-fields))

      (unless construct
        (raise-syntax-error #f
                            "constructor not statically known for structure type"
                            stx
                            #'info))
      (unless pred
        (raise-syntax-error #f
                            "predicate not statically known for structure type"
                            stx
                            #'info))

      (define dests (map car new-fields))

      ;; Check for duplicates using dests, not as, because mod=? as might not be id=?
      (let ([dupe (check-duplicate-identifier dests)])
        (when dupe
          (raise-syntax-error #f
                              "duplicate field assignment"
                              stx
                              ;; Map back to an original field:
                              (ormap (lambda (nf)
                                       (and nf
                                            (free-identifier=? dupe (car nf))
                                            (car nf)))
                                     (reverse new-fields)))))

      ;; the actual result
      #`(let ([the-struct struct-expr])
          #,(syntax-property #'(void) 'disappeared-use (syntax-local-introduce #'info))
          (if (#,pred the-struct)
              (let #,(map (lambda (new-field)
                            #`[#,(caddr new-field) #,(cadr new-field)])
                          new-fields)
                (#,construct
                 #,@(map
                     (lambda (field) (or (new-binding-for field)
                                         #`(#,field the-struct)))
                     (reverse accessors))))
              (raise-argument-error 'form-name
                                    #,(format "~a?" (syntax-e #'info))
                                    the-struct)))))

  (define-syntax (struct-copy stx)
    (if (not (eq? (syntax-local-context) 'expression))
        (quasisyntax/loc stx (#%expression #,stx))
        (struct-copy-core stx))))
