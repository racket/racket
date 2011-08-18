#lang scheme/base
(require "../utils/utils.rkt")

(require mzlib/struct mzlib/pconvert
         racket/match
         syntax/boundmap
         "free-variance.rkt"
         "interning.rkt"
	 racket/syntax unstable/match unstable/struct
         mzlib/etc
         scheme/contract
         (for-syntax
          scheme/list
          (only-in racket/syntax generate-temporary)
          racket/match
          (except-in syntax/parse id identifier keyword)
          scheme/base
          syntax/struct
          syntax/stx
          scheme/contract
	  racket/syntax
          (rename-in (except-in (utils utils stxclass-util) bytes byte-regexp regexp byte-pregexp pregexp)
                     [id* id]
                     [keyword* keyword])))

(provide == defintern hash-id (for-syntax fold-target))

;; seq: interning-generated count that is used to compare types (type<).
;; free-vars: cached free type variables
;; free-idxs: cached free dot sequence variables
;; stx: originating syntax for error-reporting
(define-struct Rep (seq free-vars free-idxs stx) #:transparent)

;; evil tricks for hygienic yet unhygienic-looking reference
;; in say def-type for type-ref-id
(define-for-syntax fold-target #'fold-target)
(define-for-syntax default-fields (list #'seq #'free-vars #'free-idxs #'stx))

;; parent is for struct inheritance.
;; ht-stx is the identifier of the intern-table
;; key? is #f iff the kind generated should not be interned.
(define-for-syntax (mk parent ht-stx key?)
  (define-syntax-class opt-contract-id
    #:attributes (i contract)
    (pattern i:id
             #:with contract #'any/c)
    (pattern [i:id contract]))
  ;; unhygienic struct function generation
  (define-syntax-class (idlist name)
    #:attributes ((i 1) (contract 1) fields maker pred (accessor 1))
    (pattern (oci:opt-contract-id ...)
             #:with (i ...) #'(oci.i ...)
             #:with (contract ...) #'(oci.contract ...)
             #:with fields #'(i ...)
             #:with (_ maker pred accessor ...) (build-struct-names name (syntax->list #'fields) #f #t name)))

  ;; applies f to all fields and combines the results.
  ;; (construction prevents duplicates)
  (define (combiner f flds)
    (syntax-parse flds
      [() #'#hasheq()]
      [(e) #`(#,f e)]
      [(e ...) #`(combine-frees (list (#,f e) ...))]))

  (define-splicing-syntax-class frees-pat
    #:transparent
    #:attributes (f1 f2)
    (pattern (~seq f1:expr f2:expr))
    ;; [#:frees #f] pattern in e.g. def-type means no free vars or idxs.
    (pattern #f
             #:with f1 #'#hasheq()
             #:with f2 #'#hasheq())
    ;; [#:frees (λ (f) ...)] should combine free variables or idxs accordingly
    ;; (given the respective accessor functions)
    (pattern e:expr
             #:with f1 #'(e Rep-free-vars)
             #:with f2 #'(e Rep-free-idxs)))

  ;; fold-pat takes fold-name (e.g. App-fold) and produces the
  ;; pattern for the match as
  (define-syntax-class (fold-pat fold-name)
    #:transparent
    #:attributes (e proc)
    (pattern #:base
             #:with e fold-target
             #:with proc #`(procedure-rename
                            (lambda () #,fold-target)
                            '#,fold-name))
    (pattern match-expander:expr
             ;; Doubly quoted. First unquote at (*1). Second at (*2)
             #:with e #'#'match-expander
             #:with proc #`(procedure-rename
                            ;; still doubly quoted.
                            (lambda () #'match-expander)
                            '#,fold-name)))

  (define-syntax-class form-name
    (pattern name:id
             ;; Type -> Type:
             #:with match-expander (format-id #'name "~a:" #'name)
             ;; Type -> Type-fold
             #:with fold (format-id #f "~a-fold" #'name)
             ;; symbol made keyword of given type's name (e.g. Type -> #:Type)
             #:with kw (string->keyword (symbol->string (syntax-e #'name)))
             ;; Type -> *Type
             #:with *maker (format-id #'name "*~a" #'name)))

  (define (key->list key? v) (if key? (list v) (list)))
  (lambda (stx)
    (syntax-parse stx
      [(dform name:form-name ;; e.g. Function
              ;; field/contract pairs e.g. ([rator Type/c] [rand Type/c])
              (~var flds (idlist #'name))
              (~or
               (~optional (~and (~fail #:unless key? "#:key not allowed")
                                ;; expression evaluates to intern key.
                                ;; e.g. (list rator rand)
                                [#:key key-expr:expr])
                          #:defaults ([key-expr #'#f]))
               ;; intern? is explicitly given when other fields of the type
               ;; shouldn't matter. (e.g. Opaque)
               ;; or need further processing (e.g. fld)
               (~optional [#:intern intern?:expr]
                          #:defaults
                          ([intern? (syntax-parse #'flds.fields
                                      [() #'#f]
                                      [(f) #'f]
                                      [(fields ...) #'(list fields ...)])]))
               ;; expression that when given a "get free-variables"
               ;; function, combines the results in the expected pashion.
               (~optional [#:frees frees:frees-pat]
                          #:defaults
                          ([frees.f1 (combiner #'Rep-free-vars #'flds.fields)]
                           [frees.f2 (combiner #'Rep-free-idxs #'flds.fields)]))
               ;; This tricky beast is for defining the type/filter/etc.'s
               ;; part of the fold. The make-prim-type's given
               ;; rec-ids are bound in this expression's context.
               ;; The defining primitive's fields' names are bound as
               ;; the fields' values.
               (~optional [#:fold-rhs (~var fold-rhs (fold-pat #'name.fold))]
                          #:defaults ;; defaults to folding down all fields.
                          ([fold-rhs.proc
                            ;; This quote makes the inner quasiquote be
                            ;; evaluated later (3rd element of the hashtable)
                            ;; in mk-fold.
                            ;; Thus only def-type'd entities will be properly
                            ;; folded down.
                            #'(procedure-rename
                               (lambda ()
                                 #`(name.*maker (#,type-rec-id flds.i) ...))
                               'name.fold)]))
               ;; how do we contract a value of this type?
               (~optional [#:contract contract:expr]
                          ;; defaults to folding down all fields.
                          #:defaults ([contract
                                       #'(->* (flds.contract ...)
                                              (#:syntax (or/c syntax? #f))
                                              flds.pred)]))
               (~optional (~and #:no-provide no-provide?))) ...)
       (with-syntax
         ;; makes as many underscores as default fields (+1 for key? if provided)
        ([(ign-pats ...) (let loop ([fs default-fields])
                           (if (empty? fs)
                               (key->list key? #'_)
                               (cons #'_ (loop (cdr fs)))))]
         ;; has to be down here to refer to #'contract
         [provides (if (attribute no-provide?)
                       #'(begin)
                       #'(begin
                           (provide name.match-expander flds.pred flds.accessor ...)
                           (provide/cond-contract (rename name.*maker flds.maker contract))))])
        #`(begin
            ;; struct "name" defined here.
            (define-struct (name #,parent) flds.fields #:inspector #f)
            (define-match-expander name.match-expander
              (lambda (s)
                (syntax-parse s
                  [(_ . fields)
                   ;; skips past ignores and binds fields for struct "name"
                   #:with pat (syntax/loc s (ign-pats ... . fields))
                   ;; This is the match (struct struct-id (pat ...)) form.
                   (syntax/loc s (struct name pat))])))
            ;; set the type's keyword in the hashtable to its
            ;; match expander, fields and fold-rhs's for further construction.
            (begin-for-syntax
              (hash-set! #,ht-stx 'name.kw (list #'name.match-expander #'flds.fields fold-rhs.proc #f)))
            #,(quasisyntax/loc stx
                (with-cond-contract name ([name.*maker contract])
                     #,(quasisyntax/loc #'name
                         (defintern (name.*maker . flds.fields)
                           flds.maker intern?
                           #:extra-args
                           frees.f1 frees.f2
                           #:syntax [orig-stx #f]
                           #,@(key->list key? #'key-expr)))))
            provides))])))

;; rec-ids are identifiers that are of the folded type, so we recur on them.
;; kws is e.g. '(#:Type #:Filter #:Object #:PathElem)
(define-for-syntax (mk-fold ht type-rec-id rec-ids kws)
  (lambda (stx)
    (define new-ht (hash-copy ht))
    (define-syntax-class clause
      (pattern
       ;; Given name, matcher.
       (k:keyword #:matcher matcher pats ... e:expr)
       #:attr kw (attribute k.datum)
       #:attr val (list #'matcher
                        (syntax/loc this-syntax (pats ...))
                        (lambda () #'e)
                        this-syntax))
      ;; Match on a type (or filter etc) case with keyword k
      ;; pats are the unignored patterns (say for rator rand)
      ;; and e is the expression that will run as fold-rhs.
      (pattern
       (k:keyword pats ... e:expr)
       #:attr kw (syntax-e #'k)
        ;; no given name. Use "keyword:"
       #:attr val (list (format-id stx "~a:" (attribute kw))
                        (syntax/loc this-syntax (pats ...))
                        (lambda () #'e)
                        this-syntax)))
    #|
    e.g. #:App (list #'App: (list #'rator #'rand)
                     (lambda () #'(*App (type-rec-id rator)
                                        (map type-rec-id rands)
                                        stx))
                     <stx>)
    |#
    (define (gen-clause k v)
      (match v
        [(list match-expander pats body-f src)
         ;; makes [(Match-name all-patterns ...) body]
         (define pat (quasisyntax/loc (or src stx)
                                      (#,match-expander  . #,pats)))
         (quasisyntax/loc (or src stx) (#,pat #,(body-f)))]))

    (define (no-duplicates? lst)
      (cond [(empty? lst) #t]
            [(member (car lst) (cdr lst)) #f]
            [else (no-duplicates? (cdr lst))]))

    ;; Accept only keywords in the given list.
    (define-syntax-class (keyword-in kws)
      #:attributes (datum)
      (pattern k:keyword
               #:fail-unless (memq (attribute k.datum) kws) (format "expected keyword in ~a" kws)
               #:attr datum (attribute k.datum)))
    ;; makes a keyword to expr hash table out of given keyword expr pairs.
    (define-syntax-class (sized-list kws)
      #:description (format "keyword expr pairs matching with keywords in the list ~a" kws)
      (pattern ((~seq (~var k (keyword-in kws)) e:expr) ...)
               #:when (no-duplicates? (attribute k.datum))
               #:attr mapping (for/hash ([k* (attribute k.datum)]
                                         [e* (attribute e)])
                                (values k* e*))))
    (syntax-parse stx
      [(tc (~var recs (sized-list kws)) ty clauses:clause ...)
       ;; map defined types' keywords to their given fold-rhs's.
       ;; This is done with a new copy of the hash generated in mk
       ;; so we can give special cases for only specific elements.
       (for ([k (attribute clauses.kw)]
             [v (attribute clauses.val)])
         (hash-set! new-ht k v))
       (with-syntax ([(let-clauses ...)
                      (for/list ([rec-id rec-ids]
                                 [k kws])
                        ;; Each rec-id binds to their corresponding given exprs
                        ;; rec-ids and kws correspond pointwise.
                        #`[#,rec-id #,(hash-ref (attribute recs.mapping) k
                                                #'values)])]
                     [(match-clauses ...)
                      ;; create all clauses we fold on, with keyword/body
                      (hash-map new-ht gen-clause)]
                     [error-msg (quasisyntax/loc stx (error 'tc "no pattern for ~a" #,fold-target))])
         #`(let (let-clauses ...
                 ;; binds #'fold-target to the given element to fold down.
                 ;; e.g. In a type-case, this is commonly "ty." Others perhaps "e".
                 [#,fold-target ty])
             ;; then generate the fold
             #,(quasisyntax/loc stx
                 (match #,fold-target
                   match-clauses ...
                   [_ error-msg]))))])))


(define-syntax (make-prim-type stx)
  (define-syntax-class type-name
    #:attributes (name define-id key? (fld-names 1) case printer ht rec-id kw pred? (accessors 1))
    #:transparent
    (pattern [name:id ;; e.g. Type
              define-id:id ;; e.g. def-type
              kw:keyword ;; e.g. #:Type
              case:id ;; e.g. type-case
              hashtable:id ;; e.g. type-name-ht
              rec-id:id ;; e.g. type-rec-id
              (~optional (~and #:key ;; only given for Type.
                               (~bind [key? #'#t]
                                      [(field-names 1) (list #'key)]))
                         #:defaults ([key? #'#f]
                                     [(field-names 1) null]))]
             #:with (_ _ pred? accessors ...)
             (build-struct-names #'name (syntax->list #'(fld-names ...)) #f #t #'name)))
  (syntax-parse stx
    [(_ i:type-name ...)
     #'(begin
         (provide i.define-id ...
                  i.printer ...
                  i.name ...
                  i.pred? ...
                  i.accessors ... ... ;; several accessors per type.
                  (for-syntax i.ht ... i.rec-id ...))
         ;; make type name and populate hashtable with
         ;; keyword to (list match-expander-stx fields fold-rhs.proc #f)
         ;; e.g. def-type type-name-ht #t
         (define-syntax i.define-id (mk #'i.name #'i.ht i.key?)) ...
         (define-for-syntax i.ht (make-hasheq)) ...
         (define-struct/printer (i.name Rep) (i.fld-names ...)
           (lambda (a b c) ((unbox i.printer) a b c))) ...
         (define-for-syntax i.rec-id #'i.rec-id) ...
         (provide i.case ...)
         (define-syntaxes (i.case ...) ;; each fold case gets its own macro.
           (apply values
                    (map (lambda (ht) ;; each type has a hashtable. For each type...
                           (define rec-ids (list i.rec-id ...))
                           ;; make its fold function using populated hashtable.
                           ;; [unsyntax (*1)]
                           (mk-fold ht
                                    ;; binds #'type-rec-id to mk-fold's type-rec-id
                                    (car rec-ids)
                                    ;; binds (list #'type-rec-id
                                    ;;             #'filter-rec-id
                                    ;;             #'object-rec-id
                                    ;;             #'pathelem-rec-id
                                    ;;       ) to rec-ids.
                                    rec-ids
                                    ;; '(#:Type #:Filter #:Object #:PathElem)
                                    '(i.kw ...)))
                         (list i.ht ...)))))]))

(make-prim-type [Type def-type #:Type type-case print-type* type-name-ht type-rec-id #:key]
                [Filter def-filter #:Filter filter-case print-filter* filter-name-ht filter-rec-id]
                [Object def-object #:Object object-case print-object* object-name-ht object-rec-id]
                [PathElem def-pathelem #:PathElem pathelem-case print-pathelem* pathelem-name-ht pathelem-rec-id])

(provide PathElem? (rename-out [Rep-seq Type-seq]
                               [Rep-free-vars free-vars*]
                               [Rep-free-idxs free-idxs*]))

(provide/cond-contract (struct Rep ([seq exact-nonnegative-integer?]
                                    [free-vars (hash/c symbol? variance?)]
                                    [free-idxs (hash/c symbol? variance?)]
                                    [stx (or/c #f syntax?)]))
                       [replace-syntax (Rep? syntax? . -> . Rep?)])

(define (replace-field val new-val idx)
  (define-values (type skipped) (struct-info val))
  (define maker (struct-type-make-constructor type))
  (define flds (struct->list val))
  (apply maker (list-set flds idx new-val)))

(define (replace-syntax rep stx)
  (replace-field rep stx 3))

(define (converter v basic sub)
  (define (gen-constructor sym)
    (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
  (match v
    [(? (lambda (e) (or (Filter? e)
                        (Object? e)
                        (PathElem? e)))
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq fv fi stx vals)))
     `(,(gen-constructor tag) ,@(map sub vals))]
    [(? Type?
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq fv fi stx key vals)))
     `(,(gen-constructor tag) ,@(map sub vals))]
    [_ (basic v)]))

(current-print-convert-hook converter)
