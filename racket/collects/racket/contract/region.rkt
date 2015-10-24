#lang racket/base

(provide define-struct/contract
         define/contract
         with-contract
         current-contract-region
         invariant-assertion)

(require (for-syntax racket/base
                     racket/struct-info
                     syntax/define
                     syntax/kerncase
                     syntax/parse
                     racket/syntax
                     (only-in racket/list partition)
                     (prefix-in a: "private/helpers.rkt"))
         racket/splicing
         racket/stxparam
         syntax/location
         "private/arrow.rkt"
         "private/base.rkt"
         "private/guts.rkt"
         "private/misc.rkt")

;; These are useful for all below.

(define-syntax (verify-contract stx)
  (syntax-case stx ()
    [(_ name x) (a:known-good-contract? #'x) #'x]
    [(_ name x) #'(coerce-contract name x)]))



;                                                                                                    
;                                                                                                    
;                                                                                                    
;       ;           ;;; ;                     ;                                                      
;       ;          ;                          ;                                                      
;       ;          ;                         ;                           ;                       ;   
;    ;; ;    ;;;  ;;;;  ;   ; ;;     ;;;     ;     ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;  ;;   ;   ;  ;    ;   ;;  ;   ;   ;    ;    ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;    ;  ;    ;  ;    ;   ;   ;  ;    ;    ;   ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;    ;  ;;;;;;  ;    ;   ;   ;  ;;;;;;   ;    ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;    ;  ;       ;    ;   ;   ;  ;        ;    ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;  ;;   ;      ;    ;   ;   ;   ;       ;     ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;; ;    ;;;;  ;    ;   ;   ;    ;;;;  ;       ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                                          ;                                                         
;                                          ;                                                         
;                                                                                                    

(define-syntax (define/contract stx)
  (define s-l-c (syntax-local-context))
  (case s-l-c
    [(module-begin)
     #`(begin #,stx)]
    [else
     (define/contract-expander stx)]))

;; (define/contract id contract expr)
;; defines `id' with `contract'; initially binding
;; it to the result of `expr'.  These variables may not be set!'d.
(define-for-syntax (define/contract-expander define-stx)
  (define-splicing-syntax-class fv-clause
    #:description "a free variable clause"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq #:freevars ([var:id ctc:expr] ...))]
    [pattern (~seq #:freevar v:id c:expr)
             #:with (var ...) (list #'v)
             #:with (ctc ...) (list #'c)])
  (define-splicing-syntax-class fvs
    #:description "a sequence of free variable clauses"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq f:fv-clause ...)
             #:with (var ...) #'(f.var ... ...)
             #:with (ctc ...) #'(f.ctc ... ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
             (format "duplicate imported name ~a" 
                     (syntax-e (check-duplicate-identifier (syntax->list #'(var ...)))))])
  (when (memq (syntax-local-context) '(expression module-begin))
    (raise-syntax-error 'define/contract
                        "not used in definition context"
                        define-stx))
  (syntax-parse define-stx
    [(_ name:id contract fv:fvs body)
     (syntax/loc define-stx
       (with-contract #:region definition name
         ([name contract])
         #:freevars ([fv.var fv.ctc] ...)
         (define name body)))]
    [(_ name:id contract fv:fvs body0 body ...)
     (raise-syntax-error 'define/contract
                         "multiple expressions after identifier and contract"
                         #'(body ...))]
    [(_ name+arg-list contract fv:fvs body0 body ...)
     (let-values ([(name body-expr)
                   (normalize-definition
                    (datum->syntax #'define-stx (list* 'define/contract #'name+arg-list
                                                       #'body0 #'(body ...)))
                    #'lambda #t #t)])
       (with-syntax ([name name]
                     [body-expr body-expr])
         (syntax/loc define-stx
           (with-contract #:region function name
             ([name contract])
             #:freevars ([fv.var fv.ctc] ...)
             (define name body-expr)))))]))

(begin-for-syntax
  (define-struct contract-struct-info (si cons)
    #:omit-define-syntaxes
    #:property prop:struct-info (λ (s) ((contract-struct-info-si s)))
    #:property prop:procedure (λ (s stx)
                                (with-syntax ([orig ((contract-struct-info-cons s))])
                                  (syntax-case stx ()
                                    [(_ arg ...)
                                     (datum->syntax stx
                                                    (syntax-e (syntax (orig arg ...)))
                                                    stx
                                                    stx)]
                                    [_ #'orig])))))

(define-syntax (define-struct/contract stx)
  (define-struct field-info (stx ctc [mutable? #:mutable] auto?))
  (define-struct s-info (auto-value-stx transparent? def-stxs? def-vals?))

  (define syntax-error
    (lambda v
      (apply raise-syntax-error 'define-struct/contract v)))
  
  (define (build-struct-names name field-infos)
    (let ([name-str (symbol->string (syntax-case name ()
                                      [id (identifier? #'id)
                                          (syntax-e #'id)]
                                      [(sub super) 
                                       (syntax-e #'sub)]))])
      (list* 
       (syntax-case name ()
         [id (identifier? #'id) #'id]
         [(sub super) #'sub])
       (syntax-case name ()
         [id (identifier? #'id) #'#f]
         [(sub super) #'super])
       (datum->syntax 
        name
        (string->symbol
         (string-append "struct:" name-str)))
       (datum->syntax 
        name
        (string->symbol
         (string-append "make-" name-str)))
       (datum->syntax 
        name
        (string->symbol
         (string-append name-str "?")))           
       (apply append
              (for/list ([finfo field-infos])
                (let ([field-str (symbol->string (syntax-e (field-info-stx finfo)))])
                  (cons (datum->syntax 
                         name
                         (string->symbol
                          (string-append name-str "-" field-str)))
                        (if (field-info-mutable? finfo)
                            (list (datum->syntax 
                                   name
                                   (string->symbol
                                    (string-append "set-" name-str "-" field-str "!"))))
                            null))))))))
  
  (define (build-contracts stx pred field-infos)
    (list* (syntax/loc stx any/c)
           (syntax/loc stx any/c)
           (apply append
                  (for/list ([finfo field-infos])
                    (let ([field-ctc (field-info-ctc finfo)])
                      (cons (quasisyntax/loc stx
                              (-> #,pred #,field-ctc))
                            (if (field-info-mutable? finfo)
                                (list 
                                 (quasisyntax/loc stx
                                   (-> #,pred #,field-ctc void?)))
                                null)))))))
  
  (define (check-field f ctc)
    (let ([p-list (syntax->list f)])
      (if p-list
          (begin 
            (when (null? p-list)
              (syntax-error "expected struct field" f))
            (unless (identifier? (car p-list))
              (syntax-error "expected identifier" f))
            (let loop ([rest (cdr p-list)]
                       [mutable? #f]
                       [auto? #f])
              (if (null? rest) 
                  (make-field-info (car p-list) ctc mutable? auto?)
                  (let ([elem (syntax-e (car rest))])
                    (if (keyword? elem)
                        (cond
                          [(eq? elem '#:mutable)
                           (begin (when mutable?
                                    (syntax-error "redundant #:mutable"
                                                  (car rest)))
                                  (loop (cdr rest) #t auto?))]
                          [(eq? elem '#:auto)
                           (begin (when auto?
                                    (syntax-error "redundant #:mutable"
                                                  (car rest)))
                                  (loop (cdr rest) mutable? #t))]
                          [else (syntax-error "expected #:mutable or #:auto"
                                              (car rest))])
                        (syntax-error "expected #:mutable or #:auto"
                                      (car rest)))))))
          (if (identifier? f)
              (make-field-info f ctc #f #f)
              (syntax-error "expected struct field" f)))))
  (define (check-kwds kwd-list field-infos)
    (let loop ([kwds kwd-list]
               [auto-value-stx #f]
               [mutable? #f]
               [transparent? #f]
               [def-stxs? #t]
               [def-vals? #t])
      (if (null? kwds)
          (make-s-info auto-value-stx transparent? def-stxs? def-vals?)
          (let ([kwd (syntax-e (car kwds))])
            (when (not (keyword? kwd))
              (syntax-error "expected a keyword"
                            (car kwds)))
            (cond
              [(eq? kwd '#:auto-value)
               (when (null? (cdr kwds))
                 (syntax-error "expected a following expression"
                               (car kwds)))
               (loop (cddr kwds) (cadr kwds)
                     transparent? mutable? def-stxs? def-vals?)]
              ;; let arbitrary properties through
              [(eq? kwd '#:property)
               (when (null? (cdr kwds))
                 (syntax-error "expected a property"
                               (car kwds)))
               (when (null? (cddr kwds))
                 (syntax-error "expected a value for the property"
                               (car kwds)))
               (loop (cdddr kwds) auto-value-stx
                     mutable? transparent? def-stxs? def-vals?)]
              [(eq? kwd '#:mutable)
               (when mutable?
                 (syntax-error "redundant #:mutable"
                               (car kwds)))
               (for ([finfo field-infos])
                 (set-field-info-mutable?! finfo #t))
               (loop (cdr kwds) auto-value-stx
                     #t transparent? def-stxs? def-vals?)]
              [(eq? kwd '#:transparent)
               (when transparent?
                 (syntax-error "redundant #:transparent"
                               (car kwds)))
               (loop (cdr kwds) auto-value-stx
                     mutable? #t def-stxs? def-vals?)]
              [(eq? kwd '#:omit-define-syntaxes)
               (when (not def-stxs?)
                 (syntax-error "redundant #:omit-define-syntaxes"
                               (car kwds)))
               (loop (cdr kwds) auto-value-stx
                     transparent? mutable? #f def-vals?)]
              [(eq? kwd '#:omit-define-values)
               (when (not def-vals?)
                 (syntax-error "redundant #:omit-define-values"
                               (car kwds)))
               (loop (cdr kwds) auto-value-stx
                     transparent? mutable? def-stxs? #f)]
              [else (syntax-error "unexpected keyword"
                                  (car kwds))])))))
  (syntax-case stx ()
    [(_ name ([field ctc] ...) kwds ...)
     (let ([fields (syntax->list #'(field ...))])
       (unless (or (identifier? #'name)
                   (syntax-case #'name ()
                     [(x y) (and (identifier? #'x)
                                 (identifier? #'y))]
                     [_ #f]))
         (syntax-error "expected identifier for struct name or a sub-type relationship (subtype supertype)"
                       #'name))
       (let* ([field-infos (map check-field fields (syntax->list #'(ctc ...)))]
              [sinfo (check-kwds (syntax->list #'(kwds ...)) field-infos)]
              [names (build-struct-names #'name field-infos)]
              [pred (car (cddddr names))]
              [ctcs (build-contracts stx pred field-infos)]
              [super-refs (let ([super (cadr names)])
                            (if (identifier? super)
                                (let ([v (syntax-local-value super (lambda () #f))])
                                  (unless (struct-info? v)
                                    (raise-syntax-error #f "identifier is not bound to a structure type"
                                                        stx super))
                                  (let ([v (extract-struct-info v)])
                                    (cadddr v)))
                                null))]
              [super-muts (let ([super (cadr names)])
                            (if (identifier? super)
                                (let ([v (syntax-local-value super (lambda () #f))])
                                  (unless (struct-info? v)
                                    (raise-syntax-error #f "identifier is not bound to a structure type"
                                                        stx super))
                                  (let ([v (extract-struct-info v)])
                                    (car (cddddr v))))
                                null))])
         (let-values ([(non-auto-fields auto-fields)
                       (let loop ([fields field-infos]
                                  [nautos null]
                                  [autos null])
                         (if (null? fields)
                             (values (reverse nautos)
                                     (reverse autos))
                             (if (field-info-auto? (car fields))
                                 (loop (cdr fields)
                                       nautos
                                       (cons (car fields) autos))
                                 (if (null? autos)
                                     (loop (cdr fields)
                                           (cons (car fields) nautos)
                                           autos)
                                     (syntax-error "non-auto field after auto fields"
                                                   (field-info-stx (car fields)))))))])
           (with-syntax ([ctc-bindings
                          (if (s-info-def-vals? sinfo)
                              (map list (cdddr names)
                                   ctcs)
                              null)]
                         [orig stx]
                         [struct-name (car names)]
                         [(auto-check ...)
                          (let* ([av-stx (if (s-info-auto-value-stx sinfo)
                                             (s-info-auto-value-stx sinfo)
                                             #'#f)]
                                 [av-id (datum->syntax av-stx
                                                       (string->symbol
                                                        (string-append (symbol->string (syntax-e (car names)))
                                                                       ":auto-value"))
                                                       av-stx)])
                            (for/list ([finfo auto-fields])
                              #`(let ([#,av-id #,av-stx])
                                  (contract #,(field-info-ctc finfo)
                                            #,av-id
                                            '(struct #,(car names))
                                            'cant-happen
                                            (quote #,av-id)
                                            (quote-srcloc #,av-id)))))]
                         ;; a list of variables, one for each super field
                         [(super-field ...) (generate-temporaries super-refs)]
                         ;; the contract for a super field is any/c because the
                         ;; super constructor will have its own contract
                         [(super-contract ...) (for/list ([i (in-list super-refs)])
                                                 (datum->syntax stx 'any/c))]
                         [(non-auto-contracts ...)
                          (map field-info-ctc
                               (filter (lambda (f)
                                         (not (field-info-auto? f)))
                                       field-infos))]
                         [(struct: maker pred (ref ...) (mut ...) super)
                          (let-values ([(refs muts)
                                        (let loop ([names (cdr (cddddr names))]
                                                   [infos field-infos]
                                                   [refs null]
                                                   [muts null])
                                          (cond
                                            [(null? names)
                                             ;; Don't reverse
                                             (values refs muts)]
                                            [(field-info-mutable? (car infos))
                                             (loop (cddr names)
                                                   (cdr infos)
                                                   (cons (car names) refs)
                                                   (cons (cadr names) muts))]
                                            [else
                                             (loop (cdr names)
                                                   (cdr infos)
                                                   (cons (car names) refs)
                                                   (cons #f muts))]))])
                            (list (caddr names)
                                  (cadddr names)
                                  (car (cddddr names))
                                  refs
                                  muts
                                  (cadr names)))]
                         [(non-auto-name ...)
                          (map field-info-stx non-auto-fields)])
             (with-syntax ([(stx-def ...)
                            (let ([quoter
                                   (λ (s)
                                     (if (identifier? s)
                                         #`(quote-syntax #,s)
                                         #'#f))])
                              (cond
                                [(not (s-info-def-stxs? sinfo))
                                 null]
                                [(s-info-def-vals? sinfo)
                                 (list
                                  #`(define-syntax struct-name
                                      (make-contract-struct-info
                                       (λ ()
                                         (list #,(quoter #'struct:)
                                               #,(quoter #'maker)
                                               #,(quoter #'pred)
                                               (list* #,@(map quoter (syntax->list #'(ref ...)))
                                                      (list #,@(map quoter super-refs)))
                                               (list* #,@(map quoter (syntax->list #'(mut ...)))
                                                      (list #,@(map quoter super-muts)))
                                               #,(quoter #'super)))
                                       (λ () #,(quoter #'maker)))))]
                                [else
                                 (list
                                  #'(define-syntax struct-name
                                      (make-struct-info
                                       (λ ()
                                         (list #f #f #f 
                                               (list #f) (list #f)
                                               #,(quoter #'super))))))]))]
                           [(omit-stx-def ...)
                            (if (s-info-def-stxs? sinfo)
                                (list '#:omit-define-syntaxes)
                                null)])
                 
                 (syntax/loc stx
                   (begin
                     (define-values () (begin auto-check ... (values)))
                     stx-def ...
                     (define (guard super-field ... non-auto-name ... struct-name)
                       (values super-field ... non-auto-name ...))
                     (define blame-id
                       (current-contract-region))
                     (with-contract #:region struct struct-name
                       ctc-bindings
                       (define-struct/derived orig name (field ...)
                         omit-stx-def ...
                         kwds ...
                         #:guard (contract (-> super-contract ... non-auto-contracts ... symbol? any)
                                           guard
                                           (current-contract-region) blame-id
                                           (quote maker)
                                           (quote-srcloc maker)))))))))))]
    [(_ name . bad-fields)
     (identifier? #'name)
     (syntax-error "expected a list of field name/contract pairs"
                   #'bad-fields)]
    [(_ . body)
     (syntax-error "expected a structure name"
                   #'body)]))

;                                                                                         
;                                                                                         
;              ;       ;                                                                  
;                  ;   ;                                     ;                         ;  
;                  ;   ;                                     ;                         ;  
;  ;    ;    ; ;  ;;;; ; ;;;          ;;;     ;;;    ; ;;;  ;;;; ; ;;   ;;;;     ;;;  ;;;;
;  ;    ;    ; ;   ;   ;;   ;        ;   ;   ;   ;   ;;   ;  ;   ;;    ;    ;   ;   ;  ;  
;   ;  ; ;  ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;    ;     ;  ;       ;  
;   ;  ; ;  ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;        ;;;  ;       ;  
;   ; ;   ; ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;     ;;;  ;  ;       ;  
;   ; ;   ; ;  ;   ;   ;    ; ;;;;  ;       ;     ;  ;    ;  ;   ;    ;     ;  ;       ;  
;   ; ;   ; ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;    ;     ;  ;       ;  
;    ;     ;   ;   ;   ;    ;        ;   ;   ;   ;   ;    ;  ;   ;    ;    ;;   ;   ;  ;  
;    ;     ;   ;   ;;; ;    ;         ;;;     ;;;    ;    ;  ;;; ;     ;;;; ;    ;;;   ;;;
;                                                                                         
;                                                                                         
;                                                                                         

(define-for-syntax (make-contracted-id-transformer id contract-stx pos-blame-stx neg-blame-stx)
  (with-syntax ([ctc contract-stx]
                [id id]
                [pos pos-blame-stx]
                [neg neg-blame-stx])
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! i arg)
          (syntax/loc stx
            (set! id (contract ctc arg neg pos (quote id) (quote-srcloc id))))]
         [(f . args)
          (with-syntax ([app (datum->syntax stx '#%app)])
            (syntax/loc stx
              (app (contract ctc id pos neg (quote id) (quote-srcloc id)) . args)))]
         [ident
          (identifier? (syntax ident))
          (syntax/loc stx
            (contract ctc id pos neg (quote id) (quote-srcloc id)))])))))

#|
  We've added separate external and internal contracted identifier transformers for the definition version of
  with-contract.  The new transformers only add new contract applications on internal or external mutation,
  instead of the old version where there was contract application on external access or mutation.  We do this
  by keeping two secret identifiers, one for external uses and one for internal uses, in sync, with appropriate
  contract application on changes.

  Also make sure to set up the initial value for the external id as soon as possible (after the corresponding
  definition of the internal id within with-contract-helper), just because I want to reduce the amount of time
  the two are possibly out of sync.
|#

(define-for-syntax (make-external-contracted-id-transformer int-id ext-id contract-stx pos-blame-stx neg-blame-stx)
  (with-syntax ([ctc contract-stx]
                [int-id int-id]
                [ext-id ext-id]
                [pos pos-blame-stx]
                [neg neg-blame-stx])
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! i arg)
          (syntax/loc stx
            (begin
              (set! int-id (contract ctc arg neg pos (quote int-id) (quote-srcloc int-id)))
              (set! ext-id (contract ctc int-id pos neg (quote ext-id) (quote-srcloc ext-id)))))]
         [(f . args)
          (with-syntax ([app (datum->syntax stx '#%app)])
            (syntax/loc stx (app ext-id . args)))]
         [ident
          (identifier? (syntax ident))
          (syntax/loc stx ext-id)])))))

(define-for-syntax (make-internal-contracted-id-transformer int-id ext-id contract-stx pos-blame-stx neg-blame-stx)
  (with-syntax ([ctc contract-stx]
                [int-id int-id]
                [ext-id ext-id]
                [pos pos-blame-stx]
                [neg neg-blame-stx])
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! i arg)
          (syntax/loc stx
            (begin
              (set! int-id arg)
              (set! ext-id (contract ctc int-id pos neg (quote ext-id) (quote-srcloc ext-id)))))]
         [(f . args)
          (with-syntax ([app (datum->syntax stx '#%app)])
            (syntax/loc stx (app int-id . args)))]
         [ident
          (identifier? (syntax ident))
          (syntax/loc stx int-id)])))))

(define-syntax-rule (add-blame-region blame . body)
  (splicing-syntax-parameterize ([current-contract-region (λ (stx) #'blame)])
    . body))

#|
 with-contract-helper takes syntax of the form:

 (with-contract-helper ((p b e e-expr c c-expr) ...) m-id um-id blame . body)

 where
 p = internal id (transformer binding)
 b = bare (internal) id
 e = bare (external) id
 e-expr = initialization value for bare external id
 c = contract id
 c-expr = initialization value for contract id
 blame = blame syntax for the contract region that's being defined
 body = the body expressions of the with-contract form

 Every time a contracted value is defined (that is, a define
 that defines a value with identifier p), we change it to
 define b instead.  We then define the contract, then the
 external identifier, since defining the external identifier
 requires the contract.  We set up all the transformer bindings
 before calling with-contract-helper, so we don't need definitions
 for p (or marked-p, in the main with-contract macro).

  For identifiers not among the `p`s, use `m-id` and `um-id` to
  remove a mark.
|#   
(define-syntax (with-contract-helper stx)
  (syntax-case stx ()
    [(_ () blame m-id um-id)
     #'(begin)]
    [(_ ((p0 . rest0) (p . rest) ...) m-id um-id blame)
     (raise-syntax-error 'with-contract
                         "no definition found for identifier"
                         #'p0)]
    [(_ id-info blame m-id um-id body0 body ...)
     (let ([expanded-body0 (local-expand #'body0
                                         (syntax-local-context)
                                         (cons #'define (kernel-form-identifier-list)))])
       (define (split-ids to-filter to-match)
         (partition (λ (pair1)
                      (memf (λ (pair2) (bound-identifier=? (car pair1) pair2)) to-match))
                    to-filter))
       (define (recreate-ids ids id-pairs)
         (for/list ([id (in-list ids)])
           (let ([id-pair (findf (λ (p) (bound-identifier=? id (car p))) id-pairs)])
             (if id-pair
                 (cadr id-pair)
                 (unmark id)))))
       (define unmark
         (let ([f (make-syntax-delta-introducer #'m-id #'um-id)])
           (lambda (stx)
             (f stx 'remove))))
       ;; rewrite-define returns:
       ;; * The unused parts of id-info
       ;; * The definition, possibly rewritten to replace certain identifiers
       ;;   along with any auxillary definitions that should be introduced
       ;;   (contract and external id defs)
       (define (rewrite-define head ids expr)
         (let ([id-pairs (map syntax->list (syntax->list #'id-info))])
            (let-values ([(used-ps unused-ps) (split-ids id-pairs ids)])
              (with-syntax* ([new-ids (recreate-ids ids used-ps)]
                             [((e e-expr c c-expr) ...) (map cddr used-ps)])
                (list unused-ps
                      (quasisyntax/loc expanded-body0
                        (begin (#,head new-ids #,expr)
                               (define-values (c ...) (values c-expr ...))
                               (define-values (e ...) (values e-expr ...)))))))))
       (syntax-case expanded-body0 (begin define define-values define-syntaxes)
         [(begin sub ...)
          (syntax/loc stx
            (with-contract-helper id-info blame m-id um-id sub ... body ...))]
         [(define rest ...)
          (let-values ([(def-id body-stx) (normalize-definition expanded-body0 #'lambda #t #t)])
            (with-syntax ([(unused-ps def) (rewrite-define #'define-values (list def-id) body-stx)])
              (syntax/loc stx
                (begin (add-blame-region blame def) (with-contract-helper unused-ps blame m-id um-id body ...)))))]
         [(define-syntaxes (id ...) expr)
          (let ([ids (syntax->list #'(id ...))])
            (with-syntax ([(unused-ps def) (rewrite-define #'define-syntaxes ids #'expr)])
              (syntax/loc stx
                (begin (add-blame-region blame def) (with-contract-helper unused-ps blame m-id um-id body ...)))))]
         [(define-values (id ...) expr)
          (let ([ids (syntax->list #'(id ...))])
            (with-syntax ([(unused-ps def) (rewrite-define #'define-values ids #'expr)])
              (syntax/loc stx
                (begin (add-blame-region blame def) (with-contract-helper unused-ps blame m-id um-id body ...)))))]
         [else
          (quasisyntax/loc stx
            (begin (add-blame-region blame #,expanded-body0)
                   (with-contract-helper id-info blame m-id um-id body ...)))]))]))

(define-syntax (with-contract stx)
  (define-splicing-syntax-class region-clause
    #:description "contract region type"
    [pattern (~seq #:region region:id)])
  (define-splicing-syntax-class fv-clause
    #:description "a free variable clause"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq #:freevars ([var:id ctc:expr] ...))]
    [pattern (~seq #:freevar v:id c:expr)
             #:with (var ...) (list #'v)
             #:with (ctc ...) (list #'c)])
  (define-splicing-syntax-class fvs
    #:description "a sequence of free variable clauses"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq f:fv-clause ...)
             #:with (var ...) #'(f.var ... ...)
             #:with (ctc ...) #'(f.ctc ... ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
             (format "duplicate imported name ~a" 
                     (syntax-e (check-duplicate-identifier (syntax->list #'(var ...)))))])
  (define-syntax-class export-clause
    #:description "a name/contract pair"
    [pattern (var:id ctc:expr)])
  (define-syntax-class exports-clause
    #:attributes ([var 1] [ctc 1])
    #:description "a sequence of name/contract pairs"
    [pattern (ec:export-clause ...)
             #:with (var ...) #'(ec.var ...)
             #:with (ctc ...) #'(ec.ctc ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
             (format "duplicate exported name ~a" 
                     (syntax-e (check-duplicate-identifier (syntax->list #'(var ...)))))])
  (define-splicing-syntax-class results-clause
    #:attributes ([ctc 1])
    #:description "a results clause"
    [pattern (~seq #:result c:expr)
	     #:with (ctc ...) #'(c)]
    [pattern (~seq #:results (ctc:expr ...))])
  (syntax-parse stx
    [(_ (~optional :region-clause #:defaults ([region #'region])) blame:id rc:results-clause fv:fvs . body)
     (if (not (eq? (syntax-local-context) 'expression))
         (quasisyntax/loc stx (#%expression #,stx))
         (let*-values ([(cid-marker) (make-syntax-introducer)]
                       [(free-vars free-ctcs)
                        (values (syntax->list #'(fv.var ...))
                                (syntax->list #'(fv.ctc ...)))])
           (define add-context (make-syntax-introducer #t))
           (with-syntax ([blame-stx #''(region blame)]
                         [blame-id (generate-temporary)]
                         [(res ...) (generate-temporaries #'(rc.ctc ...))]
                         [(free-var ...) free-vars]
                         [(free-var-id ...) (add-context #`#,free-vars)]
                         [(free-ctc-id ...) (map cid-marker free-vars)]
                         [(free-ctc ...) (map (λ (c v)
                                                (syntax-property c 'inferred-name v))
                                              free-ctcs
                                              free-vars)])
             (with-syntax ([new-stx (add-context #'(syntax-parameterize 
                                                    ([current-contract-region (λ (stx) #'blame-stx)])
                                                    (let-values ([(res ...) (let () . body)])
                                                      (values (contract (verify-contract 'with-contract rc.ctc)
                                                                        res
                                                                        blame-stx
                                                                        blame-id) ...))))])
               (syntax/loc stx
                 (let ()
                   (define-values (free-ctc-id ...)
                     (values (verify-contract 'with-contract free-ctc) ...))
                   (define blame-id
                     (current-contract-region))
                   (define-values ()
                     (begin (contract free-ctc-id
                                      free-var
                                      blame-id
                                      'cant-happen
                                      (quote free-var)
                                      (quote-srcloc free-var))
                            ...
                            (values)))
                   (define-syntaxes (free-var-id ...)
                     (values (make-contracted-id-transformer
                              (quote-syntax free-var)
                              (quote-syntax free-ctc-id)
                              (quote-syntax blame-id)
                              (quote-syntax blame-stx)) ...))
                   new-stx))))))]
    [(_ (~optional :region-clause #:defaults ([region #'region])) blame:id ec:exports-clause fv:fvs . body)
     (when (memq (syntax-local-context) '(expression module-begin))
       (raise-syntax-error 'with-contract
                           "not used in definition context"
                           stx))
     (let*-values ([(cid-marker) (make-syntax-introducer)]
                   [(tid-marker) (make-syntax-introducer)]
                   [(eid-marker) (make-syntax-introducer)]
                   [(free-vars free-ctcs)
                    (values (syntax->list #'(fv.var ...))
                            (syntax->list #'(fv.ctc ...)))]
                   [(protected protections)
                    (values (syntax->list #'(ec.var ...))
                            (syntax->list #'(ec.ctc ...)))])
       (define add-context (make-syntax-introducer #t))
       (with-syntax ([blame-stx #''(region blame)]
                     [blame-id (generate-temporary)]
                     [(free-var ...) free-vars]
                     [(free-var-id ...) (add-context #`#,free-vars)]
                     [(free-ctc-id ...) (map cid-marker free-vars)]
                     [(free-ctc ...) (map (λ (c v)
                                            (syntax-property c 'inferred-name v))
                                          free-ctcs
                                          free-vars)]
                     [(ctc-id ...) (map cid-marker protected)]
                     [(ctc ...) (map (λ (c v)
                                       (syntax-property (add-context c) 'inferred-name v))
                                     protections
                                     protected)]
                     [(p ...) protected]
                     [(true-p ...) (map tid-marker protected)]
                     [(ext-id ...) (map eid-marker protected)]
                     [(marked-p ...) (add-context #`#,protected)]
                     [unmarked-id #'here]
                     [marked-id (add-context #'here)])
         (with-syntax ([new-stx (add-context #'body)])
               (syntax/loc stx
                 (begin
                   (define-values (free-ctc-id ...)
                     (values (verify-contract 'with-contract free-ctc) ...))
                   (define blame-id
                     (current-contract-region))
                   (define-values ()
                     (begin (contract free-ctc-id
                                      free-var
                                      blame-id
                                      'cant-happen
                                      (quote free-var)
                                      (quote-srcloc free-var))
                            ...
                            (values)))
                   (define-syntaxes (free-var-id ...)
                     (values (make-contracted-id-transformer
                              (quote-syntax free-var)
                              (quote-syntax free-ctc-id)
                              (quote-syntax blame-id)
                              (quote-syntax blame-stx)) ...))
                   (define-syntaxes (marked-p ...)
                     (values (make-internal-contracted-id-transformer
                              (quote-syntax true-p)
                              (quote-syntax ext-id)
                              (quote-syntax ctc-id)
                              (quote-syntax blame-stx)
                              (quote-syntax blame-id)) ...))
                   (with-contract-helper ((marked-p
                                           true-p
                                           ext-id
                                           (contract ctc-id true-p blame-stx blame-id (quote ext-id) (quote-srcloc ext-id))
                                           ctc-id
                                           (verify-contract 'with-contract ctc))
                                          ...)
                                         blame-stx
                                         marked-id unmarked-id
                                         .
                                         new-stx)
                   (define-syntaxes (p ...)
                     (values (make-external-contracted-id-transformer
                              (quote-syntax true-p)
                              (quote-syntax ext-id)
                              (quote-syntax ctc-id)
                              (quote-syntax blame-stx)
                              (quote-syntax blame-id)) ...)))))))]))
