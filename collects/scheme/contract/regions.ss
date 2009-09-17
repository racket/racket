#lang scheme/base

(provide define-struct/contract
         define/contract
         with-contract)

(require (for-syntax scheme/base
                     scheme/list
                     scheme/struct-info
                     syntax/define
                     syntax/kerncase
                     (prefix-in a: "private/helpers.ss"))
         scheme/splicing
         "private/arrow.ss"
         "private/base.ss"
         "private/guts.ss")

;; These are useful for all below.

(define-syntax (verify-contract stx)
  (syntax-case stx ()
    [(_ name x) (a:known-good-contract? #'x) #'x]
    [(_ name x) #'(coerce-contract name x)]))

;; id->contract-src-info : identifier -> syntax
;; constructs the last argument to the -contract, given an identifier
(define-for-syntax (id->contract-src-info id)
  #`(list (make-srcloc #,id
                       #,(syntax-line id)
                       #,(syntax-column id)
                       #,(syntax-position id)
                       #,(syntax-span id))
          #,(format "~s" (syntax->datum id))))



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

;; (define/contract id contract expr)
;; defines `id' with `contract'; initially binding
;; it to the result of `expr'.  These variables may not be set!'d.
(define-syntax (define/contract define-stx)
  (when (eq? (syntax-local-context) 'expression)
    (raise-syntax-error 'define/contract
                        "used in expression context"
                        define-stx))
  (syntax-case define-stx ()
    [(_ name)
     (raise-syntax-error 'define/contract
                         "no contract or body"
                         define-stx)]
    [(_ name contract-expr)
     (raise-syntax-error 'define/contract
                         "expected a contract expression and a definition body, but found only one expression"
                         define-stx)]
    [(_ name+arg-list contract #:freevars args . body)
     (identifier? #'args)
     (raise-syntax-error 'define/contract
                         "expected list of identifier/contract pairs"
                         #'args)]
    [(_ name+arg-list contract #:freevars (arg ...) #:freevar x c . body)
     (syntax/loc define-stx
       (define/contract name+arg-list contract #:freevars (arg ... [x c]) . body))]
    [(_ name+arg-list contract #:freevar x c . body)
     (syntax/loc define-stx
       (define/contract name+arg-list contract #:freevars () #:freevar x c . body))]
    [(_ name+arg-list contract #:freevars args body0 body ...)
     (begin
       (when (and (identifier? #'name+arg-list)
                  (not (null? (syntax->list #'(body ...)))))
         (raise-syntax-error 'define/contract
                             "multiple expressions after identifier and contract"
                             #'(body ...)))
       (let-values ([(name body-expr)
                     (if (identifier? #'name+arg-list)
                         (values #'name+arg-list #'body0)
                         (normalize-definition
                          (datum->syntax #'define-stx (list* 'define/contract #'name+arg-list
                                                             #'body0 #'(body ...)))
                          #'lambda #t #t))])
         (with-syntax ([name name]
                       [body-expr body-expr]
                       [type (if (identifier? #'name+arg-list) 'definition 'function)])
           (syntax/loc define-stx
             (with-contract #:type type name
               ([name contract])
               #:freevars args
               (define name body-expr))))))]
    [(_ name+arg-list contract body0 body ...)
     (syntax/loc define-stx
       (define/contract name+arg-list contract #:freevars () body0 body ...))]))

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
              [pred (cadddr names)]
              [ctcs (build-contracts stx pred field-infos)]
              [super-fields (syntax-case #'name ()
                              [(child parent)
                               (let ([v (syntax-local-value #'parent (lambda () #f))])
                                 (unless (struct-info? v)
                                   (raise-syntax-error #f "identifier is not bound to a structure type" stx #'parent))
                                 (let ([v (extract-struct-info v)])
                                   (cadddr v)))]
                              [else '()])])
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
                          (let ([val-bindings (if (s-info-def-vals? sinfo)
                                                  (cons (cadr names)
                                                        (map list (cddr names)
                                                             ctcs))
                                                  null)])
                            (if (s-info-def-stxs? sinfo)
                                (cons (car names) val-bindings)
                                val-bindings))]
                         [orig stx]
                         [struct-name (syntax-case #'name ()
                                        [id (identifier? #'id) #'id]
                                        [(id1 super) #'id1])]
                         [(auto-check ...)
                          (let* ([av-stx (if (s-info-auto-value-stx sinfo)
                                             (s-info-auto-value-stx sinfo)
                                             #'#f)]
                                 [av-id (datum->syntax av-stx
                                                       (string->symbol
                                                        (string-append (syntax-case #'name ()
                                                                         [id (identifier? #'id)
                                                                             (symbol->string (syntax-e #'id))]
                                                                         [(id1 super)
                                                                          (symbol->string (syntax-e #'id1))])
                                                                       ":auto-value"))
                                                       av-stx)])
                            (for/list ([finfo auto-fields])
                              #`(let ([#,av-id #,av-stx])
                                  (contract #,(field-info-ctc finfo)
                                            #,av-id
                                            '(struct name)
                                            'cant-happen
                                            #,(id->contract-src-info av-id)))))]
                         ;; a list of variables, one for each super field
                         [(super-fields ...) (generate-temporaries super-fields)]
                         ;; the contract for a super field is any/c becuase the
                         ;; super constructor will have its own contract
                         [(super-contracts ...) (for/list ([i (in-list super-fields)])
                                                  (datum->syntax stx 'any/c))]
                         [(non-auto-contracts ...)
                          (map field-info-ctc
                               (filter (lambda (f)
                                         (not (field-info-auto? f)))
                                       field-infos))]
                         ;; the make-foo function. this is used to make the contract
                         ;; print the right name in the blame
                         [maker (caddr names)]
                         [(non-auto-name ...)
                          (map field-info-stx non-auto-fields)])
             (syntax/loc stx
               (begin
                 (define-values () (begin auto-check ... (values)))
                 (define (guard super-fields ... non-auto-name ... struct-name)
                   (values super-fields ... non-auto-name ...))
                 (define blame-id
                   (current-contract-region))
                 (with-contract #:type struct struct-name
                                ctc-bindings
                                (define-struct/derived orig name (field ...)
                                  kwds ...
                                  #:guard (contract (-> super-contracts ... non-auto-contracts ... symbol? any)
                                                    guard
                                                    (current-contract-region) blame-id
                                                    #'maker)))))))))]
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

(define-for-syntax (make-contracted-id-transformer id contract-stx pos-blame-id neg-blame-id)
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! id arg)
        (raise-syntax-error 'with-contract
                            "cannot set! a contracted variable"
                            stx
                            (syntax id))]
       [(f arg ...)
        (quasisyntax/loc stx
          ((contract #,contract-stx
                     #,id
                     #,pos-blame-id
                     #,neg-blame-id
                     #,(id->contract-src-info id))
           arg ...))]
       [ident
        (identifier? (syntax ident))
        (quasisyntax/loc stx
          (contract #,contract-stx
                    #,id
                    #,pos-blame-id
                    #,neg-blame-id
                    #,(id->contract-src-info id)))]))))


(define-syntax (with-contract-helper stx)
  (syntax-case stx ()
    [(_ () ())
     (begin #'(define-values () (values)))]
    [(_ (p0 p ...) (u ...))
     (raise-syntax-error 'with-contract
                         "no definition found for identifier"
                         #'p0)]
    [(_ () (u0 u ...))
     (raise-syntax-error 'with-contract
                         "no definition found for identifier"
                         #'u0)]
    [(_ (p ...) (u ...) body0 body ...)
     (let ([expanded-body0 (local-expand #'body0
                                         (syntax-local-context)
                                         (kernel-form-identifier-list))])
       (define (filter-ids to-filter to-remove)
         (filter (λ (i1)
                   (not (memf (λ (i2)
                                (bound-identifier=? i1 i2))
                              to-remove)))
                 to-filter))
       (syntax-case expanded-body0 (begin define-values define-syntaxes)
         [(begin sub ...)
          (syntax/loc stx
            (with-contract-helper (p ...) (u ...) sub ... body ...))]
         [(define-syntaxes (id ...) expr)
          (let ([ids (syntax->list #'(id ...))])
            (for ([i1 (syntax->list #'(p ...))])
                 (when (ormap (λ (i2)
                                (bound-identifier=? i1 i2))
                              ids)
                   (raise-syntax-error 'with-contract
                                       "cannot export syntax with a contract"
                                       i1)))
            (with-syntax ([def expanded-body0]
                          [unused-us (filter-ids (syntax->list #'(u ...)) ids)])
              (with-syntax ()
                (syntax/loc stx
                  (begin def (with-contract-helper (p ...) unused-us body ...))))))]
         [(define-values (id ...) expr)
          (let ([ids (syntax->list #'(id ...))])
            (with-syntax ([def expanded-body0]
                          [unused-ps (filter-ids (syntax->list #'(p ...)) ids)]
                          [unused-us (filter-ids (syntax->list #'(u ...)) ids)])
              (syntax/loc stx
                (begin def (with-contract-helper unused-ps unused-us body ...)))))]
         [else
          (quasisyntax/loc stx
            (begin #,expanded-body0
                   (with-contract-helper (p ...) (u ...) body ...)))]))]))

(define-for-syntax (check-and-split-with-contracts single-allowed? args)
  (let loop ([args args]
             [unprotected null]
             [protected null]
             [protections null])
    (cond
      [(null? args)
       (values unprotected protected protections)]
      [(identifier? (car args))
       (unless single-allowed?
         (raise-syntax-error 'with-contract
                           "expected (identifier contract)"
                           (car args)))
       (loop (cdr args)
             (cons (car args) unprotected)
             protected
             protections)]
      [(let ([lst (syntax->list (car args))])
         (and (list? lst)
              (= (length lst) 2)
              (identifier? (first lst))
              lst))
       =>
       (lambda (l)
         (loop (cdr args)
               unprotected
               (cons (first l) protected)
               (cons (second l) protections)))]
      [else
       (raise-syntax-error 'with-contract
                           (format "expected ~a(identifier contract)"
                                   (if single-allowed? "an identifier or " ""))
                           (car args))])))

(define-syntax (with-contract stx)
  (when (eq? (syntax-local-context) 'expression)
    (raise-syntax-error 'with-contract
                        "used in expression context"
                        stx))
  (syntax-case stx ()
    [(_ #:type type etc ...)
     (not (identifier? #'type))
     (raise-syntax-error 'with-contract
                         "expected identifier for type"
                         #'type)]
    [(_ #:type type args etc ...)
     (not (identifier? #'args))
     (raise-syntax-error 'with-contract
                         "expected identifier for blame"
                         #'args)]
    [(_ #:type type blame (arg ...) #:freevars (fv ...) #:freevar x c . body)
     (identifier? #'x)
     (syntax/loc stx
       (with-contract #:type type blame (arg ...) #:freevars (fv ... [x c]) . body))]
    [(_ #:type type blame (arg ...) #:freevars (fv ...) #:freevar x c . body)
     (raise-syntax-error 'with-contract
                         "use of #:freevar with non-identifier"
                         #'x)]
    [(_ #:type type blame (arg ...) #:freevars (fv ...) . body)
     (and (identifier? #'blame)
          (identifier? #'type))
     (let*-values ([(marker) (make-syntax-introducer)]
                   [(cid-marker) (make-syntax-introducer)]
                   [(no-need free-vars free-ctcs)
                    (check-and-split-with-contracts #f (syntax->list #'(fv ...)))]
                   [(unprotected protected protections)
                    (check-and-split-with-contracts #t (syntax->list #'(arg ...)))])
       (begin
         (let ([dupd-id (check-duplicate-identifier (append unprotected protected))])
           (when dupd-id
             (raise-syntax-error 'with-contract
                                 "identifier appears twice in exports"
                                 dupd-id)))
         (with-syntax ([blame-stx #''(type blame)]
                       [blame-id (car (generate-temporaries (list #t)))]
                       [(free-var ...) free-vars]
                       [(free-var-id ...) (map marker free-vars)]
                       [(free-ctc-id ...) (map cid-marker free-vars)]
                       [(free-ctc ...) (map (λ (c v)
                                              (syntax-property c 'inferred-name v))
                                            free-ctcs
                                            free-vars)]
                       [(free-src-info ...) (map id->contract-src-info free-vars)]
                       [(ctc-id ...) (map cid-marker protected)]
                       [(ctc ...) (map (λ (c v)
                                         (marker (syntax-property c 'inferred-name v)))
                                       protections
                                       protected)]
                       [(p ...) protected]
                       [(marked-p ...) (map marker protected)]
                       [(src-info ...) (map (compose id->contract-src-info marker) protected)]
                       [(u ...) unprotected]
                       [(marked-u ...) (map marker unprotected)])
           (quasisyntax/loc stx
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
                                  free-src-info)
                        ...
                        (values)))
               (define-syntaxes (free-var-id ...)
                 (values (make-contracted-id-transformer
                          (quote-syntax free-var)
                          (quote-syntax free-ctc-id)
                          (quote-syntax blame-id)
                          (quote-syntax blame-stx)) ...))
               (splicing-syntax-parameterize ([current-contract-region (λ (stx) #'blame-stx)])
                 (with-contract-helper (marked-p ...) (marked-u ...) . #,(marker #'body)))
               (define-values (ctc-id ...)
                 (values (verify-contract 'with-contract ctc) ...))
               (define-values ()
                 (begin (contract ctc-id
                                  marked-p
                                  blame-stx
                                  'cant-happen
                                  src-info)
                        ...
                        (values)))
               (define-syntaxes (u ... p ...)
                 (values (make-rename-transformer #'marked-u) ...
                         (make-contracted-id-transformer
                          (quote-syntax marked-p)
                          (quote-syntax ctc-id)
                          (quote-syntax blame-stx)
                          (quote-syntax blame-id)) ...)))))))]
    [(_ #:type type blame (arg ...) #:freevar x c . body)
     (syntax/loc stx
       (with-contract #:type type blame (arg ...) #:freevars ([x c]) . body))]
    [(_ #:type type blame (arg ...) . body)
     (syntax/loc stx
       (with-contract #:type type blame (arg ...) #:freevars () . body))]
    [(_ #:type type blame bad-args etc ...)
     (raise-syntax-error 'with-contract
                         "expected list of identifier and/or (identifier contract)"
                         #'bad-args)]
    [(_ #:type type blame)
     (raise-syntax-error 'with-contract
                         "only blame"
                         stx)]
    [(_ etc ...)
     (syntax/loc stx
       (with-contract #:type region etc ...))]))
