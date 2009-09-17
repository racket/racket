#lang scheme/base

#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#



(provide (rename-out [-contract contract])
         recursive-contract
         provide/contract
         define-struct/contract
         define/contract
         with-contract
         current-contract-region
         new-∃/c
         ∃?)

(require (for-syntax scheme/base)
         (for-syntax "contract-opt-guts.ss")
         (for-syntax scheme/struct-info)
         (for-syntax scheme/list)
         (for-syntax syntax/define)
         (for-syntax syntax/kerncase)
         scheme/promise
         scheme/stxparam
         scheme/splicing
         mzlib/etc)

(require "contract-arrow.ss"
         "contract-guts.ss"
         "contract-opt.ss")

(require "contract-helpers.ss"
         (for-syntax (prefix-in a: "contract-helpers.ss")))


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
                                  (-contract #,(field-info-ctc finfo)
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
                                  #:guard (-contract (-> super-contracts ... non-auto-contracts ... symbol? any)
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

(define-syntax-parameter current-contract-region (λ (stx) #'(#%variable-reference)))

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
          ((-contract #,contract-stx
                      #,id
                      #,pos-blame-id
                      #,neg-blame-id
                      #,(id->contract-src-info id))
           arg ...))]
       [ident
        (identifier? (syntax ident))
        (quasisyntax/loc stx
          (-contract #,contract-stx
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
                 (begin (-contract free-ctc-id
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
                 (begin (-contract ctc-id
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

;                                                                                                            
;                                                                                                            
;                                                                                                            
;                               ;       ;             ;                                                      
;                                       ;             ;                                                      
;                                       ;            ;                           ;                       ;   
;   ; ;;    ; ;   ;;;   ;     ; ;    ;; ;    ;;;     ;     ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;;  ;   ;;   ;   ;   ;   ;  ;   ;  ;;   ;   ;    ;    ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;   ;    ;  ;   ;     ;  ;   ;  ;  ;    ;  ;    ;    ;   ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;   ;    ;  ;   ;     ;   ; ;   ;  ;    ;  ;;;;;;   ;    ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;   ;    ;  ;   ;     ;   ; ;   ;  ;    ;  ;        ;    ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;;  ;   ;    ;   ;     ;    ;   ;  ;;   ;       ;     ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;   ; ;;    ;     ;;;      ;    ;    ;; ;    ;;;;  ;       ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;   ;                                              ;                                                         
;   ;                                              ;                                                         
;   ;                                                                                                        


;; lookup-struct-info : syntax -> (union #f (list syntax syntax (listof syntax) ...))
(define-for-syntax (lookup-struct-info stx provide-stx)
  (let ([id (syntax-case stx ()
              [(a b) (syntax a)]
              [_ stx])])
    (let ([v (syntax-local-value id (λ () #f))])
      (if (struct-info? v)
          (extract-struct-info v)
          (raise-syntax-error 'provide/contract
                              "expected a struct name" 
                              provide-stx
                              id)))))

(define-for-syntax (make-provide/contract-transformer contract-id id pos-module-source)
  (make-set!-transformer
   (let ([saved-id-table (make-hasheq)])
     (λ (stx)
       (if (eq? 'expression (syntax-local-context))
           ;; In an expression context:
           (let ([key (syntax-local-lift-context)])
             ;; Already lifted in this lifting context?
             (let ([lifted-id
                    (or (hash-ref saved-id-table key #f)
                        ;; No: lift the contract creation:
                        (with-syntax ([contract-id contract-id]
                                      [id id]
                                      [pos-module-source pos-module-source]
                                      [id-ref (syntax-case stx (set!)
                                                [(set! whatever e)
                                                 id] ;; just avoid an error here, signal the error later
                                                [(id . x)
                                                 #'id]
                                                [id
                                                 (identifier? #'id)
                                                 #'id])])
                          (syntax-local-introduce
                           (syntax-local-lift-expression
                            #`(-contract contract-id
                                         id
                                         pos-module-source
                                         (#%variable-reference)
                                         #,(id->contract-src-info #'id))))))])
               (when key
                 (hash-set! saved-id-table key lifted-id))
               ;; Expand to a use of the lifted expression:
               (with-syntax ([saved-id (syntax-local-introduce lifted-id)])
                 (syntax-case stx (set!)
                   [name
                    (identifier? (syntax name))
                    (syntax saved-id)]
                   [(set! id arg) 
                    (raise-syntax-error 'provide/contract
                                        "cannot set! a provide/contract variable" 
                                        stx 
                                        (syntax id))]
                   [(name . more)
                    (with-syntax ([app (datum->syntax stx '#%app)])
                      (syntax/loc stx (app saved-id . more)))]))))
           ;; In case of partial expansion for module-level and internal-defn contexts,
           ;; delay expansion until it's a good time to lift expressions:
           (quasisyntax/loc stx (#%expression #,stx)))))))


(define-syntax (provide/contract provide-stx)
  (syntax-case provide-stx (struct)
    [(_ p/c-ele ...)
     (let ()
       
       ;; ids : table[id -o> (listof id)]
       ;; code-for-each-clause adds identifiers to this map.
       ;; when it binds things; they are then used to signal 
       ;; a syntax error for duplicates
       (define dups-table (make-hash))
       (define (add-to-dups-table id)
         (hash-update!
          dups-table
          (syntax-e id)
          (λ (ids) (cons id ids))
          '()))
       (define (signal-dup-syntax-error)
         (hash-for-each
          dups-table
          (λ (k ids)
            (let loop ([ids ids])
              (cond
                [(null? ids) (void)]
                [else
                 (cond
                   [(ormap (λ (x) (bound-identifier=? (car ids) x)) (cdr ids))
                    (let ([dups (filter (λ (x) (bound-identifier=? (car ids) x))
                                        ids)])
                      (raise-syntax-error 'provide/contract
                                          "duplicate identifiers"
                                          provide-stx
                                          (car dups)
                                          (cdr dups)))]
                   [else
                    (loop (cdr ids))])])))))
       
       ;; code-for-each-clause : (listof syntax) -> (listof syntax)
       ;; constructs code for each clause of a provide/contract
       (define (code-for-each-clause clauses)
         (let loop ([clauses clauses]
                    [exists-binders '()])
           (cond
           [(null? clauses) null]
           [else
            (let ([clause (car clauses)])
              ;; compare raw identifiers for `struct' and `rename' just like provide does
              (syntax-case* clause (struct rename) (λ (x y) (eq? (syntax-e x) (syntax-e y))) 
                [exists
		 (or (eq? '#:exists (syntax-e #'exists)) (eq? '#:∃ (syntax-e #'exists)))
                 (cond
                   [(null? (cdr clauses))
                    (raise-syntax-error 'provide/conract
                                        (format "expected either a single variable or a sequence of variables to follow ~a, but found nothing"
						(syntax-e #'exists))
                                        provide-stx
                                        clause)]
                   [else
                    (syntax-case (cadr clauses) ()
		      [x
		       (identifier? #'x)
                       (with-syntax ([(x-gen) (generate-temporaries #'(x))])
                         (cons (code-for-one-exists-id #'x #'x-gen)
                               (loop (cddr clauses) 
                                     (add-a-binder #'x #'x-gen exists-binders))))]
                      [(x ...)
                       (andmap identifier? (syntax->list #'(x ...)))
                       (with-syntax ([(x-gen ...) (generate-temporaries #'(x ...))])
                         (append (map code-for-one-exists-id 
                                      (syntax->list #'(x ...))
                                      (syntax->list #'(x-gen ...)))
                                 (loop (cddr clauses)
                                       (let loop ([binders exists-binders]
                                                  [xs (syntax->list #'(x ...))]
                                                  [x-gens (syntax->list #'(x-gen ...))])
                                         (cond
                                           [(null? xs) binders]
                                           [else (loop (add-a-binder (car xs) (car x-gens) binders)
                                                       (cdr xs)
                                                       (cdr x-gens))])))))]
                      [else
                       (raise-syntax-error 'provide/contract
					   (format "expected either a single variable or a sequence of variables to follow ~a"
						   (syntax-e #'exists))
                                           provide-stx
                                           (cadr clauses))])])]
                [(rename this-name new-name contract)
                 (and (identifier? (syntax this-name))
                      (identifier? (syntax new-name)))
                 (begin
                   (add-to-dups-table #'new-name)
                   (cons (code-for-one-id provide-stx 
                                          (syntax this-name) 
                                          (add-exists-binders (syntax contract) exists-binders)
                                          (syntax new-name))
                         (loop (cdr clauses) exists-binders)))]
                [(rename this-name new-name contract)
                 (identifier? (syntax this-name))
                 (raise-syntax-error 'provide/contract 
                                     "malformed rename clause, expected an identifier" 
                                     provide-stx
                                     (syntax new-name))]
                [(rename this-name new-name contract)
                 (identifier? (syntax new-name))
                 (raise-syntax-error 'provide/contract 
                                     "malformed rename clause, expected an identifier" 
                                     provide-stx
                                     (syntax this-name))]
                [(rename . _)
                 (raise-syntax-error 'provide/contract "malformed rename clause" provide-stx clause)]
                [(struct struct-name ((field-name contract) ...))
                 (and (well-formed-struct-name? (syntax struct-name))
                      (andmap identifier? (syntax->list (syntax (field-name ...)))))
                 (let ([sc (build-struct-code provide-stx
                                              (syntax struct-name)
                                              (syntax->list (syntax (field-name ...)))
                                              (map (λ (x) (add-exists-binders x exists-binders))
                                                   (syntax->list (syntax (contract ...)))))])
                   (add-to-dups-table #'struct-name)
                   (cons sc (loop (cdr clauses) exists-binders)))]
                [(struct name)
                 (identifier? (syntax name))
                 (raise-syntax-error 'provide/contract
                                     "missing fields"
                                     provide-stx
                                     clause)]
                [(struct name . rest)
                 (not (well-formed-struct-name? (syntax name)))
                 (raise-syntax-error 'provide/contract "name must be an identifier or two identifiers with parens around them"
                                     provide-stx
                                     (syntax name))]
                [(struct name (fields ...))
                 (for-each (λ (field)
                             (syntax-case field ()
                               [(x y) 
                                (identifier? (syntax x)) 
                                (void)]
                               [(x y) 
                                (raise-syntax-error 'provide/contract
                                                    "malformed struct field, expected identifier"
                                                    provide-stx
                                                    (syntax x))]
                               [else
                                (raise-syntax-error 'provide/contract
                                                    "malformed struct field"
                                                    provide-stx
                                                    field)]))
                           (syntax->list (syntax (fields ...))))
                 
                 ;; if we didn't find a bad field something is wrong!
                 (raise-syntax-error 'provide/contract "internal error" provide-stx clause)]
                [(struct name . fields)
                 (raise-syntax-error 'provide/contract
                                     "malformed struct fields"
                                     provide-stx
                                     clause)]
                [(name contract)
                 (identifier? (syntax name))
                 (begin
                   (add-to-dups-table #'name)
                   (cons (code-for-one-id provide-stx 
                                          (syntax name) 
                                          (add-exists-binders (syntax contract)
                                                              exists-binders)
                                          #f)
                         (loop (cdr clauses) exists-binders)))]
                [(name contract)
                 (raise-syntax-error 'provide/contract
                                     "expected identifier"
                                     provide-stx
                                     (syntax name))]
                [unk
                 (raise-syntax-error 'provide/contract
                                     "malformed clause"
                                     provide-stx
                                     (syntax unk))]))])))
       
       ;; well-formed-struct-name? : syntax -> bool
       (define (well-formed-struct-name? stx)
         (or (identifier? stx)
             (syntax-case stx ()
               [(name super)
                (and (identifier? (syntax name))
                     (identifier? (syntax super)))
                #t]
               [else #f])))
       
       ;; build-struct-code : syntax syntax (listof syntax) (listof syntax) -> syntax
       ;; constructs the code for a struct clause
       ;; first arg is the original syntax object, for source locations
       (define (build-struct-code stx struct-name-position field-names field-contracts)
         (let* ([struct-name (syntax-case struct-name-position ()
                               [(a b) (syntax a)]
                               [else struct-name-position])]
                [super-id (syntax-case struct-name-position ()
                            [(a b) (syntax b)]
                            [else #t])]
                
                
                [all-parent-struct-count/names (get-field-counts/struct-names struct-name provide-stx)]
                [parent-struct-count (if (null? all-parent-struct-count/names)
                                         #f
                                         (let ([pp (cdr all-parent-struct-count/names)])
                                           (if (null? pp)
                                               #f
                                               (car (car pp)))))]
                
                [struct-info (lookup-struct-info struct-name-position provide-stx)]
                [constructor-id (list-ref struct-info 1)]
                [predicate-id (list-ref struct-info 2)]
                [selector-ids (reverse (list-ref struct-info 3))]
                [is-id-ok?
                 (λ (id i)
                   (if (or (not parent-struct-count)
                           (parent-struct-count . <= . i))
                       id
                       #t))]
                [mutator-ids (reverse (list-ref struct-info 4))] ;; (listof (union #f identifier))
                [field-contract-ids (map (λ (field-name field-contract) 
                                           (if (a:known-good-contract? field-contract)
                                               field-contract
                                               (a:mangle-id provide-stx
                                                            "provide/contract-field-contract"
                                                            field-name
                                                            struct-name)))
                                         field-names
                                         field-contracts)]
                [struct:struct-name
                 (datum->syntax
                  struct-name
                  (string->symbol
                   (string-append
                    "struct:"
                    (symbol->string (syntax-e struct-name)))))]
                
                [-struct:struct-name
                 (datum->syntax
                  struct-name
                  (string->symbol
                   (string-append
                    "-struct:"
                    (symbol->string (syntax-e struct-name)))))]
                
                [is-new-id?
                 (λ (index)
                   (or (not parent-struct-count)
                       (parent-struct-count . <= . index)))])
           
           (let ([unknown-info
                  (λ (what names)
                    (raise-syntax-error
                     'provide/contract
                     (format "cannot determine ~a, found ~s" what names)
                     provide-stx
                     struct-name))])
             
             (unless (or (null? selector-ids)
                         (identifier? (last selector-ids)))
               (unknown-info "the selectors" (map syntax->datum selector-ids)))
             
             (unless constructor-id (unknown-info "constructor" constructor-id))
             (unless predicate-id (unknown-info "predicate" predicate-id))
             (unless (andmap/count is-id-ok? selector-ids)
               (unknown-info "selectors"
                             (map (λ (x) (if (syntax? x)
                                             (syntax->datum x)
                                             x))
                                  selector-ids))))
           
           (unless (equal? (length selector-ids)
                           (length field-contract-ids))
             (raise-syntax-error 'provide/contract
                                 (format "found ~a field~a in struct, but ~a contract~a"
                                         (length selector-ids)
                                         (if (= 1 (length selector-ids)) "" "s")
                                         (length field-contract-ids)
                                         (if (= 1 (length field-contract-ids)) "" "s"))
                                 provide-stx
                                 struct-name))
           
           ;; make sure the field names are right.
           (let* ([relative-counts (let loop ([c (map car all-parent-struct-count/names)])
                                     (cond
                                       [(null? c) null]
                                       [(null? (cdr c)) c]
                                       [else (cons (- (car c) (cadr c))
                                                   (loop (cdr c)))]))]
                  [names (map cdr all-parent-struct-count/names)]
                  [maker-name (format "~a" (syntax-e constructor-id))]
                  [struct-name (substring maker-name 5 (string-length maker-name))])
             (let loop ([count (car relative-counts)]
                        [name (car names)]
                        [counts (cdr relative-counts)]
                        [names (cdr names)]
                        [selector-strs (reverse (map (λ (x) (format "~a" (syntax-e x))) selector-ids))]
                        [field-names (reverse field-names)])
               (cond
                 [(or (null? selector-strs) (null? field-names)) 
                  (void)]
                 [(zero? count) 
                  (loop (car counts) (car names) (cdr counts) (cdr names) 
                        selector-strs
                        field-names)]
                 [else
                  (let* ([selector-str (car selector-strs)]
                         [field-name (car field-names)]
                         [field-name-should-be
                          (substring selector-str 
                                     (+ (string-length name) 1)
                                     (string-length selector-str))]
                         [field-name-is (format "~a" (syntax-e field-name))])
                    (unless (equal? field-name-should-be field-name-is)
                      (raise-syntax-error 'provide/contract
                                          (format "expected field name to be ~a, but found ~a"
                                                  field-name-should-be
                                                  field-name-is)
                                          provide-stx
                                          field-name))
                    (loop (- count 1)
                          name
                          counts
                          names
                          (cdr selector-strs)
                          (cdr field-names)))])))
           
           (with-syntax ([((selector-codes selector-new-names) ...)
                          (filter
                           (λ (x) x)
                           (map/count (λ (selector-id field-contract-id index)
                                        (if (is-new-id? index)
                                            (code-for-one-id/new-name
                                             stx
                                             selector-id
                                             (build-selector-contract struct-name 
                                                                      predicate-id
                                                                      field-contract-id)
                                             #f)
                                            #f))
                                      selector-ids
                                      field-contract-ids))]
                         [(rev-selector-old-names ...)
                          (reverse
                           (filter
                            (λ (x) x)
                            (map/count (λ (selector-id index)
                                         (if (not (is-new-id? index))
                                             selector-id
                                             #f))
                                       selector-ids)))]
                         [(mutator-codes/mutator-new-names ...)
                          (map/count (λ (mutator-id field-contract-id index)
                                       (if (and mutator-id (is-new-id? index))
                                           (code-for-one-id/new-name stx
                                                                     mutator-id 
                                                                     (build-mutator-contract struct-name 
                                                                                             predicate-id
                                                                                             field-contract-id)
                                                                     #f)
                                           #f))
                                     mutator-ids
                                     field-contract-ids)]
                         [(predicate-code predicate-new-name)
                          (code-for-one-id/new-name stx predicate-id (syntax (-> any/c boolean?)) #f)]
                         [(constructor-code constructor-new-name)
                          (code-for-one-id/new-name
                           stx
                           constructor-id
                           (build-constructor-contract stx
                                                       field-contract-ids 
                                                       predicate-id)
                           #f
                           #t)]
                         
                         [(field-contract-id-definitions ...)
                          (filter values (map (λ (field-contract-id field-contract)
                                                (if (a:known-good-contract? field-contract)
                                                    #f
                                                    (with-syntax ([field-contract-id field-contract-id]
                                                                  [field-contract field-contract])
                                                      #'(define field-contract-id (verify-contract 'provide/contract field-contract)))))
                                              field-contract-ids
                                              field-contracts))]
                         [(field-contracts ...) field-contracts]
                         [(field-contract-ids ...) field-contract-ids])
             
             (with-syntax ([((mutator-codes mutator-new-names) ...)
                            (filter syntax-e (syntax->list #'(mutator-codes/mutator-new-names ...)))])
               (with-syntax ([(rev-selector-new-names ...) (reverse (syntax->list (syntax (selector-new-names ...))))]
                             [(rev-mutator-new-names ...) (reverse (syntax->list (syntax (mutator-new-names ...))))])
                 (with-syntax ([struct-code 
                                (with-syntax ([id-rename (a:mangle-id provide-stx 
                                                                      "provide/contract-struct-expandsion-info-id"
                                                                      struct-name)]
                                              [struct-name struct-name]
                                              [-struct:struct-name -struct:struct-name]
                                              [super-id (if (boolean? super-id)
                                                            super-id
                                                            (with-syntax ([super-id super-id])
                                                              (syntax ((syntax-local-certifier) #'super-id))))]
                                              [(mutator-id-info ...)
                                               (map (λ (x)
                                                      (syntax-case x ()
                                                        [(a b) #'(slc #'b)]
                                                        [else #f]))
                                                    (syntax->list #'(mutator-codes/mutator-new-names ...)))]
                                              [(exported-selector-ids ...) (reverse selector-ids)]
                                              )
                                  #`(begin
                                      (provide (rename-out [id-rename struct-name]))
                                      (define-syntax id-rename
                                        (let ([slc (syntax-local-certifier)])
                                          #;
                                          (list (slc #'-struct:struct-name)
                                                (slc #'#,constructor-id)
                                                (slc #'#,predicate-id)
                                                (list (slc #'exported-selector-ids) ...)
                                                (list mutator-id-info ...)
                                                super-id)
                                          (list (slc #'-struct:struct-name)
                                                (slc #'constructor-new-name)
                                                (slc #'predicate-new-name)
                                                (list (slc #'rev-selector-new-names) ...
                                                      (slc #'rev-selector-old-names) ...)
                                                (list mutator-id-info ...)
                                                super-id)))))]
                               [struct:struct-name struct:struct-name]
                               [-struct:struct-name -struct:struct-name]
                               [struct-name struct-name]
                               [(selector-ids ...) selector-ids])
                   (syntax/loc stx
                     (begin
                       struct-code
                       field-contract-id-definitions ...
                       selector-codes ...
                       mutator-codes ...
                       predicate-code
                       constructor-code
                       
                       ;; expanding out the body of the `make-pc-struct-type' function
                       ;; directly here in the expansion makes this very expensive at compile time
                       ;; when there are a lot of provide/contract clause using structs
                       (define -struct:struct-name 
                         (make-pc-struct-type 'struct-name struct:struct-name field-contract-ids ...))
                       (provide (rename-out [-struct:struct-name struct:struct-name]))))))))))
       
       (define (map/count f . ls)
         (let loop ([ls ls]
                    [i 0])
           (cond
             [(andmap null? ls) '()]
             [(ormap null? ls) (error 'map/count "mismatched lists")]
             [else (cons (apply f (append (map car ls) (list i)))
                         (loop (map cdr ls)
                               (+ i 1)))])))
       
       ;; andmap/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
       (define (andmap/count f l1)
         (let loop ([l1 l1]
                    [i 0])
           (cond
             [(null? l1) #t]
             [else (and (f (car l1) i)
                        (loop (cdr l1)
                              (+ i 1)))])))
       
       ;; get-field-counts/struct-names : syntax syntax -> (listof (cons symbol number))
       ;; returns a list of numbers corresponding to the numbers of fields for each of the parent structs
       (define (get-field-counts/struct-names struct-name provide-stx)
         (let loop ([parent-info-id struct-name])
           (let ([parent-info 
                  (and (identifier? parent-info-id)
                       (lookup-struct-info parent-info-id provide-stx))])
             (cond
               [(boolean? parent-info) null]
               [else
                (let ([fields (list-ref parent-info 3)]
                      [constructor (list-ref parent-info 1)])
                  (cond
                    [(and (not (null? fields))
                          (not (last fields)))
                     (raise-syntax-error 
                      'provide/contract
                      "cannot determine the number of fields in super struct"
                      provide-stx
                      struct-name)]
                    [else
                     (cons (cons (length fields) (constructor->struct-name provide-stx constructor))
                           (loop (list-ref parent-info 5)))]))]))))
       
       (define (constructor->struct-name orig-stx stx)
         (and stx
              (let ([m (regexp-match #rx"^make-(.*)$" (format "~a" (syntax-e stx)))])
                (cond
                  [m (cadr m)]
                  [else (raise-syntax-error 'contract-base.ss
                                            "unable to cope with a struct maker whose name doesn't begin with `make-'"
                                            orig-stx)]))))
       
       ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
       (define (build-constructor-contract stx field-contract-ids predicate-id)
         (with-syntax ([(field-contract-ids ...) field-contract-ids]
                       [predicate-id predicate-id])
           (syntax/loc stx
             (-> field-contract-ids ...
                 predicate-id))))
       
       ;; build-selector-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-selector-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (-> predicate-id field-contract-id))))
       
       ;; build-mutator-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-mutator-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (-> predicate-id
                       field-contract-id
                       void?))))
       
       ;; code-for-one-exists-id : syntax -> syntax
       (define (code-for-one-exists-id x x-gen)
         #`(define #,x-gen (new-∃/c '#,x)))

       (define (add-exists-binders stx exists-binders)
         #`(let #,exists-binders #,stx))
       
       (define (add-a-binder id id-gen binders)
         (cons #`[#,id #,id-gen] binders))
       
       ;; code-for-one-id : syntax syntax syntax (union syntax #f) -> syntax
       ;; given the syntax for an identifier and a contract,
       ;; builds a begin expression for the entire contract and provide
       ;; the first syntax object is used for source locations
       (define (code-for-one-id stx id ctrct user-rename-id)
         (with-syntax ([(code id) (code-for-one-id/new-name stx id ctrct user-rename-id)])
           (syntax code)))
       
       ;; code-for-one-id/new-name : syntax syntax syntax (union syntax #f) -> (values syntax syntax)
       ;; given the syntax for an identifier and a contract,
       ;; builds a begin expression for the entire contract and provide
       ;; the first syntax object is used for source locations
       (define code-for-one-id/new-name
         (case-lambda
           [(stx id ctrct user-rename-id) 
            (code-for-one-id/new-name stx id ctrct user-rename-id #f)]
           [(stx id ctrct user-rename-id mangle-for-maker?)
            (let ([no-need-to-check-ctrct? (a:known-good-contract? ctrct)])
              (with-syntax ([id-rename ((if mangle-for-maker? 
                                            a:mangle-id-for-maker
                                            a:mangle-id)
                                        provide-stx
                                        "provide/contract-id" 
                                        (or user-rename-id id))]
                            [contract-id (if no-need-to-check-ctrct?
                                             ctrct
                                             (a:mangle-id provide-stx
                                                          "provide/contract-contract-id" 
                                                          (or user-rename-id id)))]
                            [pos-module-source (a:mangle-id provide-stx 
                                                            "provide/contract-pos-module-source"
                                                            (or user-rename-id id))]
                            [pos-stx (datum->syntax id 'here)]
                            [id id]
                            [ctrct (syntax-property ctrct 'inferred-name id)]
                            [external-name (or user-rename-id id)]
                            [where-stx stx])
                (with-syntax ([extra-test 
                               (syntax-case #'ctrct (->)
                                 [(-> dom ... arg)
                                  #`(and (procedure? id)
                                         (procedure-arity-includes? id #,(length (syntax->list #'(dom ...)))))]
                                 [_ #f])])
                (with-syntax ([code
                               (quasisyntax/loc stx
                                 (begin
                                   (define pos-module-source (#%variable-reference))
                                   
                                   #,@(if no-need-to-check-ctrct?
                                          (list)
                                          (list #'(define contract-id 
                                                    (let ([id ctrct]) ;; let is here to give the right name.
                                                      (verify-contract 'provide/contract id)))))
                                   (define-syntax id-rename
                                     (make-provide/contract-transformer (quote-syntax contract-id)
                                                                        (quote-syntax id)
                                                                        (quote-syntax pos-module-source)))
                                   
                                   (provide (rename-out [id-rename external-name]))))])
                  
                  (syntax-local-lift-module-end-declaration
                   #`(begin 
                       (unless extra-test
                         (-contract contract-id id pos-module-source 'ignored #,(id->contract-src-info #'id)))
                       (void)))
                  
                  (syntax (code id-rename))))))]))
       
       (with-syntax ([(bodies ...) (code-for-each-clause (syntax->list (syntax (p/c-ele ...))))])
         (signal-dup-syntax-error)
         (syntax 
          (begin
            bodies ...))))]))

(define (make-pc-struct-type struct-name struct:struct-name . ctcs)
  (let-values ([(struct:struct-name _make _pred _get _set)
                (make-struct-type struct-name
                                  struct:struct-name
                                  0 ;; init
                                  0 ;; auto
                                  #f  ;; auto-v
                                  '() ;; props
                                  #f  ;; inspector
                                  #f ;; proc-spec
                                  '()  ;; immutable-k-list
                                  (λ args
                                    (let ([vals (let loop ([args args])
                                                  (cond
                                                    [(null? args) null]
                                                    [(null? (cdr args)) null]
                                                    [else (cons (car args) (loop (cdr args)))]))])
                                      (apply values
                                             (map (λ (ctc val)
                                                    (-contract ctc
                                                               val
                                                               'not-enough-info-for-blame
                                                               'not-enough-info-for-blame))
                                                  ctcs
                                                  vals)))))])
    struct:struct-name))

(define-syntax (-contract stx)
  (syntax-case stx ()
    [(_ a-contract to-check pos-blame-e neg-blame-e)
     (let ([s (syntax/loc stx here)])
       (quasisyntax/loc stx
         (contract/proc a-contract to-check pos-blame-e neg-blame-e 
                        (list (make-srcloc (quote-syntax #,s)
                                           #,(syntax-line s)
                                           #,(syntax-column s)
                                           #,(syntax-position s)
                                           #,(syntax-span s)) 
                              #f))))]
    [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
     (syntax/loc stx
       (begin
         (contract/proc a-contract-e to-check pos-blame-e neg-blame-e src-info-e)))]))

(define (contract/proc a-contract-raw name pos-blame neg-blame src-info)
  (let ([a-contract (coerce-contract 'contract a-contract-raw)])

    (unless (or (and (list? src-info)
                     (= 2 (length src-info))
                     (srcloc? (list-ref src-info 0))
                     (or (string? (list-ref src-info 1))
                         (not (list-ref src-info 1))))
                (syntax? src-info))
      (error 'contract "expected syntax or a list of two elements (srcloc and string or #f) as last argument, given: ~e, other args ~e ~e ~e ~e"
             src-info
             (unpack-blame neg-blame)
             (unpack-blame pos-blame)
             a-contract-raw
             name))
    (((contract-proc a-contract) pos-blame neg-blame src-info (contract-name a-contract) #t)
     name)))

(define-syntax (recursive-contract stx)
  (syntax-case stx ()
    [(_ arg)
     (syntax (make-proj-contract 
              '(recursive-contract arg) 
              (λ (pos-blame neg-blame src str positive-position?)
                (let ([ctc (coerce-contract 'recursive-contract arg)])
                  (let ([proc (contract-proc ctc)])
                    (λ (val)
                      ((proc pos-blame neg-blame src str positive-position?) val)))))
              #f))]))

;                                                                                                   
;                                                                                                   
;                                                                                                   
;               ;                                                                                   
;                                                                                                   
;                                                                ;                       ;          
;   ; ;;  ;;    ;    ;;;    ;;;            ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;;   ;;;  
;   ;;  ;;  ;   ;   ;      ;   ;          ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;    ;     
;   ;   ;   ;   ;   ;;    ;              ;      ;     ;  ;   ;   ;    ;       ;  ;       ;    ;;    
;   ;   ;   ;   ;    ;;   ;              ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;     ;;   
;   ;   ;   ;   ;      ;  ;              ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;       ;  
;   ;   ;   ;   ;      ;   ;   ;  ;       ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;       ;  
;   ;   ;   ;   ;   ;;;     ;;;   ;        ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;;  ;;;   
;                                                                                                   
;                                                                                                   
;                                                                                                   



(provide flat-rec-contract
         flat-murec-contract
         or/c 
         not/c
         =/c >=/c <=/c </c >/c between/c
         integer-in
         real-in
         natural-number/c
         string-len/c
         false/c
         printable/c
         symbols one-of/c
         listof non-empty-listof cons/c list/c
         vectorof vector-immutableof vector/c vector-immutable/c 
         box-immutable/c box/c
         promise/c
         struct/c
         syntax/c
         
         check-between/c
         check-unary-between/c
         parameter/c
         hash/c)

(define-syntax (flat-rec-contract stx)
  (syntax-case stx  ()
    [(_ name ctc ...)
     (identifier? (syntax name))
     (with-syntax ([(ctc-id ...) (generate-temporaries (syntax (ctc ...)))]
                   [(pred-id ...) (generate-temporaries (syntax (ctc ...)))])
       (syntax 
        (let* ([pred (λ (x) (error 'flat-rec-contract "applied too soon"))]
               [name (flat-contract (let ([name (λ (x) (pred x))]) name))])
          (let ([ctc-id (coerce-contract 'flat-rec-contract ctc)] ...)
            (unless (flat-contract? ctc-id)
              (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
            ...
            (set! pred
                  (let ([pred-id (flat-contract-predicate ctc-id)] ...)
                    (λ (x)
                      (or (pred-id x) ...))))
            name))))]
    [(_ name ctc ...)
     (raise-syntax-error 'flat-rec-contract "expected first argument to be an identifier" stx (syntax name))]))

(define-syntax (flat-murec-contract stx)
  (syntax-case stx  ()
    [(_ ([name ctc ...] ...) body1 body ...)
     (andmap identifier? (syntax->list (syntax (name ...))))
     (with-syntax ([((ctc-id ...) ...) (map generate-temporaries
                                            (syntax->list (syntax ((ctc ...) ...))))]
                   [(pred-id ...) (generate-temporaries (syntax (name ...)))]
                   [((pred-arm-id ...) ...) (map generate-temporaries
                                                 (syntax->list (syntax ((ctc ...) ...))))])
       (syntax 
        (let* ([pred-id (λ (x) (error 'flat-murec-contract "applied too soon"))] ...
               [name (flat-contract (let ([name (λ (x) (pred-id x))]) name))] ...)
          (let-values ([(ctc-id ...) (values (coerce-contract 'flat-rec-contract ctc) ...)] ...)
            (begin
              (void)
              (unless (flat-contract? ctc-id)
                (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
              ...) ...
            (set! pred-id
                  (let ([pred-arm-id (flat-contract-predicate ctc-id)] ...)
                    (λ (x)
                      (or (pred-arm-id x) ...)))) ...
            body1
            body ...))))]
    [(_ ([name ctc ...] ...) body1 body ...)
     (for-each (λ (name)
                 (unless (identifier? name)
                   (raise-syntax-error 'flat-rec-contract
                                       "expected an identifier" stx name)))
               (syntax->list (syntax (name ...))))]
    [(_ ([name ctc ...] ...))
     (raise-syntax-error 'flat-rec-contract "expected at least one body expression" stx)]))

(define or/c
  (case-lambda 
    [() (make-none/c '(or/c))]
    [raw-args
     (let ([args (coerce-contracts 'or/c raw-args)])
       (let-values ([(ho-contracts flat-contracts)
                     (let loop ([ho-contracts '()]
                                [flat-contracts '()]
                                [args args])
                       (cond
                         [(null? args) (values ho-contracts (reverse flat-contracts))]
                         [else 
                          (let ([arg (car args)])
                            (cond
                              [(flat-contract? arg)
                               (loop ho-contracts (cons arg flat-contracts) (cdr args))]
                              [else
                               (loop (cons arg ho-contracts) flat-contracts (cdr args))]))]))])
         (let ([pred 
                (cond
                  [(null? flat-contracts) not]
                  [else
                   (let loop ([fst (car flat-contracts)]
                              [rst (cdr flat-contracts)])
                     (let ([fst-pred (flat-contract-predicate fst)])
                       (cond
                         [(null? rst) fst-pred]
                         [else 
                          (let ([r (loop (car rst) (cdr rst))])
                            (λ (x) (or (fst-pred x) (r x))))])))])])
           (cond
             [(null? ho-contracts)
              (make-flat-or/c pred flat-contracts)]
             [(null? (cdr ho-contracts))
              (make-or/c pred flat-contracts (car ho-contracts))]
             [else
              (make-multi-or/c flat-contracts ho-contracts)]))))]))

(define-struct or/c (pred flat-ctcs ho-ctc)
  #:omit-define-syntaxes
  #:property proj-prop
  (λ (ctc)
    (let ([c-proc ((proj-get (or/c-ho-ctc ctc)) (or/c-ho-ctc ctc))]
          [pred (or/c-pred ctc)])
      (λ (pos-blame neg-blame src-info orig-str positive-position?)
        (let ([partial-contract (c-proc pos-blame neg-blame src-info orig-str positive-position?)])
          (λ (val)
            (cond
              [(pred val) val]
              [else
               (partial-contract val)]))))))
  
  #:property name-prop
  (λ (ctc)
    (apply build-compound-type-name 
           'or/c 
           (or/c-ho-ctc ctc)
           (or/c-flat-ctcs ctc)))
  
  #:property first-order-prop
  (λ (ctc)
    (let ([pred (or/c-pred ctc)]
          [ho ((first-order-get (or/c-ho-ctc ctc)) (or/c-ho-ctc ctc))])
      (λ (x)
        (or (ho x)
            (pred x)))))
   
  #:property stronger-prop
  (λ (this that)
    (and (or/c? that)
         (contract-stronger? (or/c-ho-ctc this) (or/c-ho-ctc that))
         (let ([this-ctcs (or/c-flat-ctcs this)]
               [that-ctcs (or/c-flat-ctcs that)])
           (and (= (length this-ctcs) (length that-ctcs))
                (andmap contract-stronger?
                        this-ctcs
                        that-ctcs))))))

(define (multi-or/c-proj ctc)
  (let* ([ho-contracts (multi-or/c-ho-ctcs ctc)]
         [c-procs (map (λ (x) ((proj-get x) x)) ho-contracts)]
         [first-order-checks (map (λ (x) ((first-order-get x) x)) ho-contracts)]
         [predicates (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))])
    (λ (pos-blame neg-blame src-info orig-str positive-position?)
      (let ([partial-contracts (map (λ (c-proc) (c-proc pos-blame neg-blame src-info orig-str positive-position?)) c-procs)])
        (λ (val)
          (cond
            [(ormap (λ (pred) (pred val)) predicates)
             val]
            [else
             (let loop ([checks first-order-checks]
                        [procs partial-contracts]
                        [contracts ho-contracts]
                        [candidate-proc #f]
                        [candidate-contract #f])
               (cond
                 [(null? checks)
                  (if candidate-proc
                      (candidate-proc val)
                      (raise-contract-error val src-info pos-blame orig-str 
                                            "none of the branches of the or/c matched, given ~e"
                                            val))]
                 [((car checks) val)
                  (if candidate-proc
                      (raise-contract-error val src-info pos-blame orig-str
                                            "two of the clauses in the or/c might both match: ~s and ~s, given ~e"
                                            (contract-name candidate-contract)
                                            (contract-name (car contracts))
                                            val)
                      (loop (cdr checks)
                            (cdr procs)
                            (cdr contracts)
                            (car procs)
                            (car contracts)))]
                 [else
                  (loop (cdr checks)
                        (cdr procs)
                        (cdr contracts)
                        candidate-proc
                        candidate-contract)]))]))))))

(define-struct multi-or/c (flat-ctcs ho-ctcs)
  #:property proj-prop multi-or/c-proj
  #:property name-prop
  (λ (ctc)
    (apply build-compound-type-name 
           'or/c 
           (append
            (multi-or/c-flat-ctcs ctc)
            (reverse (multi-or/c-ho-ctcs ctc)))))
  
  #:property first-order-prop
  (λ (ctc)
    (let ([flats (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))]
          [hos (map (λ (x) ((first-order-get x) x)) (multi-or/c-ho-ctcs ctc))])
      (λ (x)
        (or (ormap (λ (f) (f x)) hos)
            (ormap (λ (f) (f x)) flats)))))
  
  #:property stronger-prop
  (λ (this that)
    (and (multi-or/c? that)
         (let ([this-ctcs (multi-or/c-ho-ctcs this)]
               [that-ctcs (multi-or/c-ho-ctcs that)])
           (and (= (length this-ctcs) (length that-ctcs))
                (andmap contract-stronger?
                        this-ctcs
                        that-ctcs)))
         (let ([this-ctcs (multi-or/c-flat-ctcs this)]
               [that-ctcs (multi-or/c-flat-ctcs that)])
           (and (= (length this-ctcs) (length that-ctcs))
                (andmap contract-stronger?
                        this-ctcs
                        that-ctcs))))))

(define-struct flat-or/c (pred flat-ctcs)
  #:property proj-prop flat-proj
  #:property name-prop
  (λ (ctc)
    (apply build-compound-type-name 
           'or/c 
           (flat-or/c-flat-ctcs ctc)))
  #:property stronger-prop
  (λ (this that)
    (and (flat-or/c? that)
         (let ([this-ctcs (flat-or/c-flat-ctcs this)]
               [that-ctcs (flat-or/c-flat-ctcs that)])
           (and (= (length this-ctcs) (length that-ctcs))
                (andmap contract-stronger?
                        this-ctcs
                        that-ctcs)))))

  #:property flat-prop
  (λ (ctc) (flat-or/c-pred ctc)))

;;
;; or/c opter
;;
(define/opter (or/c opt/i opt/info stx)
  ;; FIXME code duplication
  (define (opt/or-unknown uctc)
    (let* ((lift-var (car (generate-temporaries (syntax (lift)))))
           (partial-var (car (generate-temporaries (syntax (partial))))))
      (values
       (with-syntax ((partial-var partial-var)
                     (lift-var lift-var)
                     (uctc uctc)
                     (val (opt/info-val opt/info)))
         (syntax (partial-var val)))
       (list (cons lift-var 
                   ;; FIXME needs to get the contract name somehow
                   (with-syntax ((uctc uctc))
                     (syntax (coerce-contract 'opt/c uctc)))))
       null
       (list (cons
              partial-var
              (with-syntax ((lift-var lift-var)
                            (pos (opt/info-pos opt/info))
                            (neg (opt/info-neg opt/info))
                            (src-info (opt/info-src-info opt/info))
                            (orig-str (opt/info-orig-str opt/info))
                            (positive-position? (opt/info-orig-str opt/info)))
                (syntax (((proj-get lift-var) lift-var) pos neg src-info orig-str positive-position?)))))
       #f
       lift-var
       (list #f)
       null)))
  
  (define (opt/or-ctc ps)
    (let ((lift-from-hos null)
          (superlift-from-hos null)
          (partial-from-hos null))
      (let-values ([(opt-ps lift-ps superlift-ps partial-ps stronger-ribs hos ho-ctc)
                    (let loop ([ps ps]
                               [next-ps null]
                               [lift-ps null]
                               [superlift-ps null]
                               [partial-ps null]
                               [stronger-ribs null]
                               [hos null]
                               [ho-ctc #f])
                      (cond
                        [(null? ps) (values next-ps
                                            lift-ps
                                            superlift-ps
                                            partial-ps
                                            stronger-ribs
                                            (reverse hos)
                                            ho-ctc)]
                        [else
                         (let-values ([(next lift superlift partial flat _ this-stronger-ribs)
                                       (opt/i opt/info (car ps))])
                           (if flat
                               (loop (cdr ps)
                                     (cons flat next-ps)
                                     (append lift-ps lift)
                                     (append superlift-ps superlift)
                                     (append partial-ps partial)
                                     (append this-stronger-ribs stronger-ribs)
                                     hos
                                     ho-ctc)
                               (if (< (length hos) 1)
                                   (loop (cdr ps)
                                         next-ps
                                         (append lift-ps lift)
                                         (append superlift-ps superlift)
                                         (append partial-ps partial)
                                         (append this-stronger-ribs stronger-ribs)
                                         (cons (car ps) hos)
                                         next)
                                   (loop (cdr ps)
                                         next-ps
                                         lift-ps
                                         superlift-ps
                                         partial-ps
                                         stronger-ribs
                                         (cons (car ps) hos)
                                         ho-ctc))))]))])
        (with-syntax ((next-ps
                       (with-syntax (((opt-p ...) (reverse opt-ps)))
                         (syntax (or opt-p ...)))))
          (values
           (cond
             [(null? hos) 
              (with-syntax ([val (opt/info-val opt/info)]
                            [pos (opt/info-pos opt/info)]
                            [src-info (opt/info-src-info opt/info)]
                            [orig-str (opt/info-orig-str opt/info)])
                (syntax
                 (if next-ps 
                     val
                     (raise-contract-error val src-info pos orig-str 
                                           "none of the branches of the or/c matched"))))]
             [(= (length hos) 1) (with-syntax ((ho-ctc ho-ctc))
                                   (syntax
                                    (if next-ps val ho-ctc)))]
             ;; FIXME something's not right with this case.
             [(> (length hos) 1)
              (let-values ([(next-hos lift-hos superlift-hos partial-hos _ __ stronger-hos stronger-vars-hos)
                            (opt/or-unknown stx)])
                (set! lift-from-hos lift-hos)
                (set! superlift-from-hos superlift-hos)
                (set! partial-from-hos partial-hos)
                (with-syntax ((next-hos next-hos))
                  (syntax
                   (if next-ps val next-hos))))])
           (append lift-ps lift-from-hos)
           (append superlift-ps superlift-from-hos)
           (append partial-ps partial-from-hos)
           (if (null? hos) (syntax next-ps) #f)
           #f
           stronger-ribs)))))
  
  (syntax-case stx (or/c)
    [(or/c p ...)
     (opt/or-ctc (syntax->list (syntax (p ...))))]))

(define false/c #f)

(define (string-len/c n)
  (unless (number? n)
    (error 'string-len/c "expected a number as argument, got ~e" n))
  (flat-named-contract 
   `(string-len/c ,n)
   (λ (x)
     (and (string? x)
          ((string-length x) . < . n)))))

(define (symbols . ss)
  (unless ((length ss) . >= . 1)
    (error 'symbols "expected at least one argument"))
  (unless (andmap symbol? ss)
    (error 'symbols "expected symbols as arguments, given: ~a"
           (apply string-append (map (λ (x) (format "~e " x)) ss))))
  (make-one-of/c ss))

(define atomic-value? 
  (let ([undefined (letrec ([x x]) x)])
    (λ (x)
      (or (char? x) (symbol? x) (boolean? x)
          (null? x) (keyword? x) (number? x)
          (void? x) (eq? x undefined)))))

(define (one-of/c . elems)
  (unless (andmap atomic-value? elems)
    (error 'one-of/c "expected chars, symbols, booleans, null, keywords, numbers, void, or undefined, got ~e"
           elems))
  (make-one-of/c elems))

(define (one-of-pc x)
  (cond
    [(symbol? x)
     `',x]
    [(null? x)
     ''()]
    [(void? x)
     '(void)]
    [(or (char? x) 
         (boolean? x)
         (keyword? x)
         (number? x))
     x]
    [(eq? x (letrec ([x x]) x))
     '(letrec ([x x]) x)]
    [else (error 'one-of-pc "undef ~s" x)]))


(define-struct one-of/c (elems)
  #:omit-define-syntaxes
  #:property proj-prop flat-proj
  #:property name-prop
  (λ (ctc) 
    (let ([elems (one-of/c-elems ctc)])
      `(,(cond
           [(andmap symbol? elems)
            'symbols]
           [else
            'one-of/c])
        ,@(map one-of-pc elems))))
  
  #:property stronger-prop
  (λ (this that)
    (and (one-of/c? that)
         (let ([this-elems (one-of/c-elems this)]
               [that-elems (one-of/c-elems that)])
           (and 
            (andmap (λ (this-elem) (memv this-elem that-elems))
                    this-elems)
            #t))))
  #:property flat-prop 
  (λ (ctc) 
    (let ([elems (one-of/c-elems ctc)])
      (λ (x) (memv x elems)))))

(define printable/c
  (flat-named-contract
   'printable/c
   (λ (x)
     (let printable? ([x x])
       (or (symbol? x)
           (string? x)
           (bytes? x)
           (boolean? x)
           (char? x)
           (null? x)
           (number? x)
           (regexp? x)
           (prefab-struct-key x) ;; this cannot be last, since it doesn't return just #t
           (and (pair? x)
                (printable? (car x))
                (printable? (cdr x)))
           (and (vector? x)
                (andmap printable? (vector->list x)))
           (and (box? x)
                (printable? (unbox x))))))))

(define-struct between/c (low high)
  #:omit-define-syntaxes
  #:property proj-prop flat-proj
  #:property name-prop
  (λ (ctc) 
    (let ([n (between/c-low ctc)]
          [m (between/c-high ctc)])
      (cond
        [(= n -inf.0) `(<=/c ,m)]
        [(= m +inf.0) `(>=/c ,n)]
        [(= n m) `(=/c ,n)]
        [else `(between/c ,n ,m)])))

  #:property stronger-prop
  (λ (this that)
    (and (between/c? that)
         (<= (between/c-low that) (between/c-low this))
         (<= (between/c-high this) (between/c-high that))))
  
  #:property flat-prop
  (λ (ctc) 
    (let ([n (between/c-low ctc)]
          [m (between/c-high ctc)])
      (λ (x) 
        (and (number? x)
             (<= n x m))))))

(define-syntax (check-unary-between/c stx)
  (syntax-case stx ()
    [(_ 'sym x-exp)
     (identifier? #'sym)
     #'(let ([x x-exp])
         (unless (real? x)
           (error 'sym "expected a real number, got ~e" x)))]))

(define (=/c x) 
  (check-unary-between/c '=/c x)
  (make-between/c x x))
(define (<=/c x) 
  (check-unary-between/c '<=/c x)
  (make-between/c -inf.0 x))
(define (>=/c x)
  (check-unary-between/c '>=/c x)
  (make-between/c x +inf.0))
(define (check-between/c x y)
  (unless (number? x)
    (error 'between/c "expected a number as first argument, got ~e, other arg ~e" x y))
  (unless (number? y)
    (error 'between/c "expected a number as second argument, got ~e, other arg ~e" y x)))
(define (between/c x y)
  (check-between/c x y)
  (make-between/c x y))

;;
;; between/c opter helper
;;



;;
;; between/c opters
;;
;; note that the checkers are used by both optimized and normal contracts.
;;
(define/opter (between/c opt/i opt/info stx)
  (syntax-case stx (between/c)
    [(between/c low high) 
     (let*-values ([(lift-low lifts1) (lift/binding #'low 'between-low empty-lifts)]
                   [(lift-high lifts2) (lift/binding #'high 'between-high lifts1)])
       (with-syntax ([n lift-low]
                     [m lift-high])
         (let ([lifts3 (lift/effect #'(check-between/c n m) lifts2)])
           (with-syntax ((val (opt/info-val opt/info))
                         (ctc (opt/info-contract opt/info))
                         (pos (opt/info-pos opt/info))
                         (src-info (opt/info-src-info opt/info))
                         (orig-str (opt/info-orig-str opt/info))
                         (this (opt/info-this opt/info))
                         (that (opt/info-that opt/info)))
             (values
              (syntax (if (and (number? val) (<= n val m)) 
                          val
                          (raise-contract-error
                           val
                           src-info
                           pos
                           orig-str
                           "expected <~a>, given: ~e"
                           ((name-get ctc) ctc)
                           val)))
              lifts3
              null
              null
              (syntax (and (number? val) (<= n val m)))
              #f
              (list (new-stronger-var
                     lift-low
                     (λ (this that)
                       (with-syntax ([this this]
                                     [that that])
                         (syntax (<= that this)))))
                    (new-stronger-var
                     lift-high
                     (λ (this that)
                       (with-syntax ([this this]
                                     [that that])
                         (syntax (<= this that)))))))))))]))

(define-for-syntax (single-comparison-opter opt/info stx check-arg comparison arg)
  (with-syntax ([comparison comparison])
    (let*-values ([(lift-low lifts2) (lift/binding arg 'single-comparison-val empty-lifts)])
      (with-syntax ([m lift-low])
        (let ([lifts3 (lift/effect (check-arg #'m) lifts2)])
          (with-syntax ((val (opt/info-val opt/info))
                        (ctc (opt/info-contract opt/info))
                        (pos (opt/info-pos opt/info))
                        (src-info (opt/info-src-info opt/info))
                        (orig-str (opt/info-orig-str opt/info))
                        (this (opt/info-this opt/info))
                        (that (opt/info-that opt/info)))
            (values
             (syntax 
              (if (and (number? val) (comparison val m)) 
                  val
                  (raise-contract-error
                   val
                   src-info
                   pos
                   orig-str
                   "expected <~a>, given: ~e"
                   ((name-get ctc) ctc)
                   val)))
             lifts3
             null
             null
             (syntax (and (number? val) (comparison val m)))
             #f
             (list (new-stronger-var
                    lift-low
                    (λ (this that)
                      (with-syntax ([this this]
                                    [that that])
                        (syntax (comparison this that)))))))))))))

(define/opter (>=/c opt/i opt/info stx)
  (syntax-case stx (>=/c)
    [(>=/c low)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '>=/c m)))
      #'>=
      #'low)]))

(define/opter (<=/c opt/i opt/info stx)
  (syntax-case stx (<=/c)
    [(<=/c high)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '<=/c m)))
      #'<=
      #'high)]))

(define/opter (>/c opt/i opt/info stx)
  (syntax-case stx (>/c)
    [(>/c low)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '>/c m)))
      #'>
      #'low)]))

(define/opter (</c opt/i opt/info stx)
  (syntax-case stx (</c)
    [(</c high)
     (single-comparison-opter 
      opt/info
      stx
      (λ (m) (with-syntax ([m m])
               #'(check-unary-between/c '</c m)))
      #'<
      #'high)]))

(define (</c x)
  (flat-named-contract
   `(</c ,x)
   (λ (y) (and (number? y) (< y x)))))
(define (>/c x)
  (flat-named-contract
   `(>/c ,x)
   (λ (y) (and (number? y) (> y x)))))

(define natural-number/c
  (flat-named-contract
   'natural-number/c
   (λ (x)
     (and (number? x)
          (integer? x)
          (exact? x)
          (x . >= . 0)))))

(define (integer-in start end)
  (unless (and (integer? start)
               (exact? start)
               (integer? end)
               (exact? end))
    (error 'integer-in "expected two exact integers as arguments, got ~e and ~e" start end))
  (flat-named-contract 
   `(integer-in ,start ,end)
   (λ (x)
     (and (integer? x)
          (exact? x)
          (<= start x end)))))

(define (real-in start end)
  (unless (and (real? start)
               (real? end))
    (error 'real-in "expected two real numbers as arguments, got ~e and ~e" start end))
  (flat-named-contract 
   `(real-in ,start ,end)
   (λ (x)
     (and (real? x)
          (<= start x end)))))

(define (not/c f)
  (let* ([ctc (coerce-flat-contract 'not/c f)]
         [pred (flat-contract-predicate ctc)])
    (build-flat-contract
     (build-compound-type-name 'not/c ctc)
     (λ (x) (not (pred x))))))

(define-syntax (*-immutableof stx)
  (syntax-case stx ()
    [(_ predicate? fill testmap type-name name)
     (identifier? (syntax predicate?))
     (syntax
      (let ([fill-name fill])
        (λ (input)
          (let ([ctc (coerce-contract 'name input)])
            (if (flat-contract? ctc)
                (let ([content-pred? (flat-contract-predicate ctc)])
                  (build-flat-contract
                   `(name ,(contract-name ctc))
                   (lambda (x) (and (predicate? x) (testmap content-pred? x)))))
                (let ([proj (contract-proc ctc)])
                  (make-proj-contract
                   (build-compound-type-name 'name ctc)
                   (λ (pos-blame neg-blame src-info orig-str positive-position?)
                     (let ([p-app (proj pos-blame neg-blame src-info orig-str positive-position?)])
                       (λ (val)
                         (unless (predicate? val)
                           (raise-contract-error
                            val
                            src-info
                            pos-blame
                            orig-str
                            "expected <~a>, given: ~e"
                            'type-name
                            val))
                         (fill-name p-app val))))
                   predicate?)))))))]))

(define listof
  (*-immutableof list? map andmap list listof))

(define (non-empty-list? x) (and (pair? x) (list? (cdr x))))
(define non-empty-listof
  (*-immutableof non-empty-list? map andmap non-empty-list non-empty-listof))

(define (immutable-vector? val) (and (immutable? val) (vector? val)))

(define vector-immutableof
  (*-immutableof immutable-vector?
                 (λ (f v) (apply vector-immutable (map f (vector->list v))))
                 (λ (f v) (andmap f (vector->list v)))
                 immutable-vector
                 vector-immutableof))

(define (vectorof p)
  (let* ([ctc (coerce-flat-contract 'vectorof p)]
         [pred (flat-contract-predicate ctc)])
    (build-flat-contract
     (build-compound-type-name 'vectorof ctc)
     (λ (v)
       (and (vector? v)
            (andmap pred (vector->list v)))))))

(define (vector/c . args)
  (let* ([ctcs (coerce-flat-contracts 'vector/c args)]
         [largs (length args)]
         [procs (map flat-contract-predicate ctcs)])
    (build-flat-contract
     (apply build-compound-type-name 'vector/c ctcs)
     (λ (v)
       (and (vector? v)
            (= (vector-length v) largs)
            (andmap (λ (p? x) (p? x))
                    procs
                    (vector->list v)))))))

(define (box/c pred)
  (let* ([ctc (coerce-flat-contract 'box/c pred)]
         [p? (flat-contract-predicate ctc)])
    (build-flat-contract
     (build-compound-type-name 'box/c ctc)
     (λ (x)
       (and (box? x)
            (p? (unbox x)))))))

;;
;; cons/c opter
;;
(define/opter (cons/c opt/i opt/info stx)
  (define (opt/cons-ctc hdp tlp)
    (let-values ([(next-hdp lifts-hdp superlifts-hdp partials-hdp flat-hdp unknown-hdp stronger-ribs-hd)
                  (opt/i opt/info hdp)]
                 [(next-tlp lifts-tlp superlifts-tlp partials-tlp flat-tlp unknown-tlp stronger-ribs-tl)
                  (opt/i opt/info tlp)]
                 [(error-check) (car (generate-temporaries (syntax (error-check))))])
      (with-syntax ((next (with-syntax ((flat-hdp flat-hdp)
                                        (flat-tlp flat-tlp)
                                        (val (opt/info-val opt/info)))
                            (syntax
                             (and (pair? val)
                                  (let ((val (car val))) flat-hdp)
                                  (let ((val (cdr val))) flat-tlp))))))
        (values
         (with-syntax ((val (opt/info-val opt/info))
                       (ctc (opt/info-contract opt/info))
                       (pos (opt/info-pos opt/info))
                       (src-info (opt/info-src-info opt/info))
                       (orig-str (opt/info-orig-str opt/info)))
           (syntax (if next
                       val
                       (raise-contract-error
                        val
                        src-info
                        pos
                        orig-str
                        "expected <~a>, given: ~e"
                        ((name-get ctc) ctc)
                        val))))
         (append
          lifts-hdp lifts-tlp
          (list (cons error-check
                      (with-syntax ((hdp hdp)
                                    (tlp tlp)
                                    (check (with-syntax ((flat-hdp
                                                          (cond
                                                            [unknown-hdp
                                                             (with-syntax ((ctc unknown-hdp))
                                                               (syntax (flat-contract/predicate? ctc)))]
                                                            [else (if flat-hdp #'#t #'#f)]))
                                                         (flat-tlp
                                                          (cond
                                                            [unknown-tlp
                                                             (with-syntax ((ctc unknown-tlp))
                                                               (syntax (flat-contract/predicate? ctc)))]
                                                            [else (if flat-tlp #'#t #'#f)])))
                                             (syntax (and flat-hdp flat-tlp)))))
                        (syntax
                         (unless check
                           (error 'cons/c "expected two flat contracts or procedures of arity 1, got: ~e and ~e"
                                  hdp tlp)))))))
         (append superlifts-hdp superlifts-tlp)
         (append partials-hdp partials-tlp)
         (syntax (if next #t #f))
         #f
         (append stronger-ribs-hd stronger-ribs-tl)))))
  
  (syntax-case stx (cons/c)
    [(cons/c hdp tlp)
     (opt/cons-ctc #'hdp #'tlp)]))

;; only used by the opters
(define (flat-contract/predicate? pred)
  (or (flat-contract? pred)
      (and (procedure? pred)
           (procedure-arity-includes? pred 1))))


(define-syntax (*-immutable/c stx)
  (syntax-case stx ()
    [(_ predicate? constructor (arb? selectors ...) type-name name)
     #'(*-immutable/c predicate? constructor (arb? selectors ...) type-name name #t)]
    [(_ predicate? constructor (arb? selectors ...) type-name name test-immutable?)
     (and (eq? #f (syntax->datum (syntax arb?)))
          (boolean? (syntax->datum #'test-immutable?)))
     (let ([test-immutable? (syntax->datum #'test-immutable?)])
       (with-syntax ([(params ...) (generate-temporaries (syntax (selectors ...)))]
                     [(p-apps ...) (generate-temporaries (syntax (selectors ...)))]
                     [(ctc-x ...) (generate-temporaries (syntax (selectors ...)))]
                     [(procs ...) (generate-temporaries (syntax (selectors ...)))]
                     [(selector-names ...) (generate-temporaries (syntax (selectors ...)))])
         #`(let ([predicate?-name predicate?]
                 [constructor-name constructor]
                 [selector-names selectors] ...)
             (λ (params ...)
               (let ([ctc-x (coerce-contract 'name params)] ...)
                 (if (and (flat-contract? ctc-x) ...)
                     (let ([p-apps (flat-contract-predicate ctc-x)] ...)
                       (build-flat-contract
                        `(name ,(contract-name ctc-x) ...)
                        (lambda (x)
                          (and (predicate?-name x)
                               (p-apps (selector-names x)) 
                               ...))))
                     (let ([procs (contract-proc ctc-x)] ...)
                       (make-proj-contract
                        (build-compound-type-name 'name ctc-x ...)
                        (λ (pos-blame neg-blame src-info orig-str positive-position?)
                          (let ([p-apps (procs pos-blame neg-blame src-info orig-str positive-position?)] ...)
                            (λ (v)
                              (if #,(if test-immutable?
                                        #'(and (predicate?-name v)
                                               (immutable? v))
                                        #'(predicate?-name v))
                                  (constructor-name (p-apps (selector-names v)) ...)
                                  (raise-contract-error
                                   v
                                   src-info
                                   pos-blame
                                   orig-str
                                   #,(if test-immutable?
                                         "expected immutable <~a>, given: ~e"
                                         "expected <~a>, given: ~e")
                                   'type-name
                                   v)))))
                        #f))))))))]
    [(_ predicate? constructor (arb? selector) correct-size type-name name)
     (eq? #t (syntax->datum (syntax arb?)))
     (syntax
      (let ([predicate?-name predicate?]
            [constructor-name constructor]
            [selector-name selector])
        (λ params
          (let ([ctcs (map (λ (param) (coerce-contract 'name param)) params)])
            (let ([procs (map contract-proc ctcs)])
              (make-proj-contract
               (apply build-compound-type-name 'name ctcs)
               (λ (pos-blame neg-blame src-info orig-str positive-position?)
                 (let ([p-apps (map (λ (proc) (proc pos-blame neg-blame src-info orig-str positive-position?)) procs)]
                       [count (length params)])
                   (λ (v)
                     (if (and (immutable? v)
                              (predicate?-name v)
                              (correct-size count v))
                         (apply constructor-name 
                                (let loop ([p-apps p-apps]
                                           [i 0])
                                  (cond
                                    [(null? p-apps) null]
                                    [else (let ([p-app (car p-apps)])
                                            (cons (p-app (selector-name v i))
                                                  (loop (cdr p-apps) (+ i 1))))])))
                         (raise-contract-error
                          v
                          src-info
                          pos-blame
                          orig-str
                          "expected <~a>, given: ~e"
                          'type-name
                          v)))))
               #f))))))]))

(define cons/c (*-immutable/c pair? cons (#f car cdr) cons cons/c #f))
(define box-immutable/c (*-immutable/c box? box-immutable (#f unbox) immutable-box box-immutable/c))
(define vector-immutable/c (*-immutable/c vector?
                                          vector-immutable
                                          (#t (λ (v i) (vector-ref v i)))
                                          (λ (n v) (= n (vector-length v)))
                                          immutable-vector
                                          vector-immutable/c))

;;
;; cons/c opter
;;
(define/opter (cons/c opt/i opt/info stx)
  (define (opt/cons-ctc hdp tlp)
    (let-values ([(next-hdp lifts-hdp superlifts-hdp partials-hdp flat-hdp unknown-hdp stronger-ribs-hd)
                  (opt/i opt/info hdp)]
                 [(next-tlp lifts-tlp superlifts-tlp partials-tlp flat-tlp unknown-tlp stronger-ribs-tl)
                  (opt/i opt/info tlp)])
      (with-syntax ((check (with-syntax ((val (opt/info-val opt/info)))
                             (syntax (pair? val)))))
        (values
         (with-syntax ((val (opt/info-val opt/info))
                       (ctc (opt/info-contract opt/info))
                       (pos (opt/info-pos opt/info))
                       (src-info (opt/info-src-info opt/info))
                       (orig-str (opt/info-orig-str opt/info))
                       (next-hdp next-hdp)
                       (next-tlp next-tlp))
           (syntax (if check
                       (cons (let ((val (car val))) next-hdp)
                             (let ((val (cdr val))) next-tlp))
                       (raise-contract-error
                        val
                        src-info
                        pos
                        orig-str
                        "expected <~a>, given: ~e"
                        ((name-get ctc) ctc)
                        val))))        
         (append lifts-hdp lifts-tlp) 
         (append superlifts-hdp superlifts-tlp)
         (append partials-hdp partials-tlp)
         (if (and flat-hdp flat-tlp)
             (with-syntax ((val (opt/info-val opt/info))
                           (flat-hdp flat-hdp)
                           (flat-tlp flat-tlp))
               (syntax (if (and check
                                (let ((val (car val))) flat-hdp)
                                (let ((val (cdr val))) flat-tlp)) #t #f)))
             #f)
         #f
         (append stronger-ribs-hd stronger-ribs-tl)))))
  
  (syntax-case stx (cons/c)
    [(_ hdp tlp) (opt/cons-ctc #'hdp #'tlp)]))

(define (list/c . args)
  (let loop ([args (coerce-contracts 'list/c args)])
    (cond
      [(null? args) (flat-contract null?)]
      [else (cons/c (car args) (loop (cdr args)))])))

(define (syntax/c ctc-in)
  (let ([ctc (coerce-contract 'syntax/c ctc-in)])
    (build-flat-contract
     (build-compound-type-name 'syntax/c ctc)
     (let ([pred (flat-contract-predicate ctc)])
       (λ (val)
         (and (syntax? val)
              (pred (syntax-e val))))))))

(define promise/c
  (λ (ctc-in)
    (let* ([ctc (coerce-contract 'promise/c ctc-in)]
           [ctc-proc (contract-proc ctc)])
      (make-proj-contract
       (build-compound-type-name 'promise/c ctc)
       (λ (pos-blame neg-blame src-info orig-str positive-position?)
         (let ([p-app (ctc-proc pos-blame neg-blame src-info orig-str positive-position?)])
           (λ (val)
             (unless (promise? val)
               (raise-contract-error
                val
                src-info
                pos-blame
                'ignored
                orig-str
                "expected <promise>, given: ~e"
                val))
             (delay (p-app (force val))))))
       promise?))))

#|
   as with copy-struct in struct.ss, this first begin0
   expansion "declares" that struct/c is an expression.
   It prevents further expansion until the internal definition
   context is sorted out.
  |#
(define-syntax (struct/c stx)
  (syntax-case stx ()
    [(_ . args) 
     (with-syntax ([x (syntax/loc stx (do-struct/c . args))])
       (syntax/loc stx (begin0 x)))]))

(define-syntax (do-struct/c stx)
  (syntax-case stx ()
    [(_ struct-name args ...)
     (and (identifier? (syntax struct-name))
          (struct-info? (syntax-local-value (syntax struct-name) (λ () #f))))
     (with-syntax ([(ctc-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-name-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-pred-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-app-x ...) (generate-temporaries (syntax (args ...)))]
                   [(field-numbers ...)
                    (let loop ([i 0]
                               [l (syntax->list (syntax (args ...)))])
                      (cond
                        [(null? l) '()]
                        [else (cons i (loop (+ i 1) (cdr l)))]))]
                   [(type-desc-id 
                     constructor-id 
                     predicate-id 
                     (rev-selector-id ...)
                     (mutator-id ...)
                     super-id)
                    (lookup-struct-info (syntax struct-name) stx)])
       (unless (= (length (syntax->list (syntax (rev-selector-id ...))))
                  (length (syntax->list (syntax (args ...)))))
         (raise-syntax-error 'struct/c 
                             (format "expected ~a contracts because struct ~a has ~a fields"
                                     (length (syntax->list (syntax (rev-selector-id ...))))
                                     (syntax-e #'struct-name)
                                     (length (syntax->list (syntax (rev-selector-id ...)))))
                             stx))
       (with-syntax ([(selector-id ...) (reverse (syntax->list (syntax (rev-selector-id ...))))])
         (syntax
          (let ([ctc-x (coerce-contract 'struct/c args)] ...)
            
            (unless predicate-id
              (error 'struct/c "could not determine predicate for ~s" 'struct-name))
            (unless (and selector-id ...)
              (error 'struct/c "could not determine selectors for ~s" 'struct-name))
            
            (unless (flat-contract? ctc-x)
              (error 'struct/c "expected flat contracts as arguments, got ~e" args))
            ...
            
            (let ([ctc-pred-x (flat-contract-predicate ctc-x)] 
                  ...
                  [ctc-name-x (contract-name ctc-x)]
                  ...)
              (build-flat-contract
               (build-compound-type-name 'struct/c 'struct-name ctc-x ...)
               (λ (val)
                 (and (predicate-id val)
                      (ctc-pred-x (selector-id val)) ...))))))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))


(define (parameter/c x)
  (make-parameter/c (coerce-contract 'parameter/c x)))

(define-struct parameter/c (ctc)
  #:omit-define-syntaxes
  #:property proj-prop
  (λ (ctc)
    (let ([c-proc ((proj-get (parameter/c-ctc ctc)) (parameter/c-ctc ctc))])
      (λ (pos-blame neg-blame src-info orig-str positive-position?)
        (let ([partial-neg-contract (c-proc neg-blame pos-blame src-info orig-str (not positive-position?))]
              [partial-pos-contract (c-proc pos-blame neg-blame src-info orig-str positive-position?)])
          (λ (val)
            (cond
              [(parameter? val)
               (make-derived-parameter 
                val 
                partial-neg-contract
                partial-pos-contract)]
              [else
               (raise-contract-error val src-info pos-blame orig-str 
                                     "expected a parameter")]))))))
  
  #:property name-prop (λ (ctc) (build-compound-type-name 'parameter/c (parameter/c-ctc ctc)))
  #:property first-order-prop
  (λ (ctc)
    (let ([tst ((first-order-get (parameter/c-ctc ctc)) (parameter/c-ctc ctc))])
      (λ (x)
        (and (parameter? x)
             (tst (x))))))
   
  #:property stronger-prop
  (λ (this that)
    ;; must be invariant (because the library doesn't currently split out pos/neg contracts
    ;; which could be tested individually ....)
    (and (parameter/c? that)
         (contract-stronger? (parameter/c-ctc this) 
                             (parameter/c-ctc that))
         (contract-stronger? (parameter/c-ctc that) 
                             (parameter/c-ctc this)))))

(define (hash/c dom rng #:immutable [immutable 'dont-care])
  (unless (memq immutable '(#t #f dont-care))
    (error 'hash/c "expected #:immutable argument to be either #t, #f, or 'dont-care, got ~s" immutable))
  (cond
    [(eq? immutable #t) 
     (make-immutable-hash/c (coerce-contract 'hash/c dom) 
                            (coerce-contract 'hash/c rng))]
    [else
     (make-hash/c (coerce-flat-contract 'hash/c dom) 
                  (coerce-flat-contract 'hash/c rng)
                  immutable)]))

;; hash-test : hash/c -> any -> bool
(define (hash-test ctc)
  (let ([dom-proc ((flat-get (hash/c-dom ctc)) (hash/c-dom ctc))]
        [rng-proc ((flat-get (hash/c-rng ctc)) (hash/c-rng ctc))]
        [immutable (hash/c-immutable ctc)])
    (λ (val)
      (and (hash? val)
           (case immutable
             [(#t) (immutable? val)]
             [(#f) (not (immutable? val))]
             [(dont-care) #t])
           (let/ec k
             (hash-for-each
              val
              (λ (dom rng)
                (unless (dom-proc dom) (k #f))
                (unless (rng-proc rng) (k #f))))
             #t)))))

(define-struct hash/c (dom rng immutable)
  #:omit-define-syntaxes

  #:property flat-prop hash-test
  #:property proj-prop
  (λ (ctc)
    (let ([dom-proc ((proj-get (hash/c-dom ctc)) (hash/c-dom ctc))]
          [rng-proc ((proj-get (hash/c-rng ctc)) (hash/c-rng ctc))]
          [immutable (hash/c-immutable ctc)])
      (λ (pos-blame neg-blame src-info orig-str positive-position?)
        (let ([partial-dom-contract (dom-proc pos-blame neg-blame src-info orig-str positive-position?)]
              [partial-rng-contract (rng-proc pos-blame neg-blame src-info orig-str positive-position?)])
          (λ (val)
            (unless (hash? val)
              (raise-contract-error val src-info pos-blame orig-str 
                                    "expected a hash"))
            (case immutable
              [(#t) (unless (immutable? val)
                      (raise-contract-error val src-info pos-blame orig-str 
                                            "expected an immutable hash"))]
              [(#f) (when (immutable? val)
                      (raise-contract-error val src-info pos-blame orig-str 
                                            "expected a mutable hash"))]
              [(dont-care) (void)])
              
            (hash-for-each
             val
             (λ (key val)
               (partial-dom-contract key)
               (partial-rng-contract val)))
            
            val)))))
  
  #:property name-prop (λ (ctc) (apply 
                                 build-compound-type-name
                                 'hash/c (hash/c-dom ctc) (hash/c-rng ctc)
                                 (if (eq? 'dont-care (hash/c-immutable ctc))
                                     '()
                                     (list '#:immutable (hash/c-immutable ctc)))))
  #:property stronger-prop
  (λ (this that)
    #f))

(define-struct immutable-hash/c (dom rng)
  #:omit-define-syntaxes

  #:property first-order-prop (λ (ctc) (λ (val) (and (hash? val) (immutable? val))))
  #:property proj-prop
  (λ (ctc)
    (let ([dom-proc ((proj-get (immutable-hash/c-dom ctc)) (immutable-hash/c-dom ctc))]
          [rng-proc ((proj-get (immutable-hash/c-rng ctc)) (immutable-hash/c-rng ctc))])
      (λ (pos-blame neg-blame src-info orig-str positive-position?)
        (let ([partial-dom-contract (dom-proc pos-blame neg-blame src-info orig-str positive-position?)]
              [partial-rng-contract (rng-proc pos-blame neg-blame src-info orig-str positive-position?)])
          (λ (val)
            (unless (and (hash? val)
                         (immutable? val))
              (raise-contract-error val src-info pos-blame orig-str 
                                    "expected an immutable hash"))
            (make-immutable-hash
             (hash-map
              val
              (λ (k v)
                (cons (partial-dom-contract k)
                      (partial-rng-contract v))))))))))
  
  #:property name-prop (λ (ctc) (build-compound-type-name
                                 'hash/c (immutable-hash/c-dom ctc) (immutable-hash/c-rng ctc)
                                 '#:immutable #t))
  #:property stronger-prop
  (λ (this that)
    #f))

(define (∃-proj ctc)
  (let ([in (∃/c-in ctc)]
        [out (∃/c-out ctc)]
        [pred? (∃/c-pred? ctc)])
  (λ (pos-blame neg-blame src-info orig-str positive-position?)
    (if positive-position?
	in
        (λ (val)
          (if (pred? val)
              (out val)
              (raise-contract-error val src-info pos-blame orig-str 
                                    "non-polymorphic value: ~e"
                                    val)))))))

(define-struct ∃/c (in out pred? name)
  #:omit-define-syntaxes
  #:property proj-prop ∃-proj
  #:property name-prop (λ (ctc) (∃/c-name ctc))
  #:property first-order-prop
  (λ (ctc) (λ (x) #t)) ;; ???
      
  #:property stronger-prop
  (λ (this that) #f))

(define-struct ∃ ())

(define (new-∃/c raw-name)
  (define name (string->symbol (format "~a/∃" raw-name)))
  (define-values (struct-type constructor predicate accessor mutator)
    (make-struct-type name struct:∃ 1 0))
  (make-∃/c constructor (λ (x) (accessor x 0)) predicate raw-name))
