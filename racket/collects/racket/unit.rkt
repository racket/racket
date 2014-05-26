#lang racket/base

;; Library for first-class components with recursive linking

(require (for-syntax racket/base
                     syntax/boundmap
                     syntax/context
                     syntax/kerncase
                     syntax/name
                     syntax/parse
                     syntax/struct
                     racket/struct-info
                     syntax/stx
                     syntax/location
                     "private/unit-contract-syntax.rkt"
                     "private/unit-compiletime.rkt"
                     "private/unit-syntax.rkt"))

(require racket/block
         racket/unsafe/undefined
         racket/contract/base
         racket/contract/region
         racket/stxparam
         syntax/location
         "private/unit-contract.rkt"
         "private/unit-keywords.rkt"
         "private/unit-runtime.rkt"
         "private/unit-utils.rkt"
         (rename-in racket/private/struct [struct struct~]))

(provide define-signature-form open
         define-signature provide-signature-elements
         only except rename import export prefix link tag init-depend extends contracted
         define-values-for-export
         unit?
         (rename-out [:unit unit]) define-unit 
         compound-unit define-compound-unit compound-unit/infer define-compound-unit/infer
         invoke-unit define-values/invoke-unit
         invoke-unit/infer define-values/invoke-unit/infer
         unit-from-context define-unit-from-context
         define-unit-binding
         unit/new-import-export define-unit/new-import-export
         unit/s define-unit/s
         unit/c define-unit/contract
         (rename-out [struct~r/ctc struct/ctc]))

(define-syntax/err-param (define-signature-form stx)
  (syntax-case stx ()
    ((_ (name arg) . val)
     (begin
       (check-id #'name)
       (check-id #'arg)
       #'(define-syntax name
           (make-set!-transformer
            (make-signature-form (λ (arg) . val))))))
    ((_ . l)
     (let ((l (checked-syntax->list stx)))
       (unless (>= 3 (length l))
         (raise-stx-err 
          (format "expected syntax matching (~a (id id) expr ...)"
                  (syntax-e (stx-car stx)))))
       (unless (= 2 (length (checked-syntax->list (car l))))
         (raise-stx-err
          "expected syntax matching (identifier identifier)"
          (car l)))))))

(module+ compat
  ;; export only for compatibility with `mzlib/unit`
  (provide (protect-out struct) struct/ctc)

  (define-signature-form (struct stx)
    (parameterize ((error-syntax stx))
      (syntax-case stx ()
        ((_ name (field ...) . omissions)
         (let ([omit-selectors #f]
               [omit-setters #f]
               [omit-constructor #f]
               [omit-type #f])
           (define (remove-ctor&type-name l)
             (cond
              ((and omit-constructor omit-type)
               (cddr l))
              (omit-type
               (cdr l))
              (omit-constructor
               (cons (car l) (cddr l)))
              (else
               l)))
           (define (remove-ctor&type-info l)
             (define new-type
               (if omit-type
                   #f
                   (cadr l)))
             (define new-ctor
               (if omit-constructor
                   #f
                   (caddr l)))
             (cons (car l)
                   (cons new-type
                         (cons new-ctor
                               (cdddr l)))))
           (check-id #'name)
           (for-each check-id (syntax->list #'(field ...)))
           (for-each
            (lambda (omission)
              (cond
               ((and (identifier? omission)
                     (free-identifier=? omission #'-selectors))
                (set! omit-selectors #t))
               ((and (identifier? omission)
                     (free-identifier=? omission #'-setters))
                (set! omit-setters #t))
               ((and (identifier? omission)
                     (free-identifier=? omission #'-constructor))
                (set! omit-constructor #t))
               ((and (identifier? omission)
                     (free-identifier=? omission #'-type))
                (set! omit-type #t))
               (else
                (raise-stx-err
                 "expected \"-selectors\" or \"-setters\" or \"-constructor\" or \"-type\""
                 omission))))
            (checked-syntax->list #'omissions))
           (cons
            #`(define-syntaxes (name)
                #,(remove-ctor&type-info
                   (build-struct-expand-info
                    #'name (syntax->list #'(field ...))
                    omit-selectors omit-setters
                    #f '(#f) '(#f))))
            (remove-ctor&type-name
             (build-struct-names #'name (syntax->list #'(field ...))
                                 omit-selectors omit-setters #f)))))
        ((_ name (x . y) . omissions)
         ;; Will fail
         (checked-syntax->list (stx-car (stx-cdr (stx-cdr stx)))))
        ((_ name fields . omissions)
         (raise-stx-err "expected syntax matching (identifier ...)" #'fields))
        ((_ name)
         (raise-stx-err "missing fields"))
        ((_)
         (raise-stx-err "missing name and fields"))))))

(begin-for-syntax
 (define-struct self-name-struct-info (id)
   #:super struct:struct-info
   #:property prop:procedure (lambda (me stx)
                               (syntax-case stx ()
                                 [(_ arg ...) (datum->syntax
                                               stx
                                               (cons ((self-name-struct-info-id me))
                                                     #'(arg ...))
                                               stx
                                               stx)]
                                 [_ (let ([id ((self-name-struct-info-id me))])
                                      (datum->syntax id
                                                      (syntax-e id)
                                                      stx
                                                      stx))]))
   #:omit-define-syntaxes))

(define-for-syntax option-keywords
  "#:mutable, #:constructor-name, #:extra-constructor-name, #:omit-constructor, #:omit-define-syntaxes, or #:omit-define-values")

;; Replacement `struct' signature form for `scheme/unit':
(define-for-syntax (do-struct~ stx extra-make?)
  (syntax-case stx ()
    ((_ name (field ...) opt ...)
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f
                             "expected an identifier to name the structure type"
                             stx
                             #'name))
       (for-each (lambda (field)
                   (unless (identifier? field)
                     (syntax-case field ()
                       [(id #:mutable)
                        (identifier? #'id)
                        'ok]
                       [_
                        (raise-syntax-error #f
                                            "bad field specification"
                                            stx
                                            field)])))
                 (syntax->list #'(field ...)))
       (let*-values ([(no-ctr? mutable? no-stx? no-rt? opt-cname)
                      (let loop ([opts (syntax->list #'(opt ...))]
                                 [no-ctr? #f]
                                 [mutable? #f]
                                 [no-stx? #f]
                                 [no-rt? #f]
                                 [cname #f])
                        (if (null? opts)
                            (values no-ctr? mutable? no-stx? no-rt? cname)
                            (let ([opt (car opts)])
                              (case (syntax-e opt)
                                [(#:constructor-name #:extra-constructor-name)
                                 (if cname
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (if (null? (cdr opts))
                                         (raise-syntax-error #f
                                                             "missing identifier after option"
                                                             stx
                                                             opt)
                                         (if (identifier? (cadr opts))
                                             (loop (cddr opts) #f mutable? no-stx? no-rt?
                                                   (if (eq? (syntax-e opt) '#:extra-constructor-name)
                                                       (list (cadr opts))
                                                       (cadr opts)))
                                             (raise-syntax-error #f
                                                                 "not an identifier for a constructor name"
                                                                 stx
                                                                 (cadr opts)))))]
                                [(#:omit-constructor)
                                 (if no-ctr?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) #t mutable? no-stx? no-rt? cname))]
                                [(#:mutable)
                                 (if mutable?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) no-ctr? #t no-stx? no-rt? cname))]
                                [(#:omit-define-syntaxes)
                                 (if no-stx?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) no-ctr? mutable? #t no-rt? cname))]
                                [(#:omit-define-values)
                                 (if no-rt?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) no-ctr? mutable? no-stx? #t cname))]
                                [else
                                 (raise-syntax-error #f
                                                     (string-append
                                                      "expected a keyword to specify option: "
                                                      option-keywords)
                                                     stx
                                                     opt)]))))]
                     [(def-cname) (cond
                                   [opt-cname (if (pair? opt-cname)
                                                  (car opt-cname)
                                                  opt-cname)]
                                   [extra-make? #f]
                                   [else (car (generate-temporaries #'(name)))])]
                     [(cname) (cond
                               [opt-cname (if (pair? opt-cname)
                                              (cons def-cname #'name)
                                              (cons opt-cname opt-cname))]
                               [extra-make? #f]
                               [else (cons def-cname #'name)])]
                     [(self-ctr?) (and cname (bound-identifier=? #'name (cdr cname)))])
         (cons
          #`(define-syntaxes (name)
              #,(let ([e (build-struct-expand-info
                          #'name (syntax->list #'(field ...))
                          #f (not mutable?)
                          #f '(#f) '(#f)
                          #:omit-constructor? no-ctr?
                          #:constructor-name def-cname)])
                  (if self-ctr?
                      #`(make-self-name-struct-info 
                         (lambda () #,e)
                         (lambda () (quote-syntax #,def-cname)))
                      e)))
          (let ([names (build-struct-names #'name (syntax->list #'(field ...))
                                           #f (not mutable?)
                                           #:constructor-name def-cname)])
            (cond
             [no-ctr? (cons (car names) (cddr names))]
             [self-ctr? (cons #`(define-values-for-export (#,def-cname) name) 
                              names)]
             [else names]))))))
    ((_ name fields opt ...)
     (raise-syntax-error #f
                         "bad syntax; expected a parenthesized sequence of fields"
                         stx
                         #'fields))
    ((_ name)
     (raise-syntax-error #f
                         "bad syntax; missing fields"
                         stx))
    ((_)
     (raise-syntax-error #f
                         "missing name and fields"
                         stx))))

(module+ compat
  ;; export only for compatibility with `mzlib/unit`
  (provide (protect-out struct~s) struct~r)

  (define-signature-form (struct~s stx)
    (do-struct~ stx #t)))

;; this binding is used by `racket/unit` for `define-signature`
(define-signature-form (struct~r stx)
  (do-struct~ stx #f))

(define-signature-form (struct/ctc stx)
  (parameterize ((error-syntax stx))
    (syntax-case stx ()
      ((_ name ([field ctc] ...) . omissions)
       (let ([omit-selectors #f]
             [omit-setters #f]
             [omit-constructor #f]
             [omit-type #f])
         (define (remove-ctor&type-info l)
           (define new-type
             (if omit-type
                 #f
                 (cadr l)))
           (define new-ctor
             (if omit-constructor
                 #f
                 (caddr l)))
           (cons (car l)
                 (cons new-type
                       (cons new-ctor
                             (cdddr l)))))
         (define (add-contracts l)
           (let* ([pred (caddr l)]
                  [ctor-ctc #`(-> ctc ... #,pred)]
                  [pred-ctc #`(-> any/c boolean?)]
                  [field-ctcs (apply append
                                     (map (λ (c)
                                            (append (if omit-selectors
                                                        null
                                                        (list #`(-> #,pred #,c)))
                                                    (if omit-setters
                                                        null
                                                        (list #`(-> #,pred #,c void?)))))
                                          (syntax->list #'(ctc ...))))])
             (list* (car l)
                    (list (cadr l) ctor-ctc)
                    (list pred pred-ctc)
                    (map list (cdddr l) field-ctcs))))
         (check-id #'name)
         (for-each check-id (syntax->list #'(field ...)))
         (for-each
          (lambda (omission)
            (cond
              ((and (identifier? omission)
                    (free-identifier=? omission #'-selectors))
               (set! omit-selectors #t))
              ((and (identifier? omission)
                    (free-identifier=? omission #'-setters))
               (set! omit-setters #t))
              ((and (identifier? omission)
                    (free-identifier=? omission #'-constructor))
               (set! omit-constructor #t))
              ((and (identifier? omission)
                    (free-identifier=? omission #'-type))
               (set! omit-type #t))
              (else
               (raise-stx-err
                "expected \"-selectors\" or \"-setters\" or \"-constructor\" or \"-type\""
                omission))))
          (checked-syntax->list #'omissions))
         (cons
          #`(define-syntaxes (name)
              #,(remove-ctor&type-info
                 (build-struct-expand-info
                  #'name (syntax->list #'(field ...))
                  omit-selectors omit-setters
                  #f '(#f) '(#f))))
          (let* ([res (add-contracts
                       (build-struct-names #'name (syntax->list #'(field ...))
                                           omit-selectors omit-setters #f))]
                 [cpairs (cons 'contracted (if omit-constructor (cddr res) (cdr res)))])
            (if omit-type
                (list cpairs)
                (list (car res) cpairs))))))
      ((_ name (x . y) . omissions)
       ;; Will fail
       (checked-syntax->list (stx-car (stx-cdr (stx-cdr stx)))))
      ((_ name fields . omissions)
       (raise-stx-err "expected syntax matching (identifier ...)" #'fields))
      ((_ name)
       (raise-stx-err "missing fields"))
      ((_)
       (raise-stx-err "missing name and fields")))))

;; Replacement struct/ctc form for `scheme/unit':
(define-for-syntax (do-struct~/ctc stx extra-make?)
  (syntax-case stx ()
    ((_ name ([field ctc] ...) opt ...)
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f
                             "expected an identifier to name the structure type"
                             stx
                             #'name))
       (for-each (lambda (field)
                   (unless (identifier? field)
                     (syntax-case field ()
                       [(id #:mutable)
                        (identifier? #'id)
                        'ok]
                       [_
                        (raise-syntax-error #f
                                            "bad field specification"
                                            stx
                                            field)])))
                 (syntax->list #'(field ...)))
       (let*-values ([(no-ctr? mutable? no-stx? no-rt? opt-cname)
                      (let loop ([opts (syntax->list #'(opt ...))]
                                 [no-ctr? #f]
                                 [mutable? #f]
                                 [no-stx? #f]
                                 [no-rt? #f]
                                 [cname #f])
                        (if (null? opts)
                            (values no-ctr? mutable? no-stx? no-rt? cname)
                            (let ([opt (car opts)])
                              (case (syntax-e opt)
                                [(#:constructor-name #:extra-constructor-name)
                                 (if cname
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (if (null? (cdr opts))
                                         (raise-syntax-error #f
                                                             "missing identifier after option"
                                                             stx
                                                             opt)
                                         (if (identifier? (cadr opts))
                                             (loop (cddr opts) #f mutable? no-stx? no-rt?
                                                   (if (eq? (syntax-e opt) '#:extra-constructor-name)
                                                       (list (cadr opts))
                                                       (cadr opts)))
                                             (raise-syntax-error #f
                                                                 "not an identifier for a constructor name"
                                                                 stx
                                                                 (cadr opts)))))]
                                [(#:omit-constructor)
                                 (if no-ctr?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) #t mutable? no-stx? no-rt? cname))]
                                [(#:mutable)
                                 (if mutable?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) no-ctr? #t no-stx? no-rt? cname))]
                                [(#:omit-define-syntaxes)
                                 (if no-stx?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) no-ctr? mutable? #t no-rt? cname))]
                                [(#:omit-define-values)
                                 (if no-rt?
                                     (raise-syntax-error #f
                                                         "redundant option"
                                                         stx
                                                         opt)
                                     (loop (cdr opts) no-ctr? mutable? no-stx? #t cname))]
                                [else
                                 (raise-syntax-error #f
                                                     (string-append
                                                      "expected a keyword to specify option: "
                                                      option-keywords)
                                                     stx
                                                     opt)]))))]
                     [(def-cname) (cond
                                   [opt-cname (if (pair? opt-cname)
                                                  (car opt-cname)
                                                  opt-cname)]
                                   [extra-make? #f]
                                   [else (car (generate-temporaries #'(name)))])]
                     [(cname) (cond
                               [opt-cname (if (pair? opt-cname)
                                              (cons def-cname #'name)
                                              (cons def-cname def-cname))]
                               [extra-make? #f]
                               [else (cons def-cname #'name)])]
                     [(self-ctr?) (and cname (bound-identifier=? #'name (cdr cname)))])
         (define (add-contracts l)
           (let* ([pred (caddr l)]
                  [ctor-ctc #`(-> ctc ... #,pred)]
                  [pred-ctc #'(-> any/c boolean?)]
                  [field-ctcs
                   (apply append
                          (map (λ (f c)
                                  (cons #`(-> #,pred #,c)
                                        (if (and (not mutable?)
                                                 (not (pair? (syntax-e f))))
                                            null
                                            #`(-> #,pred #,c void?))))
                               (syntax->list #'(field ...))
                               (syntax->list #'(ctc ...))))])
             (list* (car l)
                    (list (cadr l) ctor-ctc)
                    (list pred pred-ctc)
                    (map list (cdddr l) field-ctcs))))
         (cons
          #`(define-syntaxes (name)
              #,(let ([e (build-struct-expand-info
                          #'name (syntax->list #'(field ...))
                          #f (not mutable?)
                          #f '(#f) '(#f)
                          #:omit-constructor? no-ctr?
                          #:constructor-name def-cname)])
                  (if self-ctr?
                      #`(make-self-name-struct-info 
                         (lambda () #,e)
                         (lambda () (quote-syntax #,def-cname)))
                      e)))
          (let* ([names (add-contracts
                         (build-struct-names #'name (syntax->list #'(field ...))
                                             #f (not mutable?)
                                             #:constructor-name def-cname))]
                 [cpairs (cons 'contracted
                               (cond
                                [no-ctr? (cddr names)]
                                [else (cdr names)]))]
                 [l (list (car names) cpairs)])
            (if self-ctr?
                (cons #`(define-values-for-export (#,def-cname) name) l)
                l))))))
    ((_ name fields opt ...)
     (raise-syntax-error #f
                         "bad syntax; expected a parenthesized sequence of fields"
                         stx
                         #'fields))
    ((_ name)
     (raise-syntax-error #f
                         "bad syntax; missing fields"
                         stx))
    ((_)
     (raise-syntax-error #f
                         "missing name and fields"
                         stx))))
(module+ compat
  ;; export only for compatibility with `mzlib/unit`
  (provide (protect-out struct~s/ctc) struct~r/ctc)
  
  (define-signature-form (struct~s/ctc stx)
    (do-struct~/ctc stx #t)))

(define-signature-form (struct~r/ctc stx)
  (do-struct~/ctc stx #f))

;; build-val+macro-defs : sig -> (list syntax-object^3)
(define-for-syntax (build-val+macro-defs sig)
  (if (and (null? (cadr sig))
           (null? (caddr sig)))
      ;; No renames needed; this shortcut avoids
      ;; an explosion of renamings, especially with chains
      ;; of `open':
      (list #'(() (values)) #'() #'())
      ;; Renames and macros needes:
      (with-syntax ([(((int-ivar . ext-ivar) ...)
                      ((((int-vid . ext-vid) ...) . vbody) ...)
                      ((((int-sid . ext-sid) ...) . sbody) ...)
                      _
                      _)
                     (map-sig (lambda (x) x)
                              (make-syntax-introducer)
                              sig)])
        (list
         #'((ext-ivar ... ext-vid ... ... ext-sid ... ...)
            (make-rename-transformers
             (quote-syntax
              (int-ivar ...
                        int-vid ... ...
                        int-sid ... ...))))
         #'(((int-sid ...) sbody) ...)
         #'(((int-vid ...) vbody) ...)))))

;; build-post-val-defs : sig -> (list syntax-object)
(define-for-syntax (build-post-val-defs sig)
  (with-syntax ([(((int-ivar . ext-ivar) ...)
                  ((((int-vid . ext-vid) ...) . _) ...)
                  ((((int-sid . ext-sid) ...) . _) ...)
                  _
                  (((post-id ...) . post-rhs) ...))
                 (map-sig (lambda (x) x)
                          (make-syntax-introducer)
                          sig)])
    (list
     #'((ext-ivar ... ext-vid ... ... ext-sid ... ...)
        (make-rename-transformers
         (quote-syntax
          (int-ivar ...
                    int-vid ... ...
                    int-sid ... ...))))
     #'(post-rhs ...))))

;; Using `make-rename-transformers' helps improve sharing in
;; a syntax-quoted list of identifiers, although it risks
;; losting certificates as the list is broken apart; since the
;; identifiers are bound at the same point that the rename
;; transformer is introduced, certificate loss should be ok.
(define-for-syntax (make-rename-transformers ids)
  (apply values
         (map 
          make-rename-transformer
          (syntax->list ids))))

(define-signature-form (open stx)
  (define (build-sig-elems sig)
    (map (λ (p c)
           (if c #`(contracted [#,(car p) #,c]) (car p)))
         (car sig)
         (cadddr sig)))
  (parameterize ([error-syntax stx])
    (syntax-case stx ()
      ((_ export-spec)
       (let ([sig (process-spec #'export-spec)])
         (with-syntax (((sig-elem ...)
                        (build-sig-elems sig))
                       ((renames
                         (((mac-name ...) mac-body) ...) 
                         (((val-name ...) val-body) ...))
                        (build-val+macro-defs sig))
                       ((((e-post-id ...) . _) ...) (list-ref sig 4))
                       ((post-renames (e-post-rhs ...))
                        (build-post-val-defs sig)))
           (syntax->list
            #'(sig-elem ...
               (define-syntaxes . renames)
               (define-syntaxes (mac-name ...) mac-body) ...
               (define-values (val-name ...) val-body) ...
               (define-values-for-export (e-post-id ...)
                 (let-syntaxes (post-renames) e-post-rhs))
               ...)))))
      (_
       (raise-stx-err (format "must match (~a export-spec)"
                              (syntax-e (stx-car stx))))))))

(define-signature-form (define-values-for-export stx)
  (raise-syntax-error #f "internal error" stx))

(define-for-syntax (introduce-def d)
  (cons (map syntax-local-introduce (car d))
        (syntax-local-introduce (cdr d))))

;; build-define-syntax : identifier (or/c identifier #f) syntax-object -> syntax-object
(define-for-syntax (build-define-signature sigid super-sigid sig-exprs)
  (unless (or (stx-null? sig-exprs) (stx-pair? sig-exprs))
    (raise-stx-err "expected syntax matching (sig-expr ...)" sig-exprs))
  (let ([ses (checked-syntax->list sig-exprs)])
    (define-values (super-names super-ctimes super-rtimes)
      (if super-sigid
          (let* ([super-sig (lookup-signature super-sigid)]
                 [super-siginfo (signature-siginfo super-sig)])
            (values (siginfo-names super-siginfo)
                    (siginfo-ctime-ids super-siginfo)
                    (map syntax-local-introduce
                         (siginfo-rtime-ids super-siginfo))))
          (values '() '() '())))
    (let loop ((sig-exprs (if super-sigid
                              (cons #`(open #,super-sigid) ses)
                              ses))
               (bindings null)
               (val-defs null)
               (stx-defs null)
               (post-val-defs null)
               (ctcs null))
      (cond
        ((null? sig-exprs)
         (let* ([all-bindings (reverse bindings)]
                [all-val-defs (reverse val-defs)]
                [all-stx-defs (reverse stx-defs)]
                [all-post-val-defs (reverse post-val-defs)]
                [all-ctcs (reverse ctcs)]
                [dup
                 (check-duplicate-identifier
                  (append all-bindings
                          (apply append (map car all-val-defs))
                          (apply append (map car all-stx-defs))))])
           (when dup
             (raise-stx-err "duplicate identifier" dup))
           (with-syntax (((super-rtime ...) super-rtimes)
                         ((super-name ...) super-names)
                         ((var ...) all-bindings)
                         ((ctc ...) all-ctcs)
                         ((((vid ...) . vbody) ...) all-val-defs)
                         ((((sid ...) . sbody) ...) all-stx-defs)
                         ((((pvid ...) . pvbody) ...) all-post-val-defs))
             #`(begin
                 (define signature-tag (gensym))
                 (define-syntax #,sigid
                   (make-set!-transformer
                    (make-signature
                     (make-siginfo (list #'#,sigid #'super-name ...)
                                   (list (quote-syntax signature-tag)
                                         #'super-rtime
                                         ...))
                     (list (quote-syntax var) ...)
                     (list (cons (list (quote-syntax vid) ...)
                                 (quote-syntax vbody))
                           ...)
                     (list (cons (list (quote-syntax sid) ...)
                                 (quote-syntax sbody))
                           ...)
                     (list (cons (list (quote-syntax pvid) ...)
                                 (quote-syntax pvbody))
                           ...)
                     (list #,@(map (lambda (c) 
                                     (if c
                                         #`(quote-syntax #,c)
                                         #'#f))
                                   all-ctcs))
                     (quote-syntax #,sigid))))
                 (define-values ()
                   (begin
                     (λ (var ...)
                       (letrec-syntaxes+values
                           ([(sid ...) sbody] ...) ([(vid ...) vbody] ...)
                         ctc ...
                         (void)))
                     (values)))))))
        (else
         (syntax-case (car sig-exprs) (define-values define-syntaxes contracted)
           (x
            (identifier? #'x)
            (loop (cdr sig-exprs) (cons #'x bindings) val-defs stx-defs post-val-defs (cons #f ctcs)))
           ((x (y z) ...)
            (and (identifier? #'x)
                 (free-identifier=? #'x #'contracted)
                 (andmap identifier? (syntax->list #'(y ...))))
            (loop (cdr sig-exprs)
                  (append (syntax->list #'(y ...)) bindings)
                  val-defs
                  stx-defs
                  post-val-defs
                  (append (syntax->list #'(z ...)) ctcs)))
           ((x . z)
            (and (identifier? #'x)
                 (free-identifier=? #'x #'contracted))
            (raise-syntax-error 
             'define-signature
             "expected a list of [id contract] pairs after the contracted keyword"
             (car sig-exprs)))
           ((x . y)
            (and (identifier? #'x)
                 (or (free-identifier=? #'x #'define-values)
                     (free-identifier=? #'x #'define-syntaxes)
                     (free-identifier=? #'x #'define-values-for-export)))
            (begin
              (check-def-syntax (car sig-exprs))
              (syntax-case #'y ()
                (((name ...) body)
                 (begin
                   (for-each (lambda (id) (check-id id))
                             (syntax->list #'(name ...)))
                   (let ((b #'body))
                     (loop (cdr sig-exprs)
                           bindings
                           (if (free-identifier=? #'x #'define-values)
                               (cons (cons (syntax->list #'(name ...)) b)
                                     val-defs)
                               val-defs)
                           (if (free-identifier=? #'x #'define-syntaxes)
                               (cons (cons (syntax->list #'(name ...)) b)
                                     stx-defs)
                               stx-defs)
                           (if (free-identifier=? #'x #'define-values-for-export)
                               (cons (cons (syntax->list #'(name ...)) b)
                                     post-val-defs)
                               post-val-defs)
                           ctcs)))))))
           ((x . y)
            (let ((trans 
                   (set!-trans-extract
                    (syntax-local-value
                     ;; redirect struct~ to struct~r
                     (if (free-identifier=? #'x #'struct~)
                         #'struct~r
                         (syntax-local-introduce #'x))
                     (lambda ()
                       (raise-stx-err "unknown signature form" #'x))))))
              (unless (signature-form? trans)
                (raise-stx-err "not a signature form" #'x))
              (let ((results ((signature-form-f trans) (car sig-exprs))))
                (unless (list? results)
                  (raise-stx-err
                   (format "expected list of results from signature form, got ~e" results)
                   (car sig-exprs)))
                (loop (append results (cdr sig-exprs))
                      bindings
                      val-defs
                      stx-defs
                      post-val-defs
                      ctcs))))
           (x (raise-stx-err 
               "expected either an identifier or signature form"
               #'x))))))))


(define-syntax/err-param (define-signature stx)
  (syntax-case stx (extends)
    ((_ sig-name sig-exprs)
     (begin
       (check-id #'sig-name)
       (build-define-signature #'sig-name #f #'sig-exprs)))
    ((_ sig-name extends super-name sig-exprs)
     (begin
       (check-id #'sig-name)
       (check-id #'super-name)
       (build-define-signature #'sig-name #'super-name #'sig-exprs)))
    (_
     (begin
       (checked-syntax->list stx)
       (raise-stx-err
        (format "expected syntax matching (~a identifier (sig-expr ...)) or (~a identifier extends identifier (sig-expr ...))"
                (syntax-e (stx-car stx)) (syntax-e (stx-car stx))))))))

(define-for-syntax (signature->identifiers sigids)
  (define provide-tagged-sigs (map process-tagged-import sigids))
  (define provide-sigs (map caddr provide-tagged-sigs))
  (map sig-int-names provide-sigs))

(define-syntax/err-param (provide-signature-elements stx)
  (syntax-case stx ()
    ((_ . p)
     (let* ((sigs (checked-syntax->list #'p))
            (nameses (signature->identifiers sigs))
            ;; Export only the names that would be visible to uses
            ;;  with the same lexical context as p. Otherwise, we
            ;;  can end up with collisions with renamings that are
            ;;  symbolically the same, such as those introduced by
            ;;  `open'.
            (nameses (map (lambda (sig names)
                            (filter (lambda (name)
                                      (bound-identifier=?
                                       name
                                       (datum->syntax sig (syntax-e name))))
                                    names))
                          sigs nameses))
            (names (apply append nameses))
            (dup (check-duplicate-identifier names)))
       (when dup
         (raise-stx-err (format "duplicate binding for ~.s" (syntax-e dup))))
       (quasisyntax/loc stx
         (provide #,@names))))))

;; A unit is 
;; - (unit (import import-spec ...) (export export-spec ...) unit-body-expr ...)

(define-for-syntax (localify exp def-ctx)
  (cadr (syntax->list
         (local-expand #`(stop #,exp)
                       'expression
                       (list #'stop)
                       def-ctx))))

(define-for-syntax (tagged-sigid->tagged-siginfo x)
  (cons (car x)
        (signature-siginfo (lookup-signature (cdr x)))))

(define-for-syntax (make-import-unboxing var renamings loc ctc)
  (if ctc
      (with-syntax ([ctc-stx (syntax-property ctc 'inferred-name var)])
        (quasisyntax/loc (error-syntax)
          (quote-syntax (let ([v/c (#,loc)])
                          (if (pair? v/c)
                              (contract (let-syntax #,renamings ctc-stx) (car v/c) (cdr v/c)
                                        (current-contract-region)
                                        (quote #,var) (quote-srcloc #,var))
                              (error 'unit "contracted import ~a used before definition"
                                     (quote #,(syntax->datum var))))))))
      (quasisyntax/loc (error-syntax)
        (quote-syntax (#,loc)))))

;; build-unit : syntax-object -> 
;;             (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a unit expression.  stx must be
;; such that it passes check-unit-syntax.
;; The two additional values are the identifiers of the unit's import and export
;; signatures
(define-for-syntax (build-unit stx)
  (syntax-case stx (import export init-depend)
    (((import i ...)
      (export e ...)
      (init-depend id ...)
      . body)
     
     (let* ([d (syntax->list #'(id ...))]
            [dep-tagged-sigids (map check-tagged-id d)]
            [dep-tagged-siginfos 
             (map tagged-sigid->tagged-siginfo dep-tagged-sigids)])
       
       (define-values (isig tagged-import-sigs import-tagged-infos 
                            import-tagged-sigids import-sigs)
         (process-unit-import #'(i ...)))
       
       (define-values (esig tagged-export-sigs export-tagged-infos 
                            export-tagged-sigids export-sigs)
         (process-unit-export #'(e ...)))
       
       (check-duplicate-sigs import-tagged-infos isig dep-tagged-siginfos d)
       
       (check-duplicate-subs export-tagged-infos esig)
       
       (check-unit-ie-sigs import-sigs export-sigs)
       
       (with-syntax ((((dept . depr) ...)
                      (map
                       (lambda (tinfo)
                         (cons (car tinfo)
                               (syntax-local-introduce (car (siginfo-rtime-ids (cdr tinfo))))))
                       dep-tagged-siginfos))
                     [((renames (mac ...) (val ...)) ...)
                      (map build-val+macro-defs import-sigs)]
                     [(((int-ivar . ext-ivar) ...) ...) (map car import-sigs)]
                     [(((int-evar . ext-evar) ...) ...) (map car export-sigs)]
                     [((((e-post-id ...) . _) ...) ...) (map (lambda (s) (list-ref s 4)) export-sigs)]
                     [((post-renames (e-post-rhs ...)) ...) (map build-post-val-defs export-sigs)]
                     [((iloc ...) ...)
                      (map (lambda (x) (generate-temporaries (car x))) import-sigs)]
                     [((eloc ...) ...)
                      (map (lambda (x) (generate-temporaries (car x))) export-sigs)]
                     [((ectc ...) ...)
                      (map (λ (sig)
                             (map (λ (ctc)
                                    (if ctc
                                        (cons 'contract ctc)
                                        #f))
                                  (cadddr sig))) export-sigs)]
                     [((import-key import-super-keys ...) ...)
                      (map tagged-info->keys import-tagged-infos)]
                     [((export-key ...) ...)
                      (map tagged-info->keys export-tagged-infos)]
                     [(import-name ...)
                      (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                           import-tagged-infos)]
                     [(export-name ...)
                      (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                           export-tagged-infos)]
                     [name (syntax-local-infer-name (error-syntax))]
                     [(icount ...) (map
                                    (lambda (import) (length (car import)))
                                    import-sigs)])
         (values 
          (quasisyntax/loc (error-syntax)
            (make-unit
             'name
             (vector-immutable (cons 'import-name
                                     (vector-immutable import-key import-super-keys ...)) ...)
             (vector-immutable (cons 'export-name 
                                     (vector-immutable export-key ...)) ...)
             (list (cons 'dept depr) ...)
             (syntax-parameterize ([current-contract-region (lambda (stx) #'(quote (unit name)))])
               (lambda ()
                 (let ([eloc (box unsafe-undefined)] ... ...)
                   (values 
                    (lambda (import-table)
                      (let-values ([(iloc ...)
                                    (vector->values (hash-ref import-table import-key) 0 icount)]
                                   ...)
                        (letrec-syntaxes (#,@(map (lambda (ivs e-ivs ils ics)
                                                    (with-syntax ([renamings 
                                                                   (map (λ (ev iv)
                                                                          #`(#,ev 
                                                                             (make-rename-transformer
                                                                              (quote-syntax #,iv))))
                                                                        (syntax->list e-ivs)
                                                                        (syntax->list ivs))])
                                                      (quasisyntax/loc (error-syntax)
                                                        [#,ivs
                                                         (make-id-mappers
                                                          #,@(map (lambda (iv l c)
                                                                    (make-import-unboxing iv #'renamings l c))
                                                                  (syntax->list ivs)
                                                                  (syntax->list ils)
                                                                  ics))])))
                                                  (syntax->list #'((int-ivar ...) ...))
                                                  (syntax->list #'((ext-ivar ...) ...))
                                                  (syntax->list #'((iloc ...) ...))
                                                  (map cadddr import-sigs)))
                                         (letrec-syntaxes+values (renames ...
                                                                  mac ... ...)
                                           (val ... ...)
                                           (unit-body #,(error-syntax)
                                                      (int-ivar ... ...)
                                                      (int-evar ... ...)
                                                      (eloc ... ...)
                                                      (ectc ... ...)
                                                      (begin . body)
                                                      (define-values (e-post-id ...) 
                                                        (letrec-syntaxes+values (post-renames ...) ()
                                                          e-post-rhs)) ... ...)))))
                    (unit-export ((export-key ...)
                                  (vector-immutable (λ () (check-not-unsafe-undefined (unbox eloc) 'int-evar))
                                                    ...))
                                 ...)))))))
          import-tagged-sigids
          export-tagged-sigids
          dep-tagged-sigids))))))

(define-syntax/err-param (:unit stx)
  (syntax-case stx ()
    ((_ . x)
     (begin
       (let-values (((u x y z) (build-unit (check-unit-syntax #'x))))
         u)))))

(define-syntax (unit-body stx)
  (syntax-case stx ()
    ((_ err-stx ivars evars elocs ectcs body ...)
     (parameterize ((error-syntax #'err-stx))
       (let* ([expand-context (generate-expand-context)]
              [def-ctx (syntax-local-make-definition-context)]
              [stop-list
               (append
                (kernel-form-identifier-list)
                (syntax->list #'ivars))]
              [definition?
                (lambda (id)
                  (and (identifier? id)
                       (or (free-identifier=? id (quote-syntax define-values))
                           (free-identifier=? id (quote-syntax define-syntaxes)))))]
              [expanded-body
               (let expand-all ((defns&exprs (syntax->list #'(body ...))))
                 ;; Also lifted from Matthew, to expand the body enough
                 (apply
                  append
                  (map
                   (lambda (defn-or-expr)
                     (let ([defn-or-expr
                             (local-expand
                              defn-or-expr
                              expand-context
                              stop-list
                              def-ctx)])
                       (syntax-case defn-or-expr (begin define-values define-syntaxes)
                         [(begin . l)
                          (let ([l (parameterize ((error-syntax defn-or-expr))
                                     (checked-syntax->list #'l))])
                            (expand-all (map (lambda (s)
                                               (syntax-track-origin s defn-or-expr #'begin))
                                             l)))]
                         [(define-syntaxes (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (with-syntax ([rhs (local-transformer-expand
                                              #'rhs
                                              'expression
                                              null)])
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
                            (list #'(define-syntaxes (id ...) rhs)))]
                         [(define-values (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (begin
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f def-ctx)
                            (list defn-or-expr))]
                         [else (list defn-or-expr)])))
                   defns&exprs)))]
              ;; Get all the defined names, sorting out variable definitions
              ;; from syntax definitions.
              [defined-names-table
                (let ((table (make-bound-identifier-mapping)))
                  (for-each
                   (lambda (defn-or-expr)
                     (syntax-case defn-or-expr ()
                       ((dv . rest)
                        (definition? #'dv)
                        (begin
                          (check-def-syntax defn-or-expr)
                          (syntax-case #'rest ()
                            [((id ...) expr)
                             (for-each 
                              (lambda (id)
                                (when (bound-identifier-mapping-get table id (lambda () #f))
                                  (raise-stx-err "variable defined twice" id))
                                (bound-identifier-mapping-put!
                                 table id 
                                 (make-var-info (free-identifier=? #'dv (quote-syntax define-syntaxes))
                                                #f
                                                id
                                                #f)))
                              (syntax->list #'(id ...)))]
                            [_ (void)])))
                       [_ (void)]))
                   expanded-body)
                  table)])
         (internal-definition-context-seal def-ctx)
         
         ;; Mark exported names and
         ;; check that all exported names are defined (as var):
         (for-each
          (lambda (name loc ctc)
            (let ([v (bound-identifier-mapping-get defined-names-table
                                                   name
                                                   (lambda () #f))])
              (unless v
                (raise-stx-err (format "undefined export ~a" (syntax-e name))))
              (when (var-info-syntax? v)
                (raise-stx-err "cannot export syntax from a unit" name))
              (set-var-info-exported?! v loc)
              (when (pair? (syntax-e ctc))
                (set-var-info-ctc! v (localify (cdr (syntax-e ctc)) def-ctx)))))
          (syntax->list (localify #'evars def-ctx))
          (syntax->list #'elocs)
          (syntax->list #'ectcs))
         
         ;; Check that none of the imports are defined
         (for-each
          (lambda (i)
            (let ((defid (bound-identifier-mapping-get defined-names-table
                                                       i
                                                       (lambda () #f))))
              (when defid
                (raise-stx-err
                 "definition for imported identifier"
                 (var-info-id defid)))))
          (syntax->list (localify #'ivars def-ctx)))
         
         (let ([marker (lambda (id) ((make-syntax-introducer) (datum->syntax #f (syntax-e id))))])
           (with-syntax ([(defn-or-expr ...)
                          (apply append
                                 (map (λ (defn-or-expr)
                                        (syntax-case defn-or-expr (define-values)
                                          [(define-values (id ...) body)
                                           (let* ([ids (syntax->list #'(id ...))]
                                                  [tmps (map marker ids)]
                                                  [do-one
                                                   (λ (id tmp)
                                                     (let ([var-info (bound-identifier-mapping-get
                                                                      defined-names-table
                                                                      id)])
                                                       (cond
                                                         [(var-info-exported? var-info)
                                                          =>
                                                          (λ (export-loc)
                                                            (let ([ctc (var-info-ctc var-info)])
                                                              (list (if ctc
                                                                        (quasisyntax/loc defn-or-expr
                                                                          (begin
                                                                            (contract #,ctc #,tmp
                                                                                      (current-contract-region)
                                                                                      'cant-happen
                                                                                      (quote #,id)
                                                                                      (quote-srcloc #,id))
                                                                            (set-box! #,export-loc
                                                                                      (cons #,tmp (current-contract-region)))))
                                                                        (quasisyntax/loc defn-or-expr
                                                                          (set-box! #,export-loc #,tmp)))
                                                                    (quasisyntax/loc defn-or-expr
                                                                      (define-syntax #,id 
                                                                        (make-id-mapper (quote-syntax #,tmp)))))))]
                                                         [else (list (quasisyntax/loc defn-or-expr
                                                                       (define-syntax #,id
                                                                         (make-rename-transformer (quote-syntax #,tmp)))))])))])
                                             (cons (quasisyntax/loc defn-or-expr
                                                     (define-values #,tmps body))
                                                   (apply append (map do-one ids tmps))))]
                                          [else (list defn-or-expr)]))
                                      expanded-body))])
             #'(block defn-or-expr ...))))))))

(define-for-syntax (redirect-imports/exports import?)
  (lambda (table-stx
           import-tagged-infos
           import-sigs
           target-import-tagged-infos
           target-import-sigs)
    (define def-table (make-bound-identifier-mapping))
    (define ctc-table (make-bound-identifier-mapping))
    (define sig-table (make-bound-identifier-mapping))
    (for-each
     (lambda (tagged-info sig)
       (define v
         #`(hash-ref #,table-stx #,(car (tagged-info->keys tagged-info))))
       (for-each
        (lambda (int/ext-name index ctc)
          (bound-identifier-mapping-put! def-table
                                         (car int/ext-name)
                                         #`(check-not-unsafe-undefined (vector-ref #,v #,index)
                                                                       '#,(car int/ext-name)))
          (bound-identifier-mapping-put! ctc-table
                                         (car int/ext-name)
                                         ctc)
          (bound-identifier-mapping-put! sig-table
                                         (car int/ext-name)
                                         sig))
        (car sig)
        (iota (length (car sig)))
        (cadddr sig)))
     import-tagged-infos
     import-sigs)
    (with-syntax ((((eloc ...) ...) 
                   (map
                    (lambda (target-sig)
                      (map
                       (lambda (target-int/ext-name target-ctc)
                         (let* ([var (car target-int/ext-name)]
                                [vref
                                 (bound-identifier-mapping-get
                                  def-table
                                  var
                                  (lambda ()
                                    (raise-stx-err
                                     (format (if import?
                                                 "identifier ~a is not present in new imports"
                                                 "identifier ~a is not present in old exports")
                                             (syntax-e (car target-int/ext-name))))))]
                                [ctc (bound-identifier-mapping-get ctc-table var)]
                                [rename-bindings (get-member-bindings def-table
                                                                      (bound-identifier-mapping-get sig-table var)
                                                                      #'(current-contract-region))])
                           (with-syntax ([ctc-stx (if ctc (syntax-property
                                                           #`(letrec-syntax #,rename-bindings #,ctc)
                                                           'inferred-name var)
                                                      ctc)])
                             (if target-ctc
                                 #`(λ ()
                                     (cons #,(if ctc
                                                 #`(let ([old-v/c (#,vref)])
                                                     (contract ctc-stx (car old-v/c) 
                                                               (cdr old-v/c) (current-contract-region)
                                                               (quote #,var) (quote-srcloc #,var)))
                                                 #`(#,vref))
                                           (current-contract-region)))
                                 (if ctc
                                     #`(λ ()
                                         (let ([old-v/c (#,vref)])
                                           (contract ctc-stx (car old-v/c) 
                                                     (cdr old-v/c) (current-contract-region)
                                                     (quote #,var) (quote-srcloc #,var))))
                                     vref)))))
                       (car target-sig)
                       (cadddr target-sig)))
                    target-import-sigs))
                  (((export-keys ...) ...) 
                   (map tagged-info->keys target-import-tagged-infos)))
      #`(unit-export ((export-keys ...)
                      (vector-immutable eloc ...)) ...))))

(define-for-syntax redirect-imports (redirect-imports/exports #t))
(define-for-syntax redirect-exports (redirect-imports/exports #f))


;; build-unit/new-import-export : syntax-object -> 
;;             (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a unit expression that changes the import and export signatures
;; of another.  stx must be such that it passes check-unit-syntax.
;; The two additional values are the identifiers of the unit's import and export
;; signatures
(define-for-syntax (build-unit/new-import-export stx)
  (syntax-case stx (import export init-depend)
    (((import i ...)
      (export e ...)
      (init-depend id ...)
      . body)
     
     (let* ([d (syntax->list #'(id ...))]
            [dep-tagged-sigids (map check-tagged-id d)]
            [dep-tagged-siginfos 
             (map tagged-sigid->tagged-siginfo dep-tagged-sigids)])
       (define-values (isig tagged-import-sigs import-tagged-infos 
                            import-tagged-sigids import-sigs)
         (process-unit-import #'(i ...)))
       
       (define-values (esig tagged-export-sigs export-tagged-infos 
                            export-tagged-sigids export-sigs)
         (process-unit-export #'(e ...)))
       
       (check-duplicate-sigs import-tagged-infos isig dep-tagged-siginfos d)
       
       (check-duplicate-subs export-tagged-infos esig)
       
       (check-unit-ie-sigs import-sigs export-sigs)
       
       (syntax-case #'body ()
         ((b) (check-link-line-syntax #'b))
         (() (raise-stx-err "missing unit specification"))
         (_ (raise-stx-err "expects a single unit specification")))
       
       (with-syntax (((((orig-e ...) unit-exp orig-i ...)) #'body))
         (define-values (orig-isig orig-tagged-import-sigs orig-import-tagged-infos 
                                   orig-import-tagged-sigids orig-import-sigs)
           (process-unit-export #'(orig-i ...)))
         
         (define-values (orig-esig orig-tagged-export-sigs orig-export-tagged-infos 
                                   orig-export-tagged-sigids orig-export-sigs)
           (process-unit-import #'(orig-e ...)))
         (with-syntax ((((dept . depr) ...)
                        (map
                         (lambda (tinfo)
                           (cons (car tinfo)
                                 (syntax-local-introduce (car (siginfo-rtime-ids (cdr tinfo))))))
                         dep-tagged-siginfos))
                       [((import-key ...) ...)
                        (map tagged-info->keys import-tagged-infos)]
                       [((export-key ...) ...)
                        (map tagged-info->keys export-tagged-infos)]
                       [((orig-import-key ...) ...)
                        (map tagged-info->keys orig-import-tagged-infos)]
                       [((orig-export-key ...) ...)
                        (map tagged-info->keys orig-export-tagged-infos)]
                       [(import-name ...)
                        (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                             import-tagged-infos)]
                       [(export-name ...)
                        (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                             export-tagged-infos)]
                       [(orig-import-name ...)
                        (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                             orig-import-tagged-infos)]
                       [(orig-export-name ...)
                        (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                             orig-export-tagged-infos)]
                       [name (syntax-local-infer-name (error-syntax))]
                       [form (syntax-e (stx-car (error-syntax)))])
           (values 
            (quasisyntax/loc (error-syntax)
              (let ([unit-tmp unit-exp])
                (check-unit unit-tmp 'form)
                (check-sigs unit-tmp
                            (vector-immutable
                             (cons 'orig-import-name
                                   (vector-immutable orig-import-key ...)) ...)
                            (vector-immutable 
                             (cons 'orig-export-name 
                                   (vector-immutable orig-export-key ...)) ...)
                            'form)
                (make-unit
                 'name
                 (vector-immutable (cons 'import-name
                                         (vector-immutable import-key ...)) ...)
                 (vector-immutable (cons 'export-name 
                                         (vector-immutable export-key ...)) ...)
                 (list (cons 'dept depr) ...)
                 (syntax-parameterize ([current-contract-region (lambda (stx) #'(quote (unit name)))])
                   (lambda ()
                     (let-values ([(unit-fn export-table) ((unit-go unit-tmp))])
                       (values (lambda (import-table)
                                 (unit-fn #,(redirect-imports #'import-table
                                                              import-tagged-infos
                                                              import-sigs
                                                              orig-import-tagged-infos
                                                              orig-import-sigs)))
                               #,(redirect-exports #'export-table
                                                   orig-export-tagged-infos
                                                   orig-export-sigs
                                                   export-tagged-infos
                                                   export-sigs))))))))
            import-tagged-sigids
            export-tagged-sigids
            dep-tagged-sigids)))))))


(define-syntax/err-param (unit/new-import-export stx)
  (syntax-case stx ()
    ((_ . x)
     (begin
       (let-values (((u x y z) (build-unit/new-import-export (check-unit-syntax #'x))))
         u)))))

;; build-compound-unit : syntax-object  -> 
;;                      (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a compound-unit expression.  stx match the return of 
;; check-compound-syntax
;; The two additional values are the identifiers of the compound-unit's import and export
;; signatures
(define-for-syntax (build-compound-unit stx)
  (define-struct lnkid-record (access-code names ctime-ids rtime-ids source-idx sigid siginfo))
  (define (lnkid-rec->keys t rec)
    (map (lambda (rid) (build-key t rid))
         (lnkid-record-rtime-ids rec)))
  (syntax-case stx ()
    (((import ...)
      (export-lnktag ...)
      (((sub-out ...) sub-exp sub-in-lnktag ...) ...))
     (with-syntax ((((import-tag import-lnkid . import-sigid) ...)
                    (map check-tagged-:-clause (syntax->list #'(import ...))))
                   (((export-tag . export-lnkid) ...)
                    (map check-tagged-id
                         (syntax->list #'(export-lnktag ...))))
                   ((((sub-out-tag sub-out-lnkid . sub-out-sigid) ...) ...)
                    (map (lambda (e) (map check-tagged-:-clause (syntax->list e)))
                         (syntax->list #'((sub-out ...) ...))))
                   ((((sub-in-tag . sub-in-lnkid) ...) ...)
                    (map (lambda (t) (map check-tagged-id (syntax->list t)))
                         (syntax->list #'((sub-in-lnktag ...) ...)))))
       
       (let ([dup (check-duplicate-identifier 
                   (syntax->list #'(import-lnkid ... sub-out-lnkid ... ...)))])
         (when dup
           (raise-stx-err "duplicate linking identifier definition" dup)))
       
       
       (let ([bt (make-bound-identifier-mapping)])
         (for-each
          (lambda (lnkid)
            (bound-identifier-mapping-put! bt lnkid #t))
          (syntax->list #'(import-lnkid ...)))
         (for-each
          (lambda (lnkid)
            (when (bound-identifier-mapping-get bt lnkid (lambda () #f))
              (raise-stx-err "cannot directly export an import" lnkid)))
          (syntax->list #'(export-lnkid ...))))
       
       
       (let* ([idxs (iota (add1 (length (syntax->list #'(sub-exp ...)))))]
              [sub-export-table-tmps (generate-temporaries #'(sub-exp ...))]
              [link-map
               (let ((bt (make-bound-identifier-mapping)))
                 (for-each 
                  (lambda (tags lnkids sigids tableid i)
                    (for-each
                     (lambda (tag lnkid sigid)
                       (define siginfo (signature-siginfo (lookup-signature sigid)))
                       (define rtime-ids (map syntax-local-introduce
                                              (siginfo-rtime-ids siginfo)))
                       (bound-identifier-mapping-put!
                        bt
                        lnkid
                        (make-lnkid-record 
                         #`(hash-ref
                            #,tableid
                            #,(build-key (syntax-e tag) (car rtime-ids)))
                         (siginfo-names siginfo)
                         (siginfo-ctime-ids siginfo)
                         rtime-ids
                         i
                         sigid
                         siginfo)))
                     (syntax->list tags)
                     (syntax->list lnkids)
                     (syntax->list sigids)))
                  (syntax->list #'((import-tag ...) (sub-out-tag ...) ...))
                  (syntax->list #'((import-lnkid ...) (sub-out-lnkid ...) ...))
                  (syntax->list #'((import-sigid ...) (sub-out-sigid ...) ...))
                  (cons #'import-table-id sub-export-table-tmps)
                  idxs)
                 (lambda (id)
                   (bound-identifier-mapping-get
                    bt 
                    id
                    (lambda ()
                      (raise-stx-err "unknown linking identifier" id)))))]
              [link-deps
               (map
                (lambda (tags lnkids i)
                  (define ht (make-hash))
                  (for-each
                   (lambda (t l)
                     (define et (syntax-e t))
                     (define el (syntax-e l))
                     (define rec (link-map l))
                     (define forward-dep (>= (lnkid-record-source-idx rec) i))
                     (define import-dep (= 0 (lnkid-record-source-idx rec)))
                     (for-each
                      (lambda (ctime-id rtime-id name)
                        (hash-set! ht
                                   (build-key et ctime-id)
                                   (list forward-dep import-dep et rtime-id name el)))
                      (lnkid-record-ctime-ids rec)
                      (lnkid-record-rtime-ids rec)
                      (lnkid-record-names rec)))
                   (syntax->list tags)
                   (syntax->list lnkids))
                  (hash-map ht (lambda (x y) y)))
                (syntax->list #'((sub-in-tag ...) ...))
                (syntax->list #'((sub-in-lnkid ...) ...))
                (cdr idxs))])
         
         (check-duplicate-subs 
          (map (lambda (t lid) (cons (syntax-e t)
                                     (lnkid-record-siginfo (link-map lid))))
               (syntax->list #'(export-tag ...))                 
               (syntax->list #'(export-lnkid ...)))
          (syntax->list #'(export-lnktag ...)))
         
         (with-syntax (((sub-tmp ...) (generate-temporaries #'(sub-exp ...)))
                       ((sub-export-table-tmp ...) sub-export-table-tmps)
                       (name (syntax-local-infer-name (error-syntax)))
                       (((import-key ...) ...)
                        (map
                         (lambda (t l) 
                           (lnkid-rec->keys (syntax-e t) (link-map l)))
                         (syntax->list #'(import-tag ...))
                         (syntax->list #'(import-lnkid ...))))
                       (((export-key ...) ...)
                        (map
                         (lambda (t l) 
                           (lnkid-rec->keys (syntax-e t) (link-map l)))
                         (syntax->list #'(export-tag ...))
                         (syntax->list #'(export-lnkid ...))))
                       ((import-name ...)
                        (map (lambda (l) (car (lnkid-record-names (link-map l))))
                             (syntax->list #'(import-lnkid ...))))
                       ((export-name ...)
                        (map (lambda (l) (car (lnkid-record-names (link-map l))))
                             (syntax->list #'(export-lnkid ...))))
                       (((((sub-in-key sub-in-code) ...) ...) ...)
                        (map
                         (lambda (stxed-tags lnkids)
                           (define lnkid-recs (map link-map (syntax->list lnkids)))
                           (define tags (map syntax-e (syntax->list stxed-tags)))
                           (define tagged-siginfos 
                             (map
                              (lambda (t l) (cons t (lnkid-record-siginfo l)))
                              tags
                              lnkid-recs))
                           (check-duplicate-subs tagged-siginfos (syntax->list lnkids))
                           (map
                            (lambda (t lr)
                              (with-syntax (((key ...)
                                             (lnkid-rec->keys t lr)))
                                #`((key #,(lnkid-record-access-code lr)) ...)))
                            tags
                            lnkid-recs))
                         (syntax->list #'((sub-in-tag ...) ...))
                         (syntax->list #'((sub-in-lnkid ...) ...))))
                       ((((sub-out-key ...) ...) ...)
                        (map
                         (lambda (lnkids tags)
                           (map
                            (lambda (l t)
                              (lnkid-rec->keys (syntax-e t) (link-map l)))
                            (syntax->list lnkids)
                            (syntax->list tags)))
                         (syntax->list #'((sub-out-lnkid ...) ...))
                         (syntax->list #'((sub-out-tag ...) ...))))
                       (((export-sigid . export-code) ...)
                        (map (lambda (lnkid)
                               (define s (link-map lnkid))
                               (cons (lnkid-record-sigid s)
                                     (lnkid-record-access-code s)))
                             (syntax->list #'(export-lnkid ...))))
                       (form (syntax-e (stx-car (error-syntax))))
                       )
           
           (with-syntax (((check-sub-exp ...)
                          (map
                           (lambda (stx link-deps)
                             (with-syntax (((sub-exp
                                             sub-tmp
                                             ((sub-in-key ...) ...)
                                             ((sub-out-key ...) ...)
                                             sub-in-lnkid
                                             sub-out-lnkid)
                                            stx))
                               (with-syntax (((sub-in-signame ...)
                                              (map (lambda (l) (car (lnkid-record-names (link-map l))))
                                                   (syntax->list #'sub-in-lnkid)))
                                             ((sub-out-signame ...)
                                              (map (lambda (l) (car (lnkid-record-names (link-map l))))
                                                   (syntax->list #'sub-out-lnkid)))
                                             (((fdep-tag fdep-rtime fsig-name flnk-name) ...)
                                              (map cddr (filter car link-deps)))
                                             (((rdep-tag rdep-rtime . _) ...)
                                              (map cddr (filter cadr link-deps))))
                                 #`(begin
                                     #,(syntax/loc #'sub-exp
                                         (check-unit sub-tmp 'form))
                                     #,(syntax/loc #'sub-exp
                                         (check-sigs sub-tmp
                                                     (vector-immutable
                                                      (cons 'sub-in-signame
                                                            (vector-immutable sub-in-key ...))
                                                      ...)
                                                     (vector-immutable
                                                      (cons 'sub-out-signame
                                                            (vector-immutable sub-out-key ...))
                                                      ...)
                                                     'form))
                                     (let ([fht (equal-hash-table
                                                 ((cons 'fdep-tag fdep-rtime)
                                                  (cons 'fsig-name 'flnk-name))
                                                 ...)]
                                           [rht (equal-hash-table
                                                 ((cons 'rdep-tag rdep-rtime)
                                                  #t)
                                                 ...)])
                                       #,(syntax/loc #'sub-exp (check-deps fht sub-tmp 'form))
                                       (for-each
                                        (lambda (dep)
                                          (when (hash-ref rht dep #f)
                                            (set! deps (cons dep deps))))
                                        (unit-deps sub-tmp)))))))
                           (syntax->list #'((sub-exp
                                             sub-tmp
                                             ((sub-in-key ...) ...)
                                             ((sub-out-key ...) ...)
                                             (sub-in-lnkid ...)
                                             (sub-out-lnkid ...))
                                            ...))
                           link-deps))
                         (((sub-in-key-code-workaround ...) ...)
                          (map
                           (lambda (x)
                             (with-syntax ((((a ...) ...) x))
                               #'(a ... ...)))
                           (syntax->list #'((((sub-in-key sub-in-code) ...) ...) ...))))
                         )
             (values
              (quasisyntax/loc (error-syntax)
                (let ([deps '()]
                      [sub-tmp sub-exp] ...)
                  check-sub-exp ...
                  (make-unit
                   'name
                   (vector-immutable
                    (cons 'import-name
                          (vector-immutable import-key ...))
                    ...)
                   (vector-immutable
                    (cons 'export-name
                          (vector-immutable export-key ...))
                    ...)
                   deps
                   (lambda ()
                     (let-values ([(sub-tmp sub-export-table-tmp) ((unit-go sub-tmp))]
                                  ...)
                       (values (lambda (import-table-id)
                                 (void)
                                 (sub-tmp (equal-hash-table sub-in-key-code-workaround ...))
                                 ...)
                               (unit-export ((export-key ...) export-code) ...)))))))
              (map syntax-e (syntax->list #'((import-tag . import-sigid) ...)))
              (map syntax-e (syntax->list #'((export-tag . export-sigid) ...)))
              '()))))))
    (((i ...) (e ...) (l ...))
     (for-each check-link-line-syntax (syntax->list #'(l ...))))))


(define-syntax/err-param (compound-unit stx)
  (let-values (((u x y z)
                (build-compound-unit
                 (check-compound-syntax (syntax-case stx () ((_ . x) #'x))))))
    u))


(define (invoke-unit/core unit)
  (check-unit unit 'invoke-unit)
  (check-no-imports unit 'invoke-unit)
  (let-values ([(f exports) ((unit-go unit))])
    (f #f)))

(define-syntax/err-param (define-values/invoke-unit/core stx)
  (syntax-case stx ()
    ((_ unit-expr . unit-out)
     (let* ((unit-out (checked-syntax->list #'unit-out))
            (tagged-out (map process-tagged-import unit-out))
            (out-tags (map car tagged-out))
            (out-sigs (map caddr tagged-out))
            (dup (check-duplicate-identifier (apply append (map sig-int-names out-sigs))))
            (out-vec (generate-temporaries out-sigs))
            (tmarker (make-syntax-introducer))
            (tmp-bindings (map (λ (s) (map tmarker (map car (car s)))) out-sigs))
            (def-table (make-bound-identifier-mapping)))
       (when dup
         (raise-stx-err (format "duplicate binding for ~.s" (syntax-e dup))))
       (for-each
        (λ (sig new-xs)
          (for-each 
           (λ (old new)
             (bound-identifier-mapping-put! def-table old new))
           (map car (car sig))
           new-xs))
        out-sigs
        tmp-bindings)
       (with-syntax ((((key1 key ...) ...) (map tagged-info->keys out-tags))
                     ((((int-binding . ext-binding) ...) ...) (map car out-sigs))
                     ((out-vec ...) out-vec)
                     (((renames
                        (((mac-name ...) mac-body) ...) 
                        (((val-name ...) val-body) ...))
                       ...)
                      (map build-val+macro-defs out-sigs))
                     ((out-names ...)
                      (map (lambda (info) (car (siginfo-names (cdr info))))
                           out-tags))
                     (((tmp-binding ...) ...) tmp-bindings)
                     (((out-code ...) ...)
                      (map 
                       (lambda (os ov)
                         (map 
                          (lambda (i)
                            #`(vector-ref #,ov #,i))
                          (iota (length (car os)))))
                       out-sigs
                       out-vec))
                     (((wrap-code ...) ...)
                      (map (λ (os ov tbs)
                             (define rename-bindings 
                               (get-member-bindings def-table os #'(quote-module-name)))
                             (map (λ (tb i v c)
                                    (if c
                                        (with-syntax ([ctc-stx
                                                       (syntax-property
                                                        #`(letrec-syntax #,rename-bindings #,c)
                                                        'inferred-name v)])
                                          #`(let ([v/c (#,tb)])
                                              (contract ctc-stx (car v/c) (cdr v/c)
                                                        (current-contract-region)
                                                        (quote #,v) (quote-srcloc #,v))))
                                        #`(#,tb)))
                                  tbs
                                  (iota (length (car os)))
                                  (map car (car os))
                                  (cadddr os)))
                           out-sigs
                           out-vec
                           tmp-bindings)))
         (quasisyntax/loc stx
           (begin
             (define-values (tmp-binding ... ...)
               #,(syntax/loc #'unit-expr
                   (let ((unit-tmp unit-expr))
                     (check-unit unit-tmp 'define-values/invoke-unit)
                     (check-sigs unit-tmp
                                 (vector-immutable)
                                 (vector-immutable (cons 'out-names
                                                         (vector-immutable key1 key ...)) ...)
                                 'define-values/invoke-unit)
                     (let-values (((unit-fn export-table)
                                   ((unit-go unit-tmp))))
                       (let ([out-vec (hash-ref export-table key1)] ...)
                         (unit-fn #f)
                         (values out-code ... ...))))))
             (define-values (int-binding ... ...)
               (values wrap-code ... ...))
             (define-syntaxes . renames) ...
             (define-syntaxes (mac-name ...) mac-body) ... ...
             (define-values (val-name ...) val-body) ... ...)))))
    ((_)
     (raise-stx-err "missing unit expression"))))

;; build-unit-from-context : syntax-object -> 
;;                           (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a unit-from-context expression.  stx must be
;; such that it passes check-ufc-syntax.
;; The two additional values are the identifiers of the unit's import and export
;; signatures
(define-for-syntax (build-unit-from-context stx)
  (syntax-case stx ()
    ((export-spec)
     (let* ((tagged-export-sig (process-tagged-export #'export-spec))
            (export-sig (caddr tagged-export-sig))
            (int+ext-ids
             (let ([int+ext-ids (car export-sig)]
                   [post-ids (apply append (map car (list-ref export-sig 4)))])
               ;; Remove any bindings that will be generated via post- definitions
               (if (null? post-ids)
                   int+ext-ids
                   (let ([ht (make-bound-identifier-mapping)])
                     (for ([post-id (in-list post-ids)])
                       (bound-identifier-mapping-put! ht post-id #t))
                     (for/list ([int+ext-id (in-list int+ext-ids)]
                                #:unless (bound-identifier-mapping-get
                                          ht
                                          (car int+ext-id)
                                          (lambda () #f)))
                       int+ext-id))))))
       (with-syntax ((((int-id . ext-id) ...) int+ext-ids)
                     ((def-name ...) (generate-temporaries (map car int+ext-ids))))
         (values
          #'(:unit (import) (export (rename export-spec (def-name int-id) ...))
                   (define def-name int-id)
                   ...)
          null
          (list (cadr tagged-export-sig))
          '()))))))

(define-for-syntax (check-ufc-syntax stx)
  (syntax-case stx ()
    ((export-spec) (void))
    (()
     (raise-stx-err "missing export-spec"))
    (_
     (raise-stx-err "nothing is permitted after export-spec"))))

(define-syntax/err-param (unit-from-context stx)
  (syntax-case stx ()
    ((_ . x)
     (begin
       (check-ufc-syntax #'x)
       (let-values (((u x y z) (build-unit-from-context #'x)))
         u)))))



(define-for-syntax (build-define-unit-helper contracted?)
  (lambda (stx build err-msg)
    (syntax-case stx ()
      ((_ name . rest)
       (begin
         (check-id #'name)
         (let-values (((exp i e d) (parameterize ([error-syntax (syntax-property (error-syntax) 'inferred-name (syntax-e #'name))])
                                     (build #'rest ))))
           (with-syntax ((((itag . isig) ...) i)
                         (((etag . esig) ...) e)
                         (((deptag . depsig) ...) d)
                         (contracted? contracted?))
             (quasisyntax/loc (error-syntax)
               (begin
                 (define u #,exp)
                 (define-syntax name
                   (make-set!-transformer
                    (make-unit-info (quote-syntax u)
                                    (list (cons 'itag (quote-syntax isig)) ...)
                                    (list (cons 'etag (quote-syntax esig)) ...)
                                    (list (cons 'deptag (quote-syntax deptag)) ...)
                                    (quote-syntax name)
                                    contracted?)))))))))
      ((_)
       (raise-stx-err err-msg)))))

;; build-define-unit : syntax-object
;;                     (syntax-object -> (values syntax-object (listof identifier) (listof identifier))
;;                     string ->
;;                     syntax-object
(define-for-syntax build-define-unit (build-define-unit-helper #f))
(define-for-syntax build-define-unit/contracted (build-define-unit-helper #t))

(define-for-syntax (build-define-unit-binding stx)
  
  (define (check-helper tagged-info)
    (cons (car (siginfo-names (cdr tagged-info)))
          (tagged-info->keys tagged-info)))
  
  (syntax-case stx (import export init-depend)
    ((unit-exp (import i ...) (export e ...) (init-depend idep ...))
     (let* ([ti (syntax->list #'(i ...))]
            [te (syntax->list #'(e ...))]
            [tidep (syntax->list #'(idep ...))]
            [tagged-import-sigids (map check-tagged-id ti)]
            [tagged-export-sigids (map check-tagged-id te)]
            [tagged-dep-sigids (map check-tagged-id tidep)]
            [tagged-import-infos (map tagged-sigid->tagged-siginfo tagged-import-sigids)]
            [tagged-export-infos (map tagged-sigid->tagged-siginfo tagged-export-sigids)]
            [tagged-dep-siginfos (map tagged-sigid->tagged-siginfo tagged-dep-sigids)])
       (check-duplicate-sigs tagged-import-infos ti tagged-dep-siginfos tidep)         
       (check-duplicate-subs tagged-export-infos te)
       (with-syntax ((((import-name . (import-keys ...)) ...)
                      (map check-helper tagged-import-infos))
                     (((export-name . (export-keys ...)) ...)
                      (map check-helper tagged-export-infos))
                     (form (stx-car (error-syntax))))
         (values
          #`(let ([unit-tmp unit-exp])
              #,(syntax/loc #'unit-exp
                  (check-unit unit-tmp 'form))
              #,(syntax/loc #'unit-exp
                  (check-sigs unit-tmp 
                              (vector-immutable
                               (cons 'import-name
                                     (vector-immutable import-keys ...))
                               ...)
                              (vector-immutable
                               (cons 'export-name
                                     (vector-immutable export-keys ...))
                               ...)
                              'form))
              unit-tmp)
          tagged-import-sigids
          tagged-export-sigids
          tagged-dep-sigids))))))

(define-syntax/err-param (define-unit-binding stx)
  (build-define-unit stx  (lambda (unit)
                            (build-define-unit-binding (check-unit-body-syntax unit)))
                     "missing unit name, unit expression, import clause, and export clause"))

(define-syntax/err-param (define-unit stx)
  (build-define-unit stx (lambda (unit)
                           (build-unit (check-unit-syntax unit)))
                     "missing unit name, import clause, and export clause"))

(define-syntax/err-param (define-unit/new-import-export stx)
  (build-define-unit stx (lambda (unit)
                           (build-unit/new-import-export (check-unit-syntax unit)))
                     "missing unit name, import clause, and export clause"))

(define-syntax/err-param (define-compound-unit stx)
  (build-define-unit stx (lambda (clauses)
                           (build-compound-unit (check-compound-syntax clauses)))
                     "missing unit name"))

(define-syntax/err-param (define-unit-from-context stx)
  (build-define-unit stx (lambda (sig)
                           (check-ufc-syntax sig)
                           (build-unit-from-context sig))
                     "missing unit name and signature"))

(define-for-syntax (build-unit/contract stx)
  (syntax-parse stx
                [(:import-clause/contract :export-clause/contract dep:dep-clause . body)
                 (let-values ([(exp isigs esigs deps) 
                               (build-unit
                                (check-unit-syntax
                                 (syntax/loc stx
                                   ((import i.s ...) (export e.s ...) dep . body))))])
                   (with-syntax ([name (syntax-local-infer-name (error-syntax))]
                                 [(import-tagged-sig-id ...)
                                  (map (λ (i s)
                                         (if (identifier? i) #`(tag #,i #,s) s))
                                       (syntax->list #'(i.s.i ...))
                                       (syntax->list #'(i.s.s.name ...)))]
                                 [(export-tagged-sig-id ...)
                                  (map (λ (i s)
                                         (if (identifier? i) #`(tag #,i #,s) s))
                                       (syntax->list #'(e.s.i ...))
                                       (syntax->list #'(e.s.s.name ...)))])
                     (with-syntax ([new-unit exp]
                                   [unit-contract
                                    (unit/c/core
                                     #'name
                                     (syntax/loc stx
                                       ((import (import-tagged-sig-id [i.x i.c] ...) ...)
                                        (export (export-tagged-sig-id [e.x e.c] ...) ...))))])
                       (values 
                        (syntax/loc stx
                          (contract unit-contract new-unit '(unit name) (current-contract-region) (quote name) (quote-srcloc name)))
                        isigs esigs deps))))]
                [(ic:import-clause/contract ec:export-clause/contract . body)
                 (build-unit/contract 
                  (syntax/loc stx
                    (ic ec (init-depend) . body)))]))

(define-syntax/err-param (define-unit/contract stx)
  (build-define-unit/contracted stx (λ (stx)
                                      (build-unit/contract stx))
                                "missing unit name"))

(define-for-syntax (unprocess-tagged-id ti)
  (if (car ti)
      #`(tag #,(car ti) #,(cdr ti))
      (cdr ti)))

(define-for-syntax (temp-id-with-tags id i)
  (syntax-case i (tag)
    [(tag t sig)
     (list id #`(tag t #,id) #'sig)]
    [_else
     (list id id i)]))

(define-syntax/err-param (define-values/invoke-unit stx)
  (syntax-case stx (import export)
    ((_ u (import) (export e ...))
     (quasisyntax/loc stx
       (define-values/invoke-unit/core u e ...)))
    ((_ u (import i ...) (export e ...))
     (with-syntax (((EU ...) (generate-temporaries #'(e ...)))
                   (((IU IUl i) ...) (map temp-id-with-tags
                                          (generate-temporaries #'(i ...))
                                          (syntax->list #'(i ...))))
                   ((iu ...) (generate-temporaries #'(i ...)))
                   ((i-id ...) (map cdadr
                                    (map process-tagged-import
                                         (syntax->list #'(i ...)))))
                   ((e-id ...) (map cdadr 
                                    (map process-tagged-export
                                         (syntax->list #'(e ...))))))
       (quasisyntax/loc stx
         (begin
           (define-unit-from-context iu i)
           ...
           (define-compound-unit u2 (import)
             (export EU ...)
             (link [((IU : i-id)) iu] ... [((EU : e-id) ...) u IUl ...]))
           (define-values/invoke-unit/core u2 e ...)))))
    ((_)
     (raise-stx-err "missing unit" stx))
    ((_ . b)
     (raise-stx-err
      (format "expected syntax matching (~a <unit-expression> (import <sig-expr> ...) (export <sig-expr> ...))"
              (syntax-e (stx-car stx)))))))

;; build-compound-unit/infer : syntax-object  -> 
;;                      (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a compound-unit/infer expression.  stx match the return of 
;; check-compound-syntax
;; The two additional values are the identifiers of the compound-unit's import and export
;; signatures
(define-for-syntax (build-compound-unit/infer stx)
  
  (define (lookup-tagged tid)
    (cons (car tid) (lookup-signature (cdr tid))))
  
  (define (process-signature s)
    (define l
      ((check-tagged 
        (lambda (b)
          (syntax-case* b (:) (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
            ((x : y)
             (and (identifier? #'x) (identifier? #'y))
             (list #'x #'y (signature-siginfo (lookup-signature #'y))))
            (x
             (identifier? #'x)
             (list (car (generate-temporaries (list #'x)))
                   #'x
                   (signature-siginfo (lookup-signature #'x))))
            (_
             (raise-stx-err "expected syntax matching <identifier> or (<identifier> : <identifier>)"
                            b)))))
       s))
    (apply make-link-record l))
  
  (define ((process-tagged-sigid introducer) sid)
    (make-link-record (car sid) #f (introducer (cdr sid)) (signature-siginfo (lookup-signature (cdr sid)))))
  
  (syntax-case stx ()
    (((import ...) 
      (export ...)
      (((out ...) u l ...) ...))
     (let* ([us (syntax->list #'(u ...))]
            [units (map lookup-def-unit us)]
            [import-sigs (map process-signature 
                              (syntax->list #'(import ...)))]
            [sig-introducers (map (lambda (unit u)
                                    (make-syntax-delta-introducer u (unit-info-orig-binder unit)))
                                  units us)]
            [sub-outs
             (map
              (lambda (outs unit sig-introducer)
                (define o
                  (map
                   (lambda (clause)
                     (define c (check-tagged-:-clause clause))
                     (make-link-record (car c) (cadr c) (cddr c)
                                       (signature-siginfo (lookup-signature (cddr c)))))
                   (syntax->list outs)))
                (complete-exports (map (process-tagged-sigid sig-introducer) (unit-info-export-sig-ids unit))
                                  o))
              (syntax->list #'((out ...) ...))
              units
              sig-introducers)]
            [link-defs (append import-sigs (apply append sub-outs))])
       
       (define lnk-table (make-bound-identifier-mapping))
       (define sig-table (make-hasheq))
       
       (let ([dup (check-duplicate-identifier (map link-record-linkid link-defs))])
         (when dup
           (raise-stx-err "duplicate identifier" dup)))
       
       (for-each
        (lambda (b)
          (bound-identifier-mapping-put! lnk-table (link-record-linkid b) b))
        link-defs)
       
       (for-each
        (lambda (b)
          (for-each
           (lambda (cid)
             (define there? (hash-ref sig-table cid #f))
             (hash-set! sig-table cid (if there? 'duplicate (link-record-linkid b))))
           (siginfo-ctime-ids (link-record-siginfo b))))
        link-defs)
       
       (let ([sub-ins
              (map
               (lambda (ins unit sig-introducer unit-stx)
                 (define is (syntax->list ins))
                 (define lrs
                   (map
                    (lambda (i)
                      (define tagged-lnkid (check-tagged-id i))
                      (define sig
                        (bound-identifier-mapping-get lnk-table
                                                      (cdr tagged-lnkid)
                                                      (lambda () #f)))
                      (unless sig
                        (raise-stx-err "unknown linking identifier" i))
                      (make-link-record (car tagged-lnkid)
                                        (cdr tagged-lnkid)
                                        (link-record-sigid sig)
                                        (link-record-siginfo sig)))
                    is))
                 (check-duplicate-subs
                  (map 
                   (lambda (lr) (cons (link-record-tag lr) (link-record-siginfo lr)))
                   lrs)
                  is)
                 (complete-imports sig-table 
                                   lrs
                                   (map (process-tagged-sigid sig-introducer)
                                        (unit-info-import-sig-ids unit))
                                   unit-stx))
               (syntax->list #'((l ...) ...))
               units
               sig-introducers
               us)]
             [exports
              (map 
               (lambda (e)
                 (define tid (check-tagged-spec-syntax e #f identifier?))
                 (define lookup (bound-identifier-mapping-get 
                                 lnk-table
                                 (cdr tid)
                                 (lambda () #f)))
                 (cond
                   [lookup (unprocess-tagged-id tid)]
                   [else
                    (let ([lnkid (hash-ref
                                  sig-table
                                  (car (siginfo-ctime-ids (signature-siginfo (lookup-signature (cdr tid)))))
                                  #f)])
                      (cond
                        [(not lnkid)
                         (raise-stx-err "no sub unit exports this signature" (cdr tid))]
                        [(eq? lnkid 'duplicate)
                         (raise-stx-err "multiple sub units export this signature" (cdr tid))]
                        [else 
                         (unprocess-tagged-id
                          (cons (car tid) lnkid))]))]))
               (syntax->list #'(export ...)))])
         (with-syntax (((import ...)
                        (map unprocess-link-record-bind import-sigs))
                       (((out ...) ...)
                        (map
                         (lambda (out) 
                           (map unprocess-link-record-bind out))
                         sub-outs))
                       (((in ...) ...)
                        (map
                         (lambda (ins)
                           (map unprocess-link-record-use ins))
                         sub-ins))
                       ((unit-id ...) (map 
                                       (lambda (u stx)
                                         (quasisyntax/loc stx #,(unit-info-unit-id u)))
                                       units (syntax->list #'(u ...)))))
           (build-compound-unit #`((import ...)
                                   #,exports
                                   (((out ...) unit-id in ...) ...)))))))
    (((i ...) (e ...) (l ...))
     (for-each check-link-line-syntax (syntax->list #'(l ...))))))


(define-for-syntax (check-compound/infer-syntax stx)
  (syntax-case (check-compound-syntax stx) ()
    ((i e (b ...))
     (with-syntax (((b ...)
                    (map
                     (lambda (b)
                       (if (identifier? b)
                           #`(() #,b)
                           b))
                     (syntax->list #'(b ...)))))
       #'(i e (b ...))))))

(define-syntax/err-param (compound-unit/infer stx)
  (let-values (((u i e d)
                (build-compound-unit/infer
                 (check-compound/infer-syntax 
                  (syntax-case stx () ((_ . x) #'x))))))
    u))

(define-for-syntax (do-define-compound-unit/infer stx)
  (build-define-unit stx 
                     (lambda (clause)
                       (build-compound-unit/infer (check-compound/infer-syntax clause)))
                     "missing unit name"))

(define-syntax/err-param (define-compound-unit/infer stx)
  (do-define-compound-unit/infer stx))

;; (syntax or listof[syntax]) boolean (boolean or listof[syntax]) -> syntax
(define-for-syntax (build-invoke-unit/infer units define? exports)
  (define (imps/exps-from-unit u)      
    (let* ([ui (lookup-def-unit u)]
           [unprocess (let ([i (make-syntax-delta-introducer u (unit-info-orig-binder ui))])
                        (lambda (p)
                          (unprocess-tagged-id (cons (car p) (i (cdr p))))))]
           [isigs (map unprocess (unit-info-import-sig-ids ui))]
           [esigs (map unprocess (unit-info-export-sig-ids ui))])
      (values isigs esigs)))
  (define (drop-from-other-list exp-tagged imp-tagged imp-sources)
    (let loop ([ts imp-tagged] [ss imp-sources])
      (cond
        [(null? ts) null]
        [(ormap (lambda (tinfo2)
                  (and (eq? (car (car ts)) (car tinfo2))
                       (siginfo-subtype (cdr tinfo2) (cdr (car ts)))))
                exp-tagged)
         (loop (cdr ts) (cdr ss))]
        [else (cons (car ss) (loop (cdr ts) (cdr ss)))])))
  
  (define (drop-duplicates tagged-siginfos sources)
    (let loop ([ts tagged-siginfos] [ss sources] [res-t null] [res-s null])
      (cond
        [(null? ts) (values res-t res-s)]
        [(ormap (lambda (tinfo2)
                  (and (eq? (car (car ts)) (car tinfo2))
                       (siginfo-subtype (cdr tinfo2) (cdr (car ts)))))
                (cdr ts))
         (loop (cdr ts) (cdr ss) res-t res-s)]
        [else (loop (cdr ts) (cdr ss) (cons (car ts) res-t) (cons (car ss) res-s))])))
  
  (define (imps/exps-from-units units exports)
    (define-values (isigs esigs)
      (let loop ([units units] [imps null] [exps null])
        (if (null? units)
            (values imps exps)
            (let-values ([(i e) (imps/exps-from-unit (car units))])
              (loop (cdr units) (append i imps) (append e exps))))))
    (define-values (isig tagged-import-sigs import-tagged-infos 
                         import-tagged-sigids import-sigs)
      (process-unit-import (datum->syntax #f isigs)))
    
    (define-values (esig tagged-export-sigs export-tagged-infos 
                         export-tagged-sigids export-sigs)
      (process-unit-export (datum->syntax #f esigs)))
    (check-duplicate-subs export-tagged-infos esig)
    (let-values ([(itagged isources) (drop-duplicates import-tagged-infos isig)])
      (values (drop-from-other-list export-tagged-infos itagged isources) 
              (cond
                [(list? exports)
                 (let-values ([(spec-esig spec-tagged-export-sigs spec-export-tagged-infos 
                                          spec-export-tagged-sigids spec-export-sigs)
                               (process-unit-export (datum->syntax #f exports))])
                   (restrict-exports export-tagged-infos
                                     spec-esig spec-export-tagged-infos))]
                [else esig]))))
  
  (define (restrict-exports unit-tagged-exports spec-exports spec-tagged-exports)
    (for-each (lambda (se ste)
                (unless (ormap (lambda (ute)
                                 (and (eq? (car ute) (car ste))
                                      (siginfo-subtype (cdr ute) (cdr ste))))
                               unit-tagged-exports)
                  (raise-stx-err (format "no subunit exports signature ~a"
                                         (syntax->datum se))
                                 se)))
              spec-exports
              spec-tagged-exports)
    spec-exports)
  (when (and (not define?) exports)
    (error 'build-invoke-unit/infer 
           "internal error: exports for invoke-unit/infer"))
  (when (null? units)
    (raise-stx-err "no units in link clause"))
  (cond [(identifier? units)
         (let-values ([(isig esig) (imps/exps-from-units (list units) exports)])
           (with-syntax ([u units]
                         [(esig ...) esig]
                         [(isig ...) isig])
             (if define?
                 (syntax/loc (error-syntax) (define-values/invoke-unit u (import isig ...) (export esig ...)))
                 (syntax/loc (error-syntax) (invoke-unit u (import isig ...))))))]
        [(list? units)
         (let-values ([(isig esig) (imps/exps-from-units units exports)])
           (with-syntax ([(new-unit) (generate-temporaries '(new-unit))]
                         [(unit ...) units]
                         [(esig ...) esig]
                         [(isig ...) isig])
             (with-syntax ([u (let-values ([(u i e d)
                                            (build-compound-unit/infer
                                             (check-compound/infer-syntax
                                              #'((import isig ...)
                                                 (export esig ...)
                                                 (link unit ...))))]) u)])
               (if define?
                   (syntax/loc (error-syntax)
                     (define-values/invoke-unit u
                       (import isig ...) (export esig ...)))
                   (syntax/loc (error-syntax)
                     (invoke-unit u
                                  (import isig ...)))))))]
        ;; just for error handling
        [else (lookup-def-unit units)]))

(define-syntax/err-param (define-values/invoke-unit/infer stx)
  (syntax-case stx (export link)
    [(_ (link unit ...))
     (build-invoke-unit/infer (syntax->list #'(unit ...)) #t #f)]
    [(_ (export e ...) (link unit ...))
     (build-invoke-unit/infer (syntax->list #'(unit ...)) #t (syntax->list #'(e ...)))]
    [(_ (export e ...) u) 
     (build-invoke-unit/infer #'u #t (syntax->list #'(e ...)))]
    [(_ u) 
     (build-invoke-unit/infer #'u #t #f)]
    [(_)
     (raise-stx-err "missing unit" stx)]
    [(_ . b)
     (raise-stx-err
      (format "expected syntax matching (~a [(export <define-signature-identifier>)] <define-unit-identifier>) or (~a  [(export <define-signature-identifier>)] (link <define-unit-identifier> ...))"
              (syntax-e (stx-car stx)) (syntax-e (stx-car stx))))]))

(define-syntax/err-param (invoke-unit stx)
  (syntax-case stx (import)
    ((_ unit)
     (syntax/loc stx
       (invoke-unit/core unit)))
    ((_ unit (import isig ...))
     (with-syntax (((u ...) (generate-temporaries (syntax->list #'(isig ...))))
                   (((U Ul isig) ...) (map temp-id-with-tags
                                           (generate-temporaries #'(isig ...))
                                           (syntax->list #'(isig ...))))
                   ((isig-id ...) (map cdadr
                                       (map process-tagged-import
                                            (syntax->list #'(isig ...))))))
       (syntax/loc stx
         (let ()
           (define-unit-from-context u isig)
           ...
           (define-compound-unit u2 (import) (export)
             (link [((U : isig-id)) u] ... [() unit Ul ...]))
           (invoke-unit/core u2)))))
    (_ (raise-stx-err (format
                       "expected (~a <expr>) or (~a <expr> (import <sig-expr> ...))"
                       (syntax-e (stx-car stx))
                       (syntax-e (stx-car stx)))))))

(define-syntax/err-param (invoke-unit/infer stx)
  (syntax-case stx ()
    [(_ (link unit ...))
     (build-invoke-unit/infer (syntax->list #'(unit ...)) #f #f)]
    [(_ u) (build-invoke-unit/infer #'u #f #f)]
    [(_)
     (raise-stx-err "missing unit" stx)]
    [(_ . b)
     (raise-stx-err
      (format "expected syntax matching (~a <define-unit-identifier>) or (~a (link <define-unit-identifier> ...))"
              (syntax-e (stx-car stx)) (syntax-e (stx-car stx))))]))

(define-for-syntax (build-unit/s stx)
  (syntax-case stx (import export init-depend)
    [((import i ...) (export e ...) (init-depend d ...) u)
     (let* ([ui (lookup-def-unit #'u)]
            [unprocess (let ([i (make-syntax-delta-introducer #'u (unit-info-orig-binder ui))])
                         (lambda (p)
                           (unprocess-tagged-id (cons (car p) (i (cdr p))))))])
       (with-syntax ([(isig ...) (map unprocess (unit-info-import-sig-ids ui))]
                     [(esig ...) (map unprocess (unit-info-export-sig-ids ui))])
         (build-unit/new-import-export
          (syntax/loc stx
            ((import i ...) (export e ...) (init-depend d ...) ((esig ...) u isig ...))))))]))

(define-syntax/err-param (define-unit/s stx)
  (build-define-unit stx (λ (stx) (build-unit/s (check-unit-syntax stx)))
                     "missing unit name"))

(define-syntax/err-param (unit/s stx)
  (syntax-case stx ()
    [(_ . stx)
     (let-values ([(u x y z) (build-unit/s (check-unit-syntax #'stx))])
       u)]))
