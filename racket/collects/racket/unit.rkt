#lang racket/base

;; Library for first-class components with recursive linking

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/boundmap
                     syntax/id-set
                     syntax/context
                     syntax/kerncase
                     syntax/name
                     syntax/parse
                     syntax/struct
                     racket/struct-info
                     syntax/stx
                     syntax/location
                     syntax/intdef
                     "private/unit-contract-syntax.rkt"
                     "private/unit-compiletime.rkt"
                     "private/unit-syntax.rkt"))

(require racket/unsafe/undefined
         racket/contract/base
         racket/contract/region
         racket/list
         racket/stxparam
         syntax/location
         syntax/parse/define
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
       (syntax-protect
        #'(define-syntax name
            (make-set!-transformer
             (make-signature-form (λ (arg ignored) . val)))))))
    ((_ (name arg intro-arg) . val)
     (begin
       (check-id #'name)
       (check-id #'arg)
       (check-id #'intro-arg)
       (syntax-protect
        #'(define-syntax name
            (make-set!-transformer
             (make-signature-form (λ (arg intro-arg) . val)))))))
    ((_ name proc-expr)
     (identifier? #'name)
     (syntax-protect
      #'(define-syntax name
          (let ([name proc-expr])
            (make-set!-transformer
             (make-signature-form (λ (arg ignored) (name arg))))))))
    ((_ . l)
     (let ((l (checked-syntax->list stx)))
       (define name (syntax-e (stx-car stx)))
       (raise-stx-err 
        (format (string-append "bad syntax\n"
                               "  expected one of:\n"
                               "   (~a (id id) expr ...)\n"
                               "   (~a (id id id) expr ...)\n"
                               "   (~a id proc-expr)")
                name name name))))))

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
                     [(self-ctr?) (and cname
                                       (bound-identifier=? #'name (cdr cname))
                                       (not no-ctr?))])
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
                                   [else  ((make-syntax-introducer) #'name)])]
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
                 [cpairs (cons #'contracted
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
(define-for-syntax ((build-val+macro-defs intro) sig)
  (if (and (null? (cadr sig))
           (null? (caddr sig)))
      ;; No renames needed; this shortcut avoids
      ;; an explosion of renamings, especially with chains
      ;; of `open':
      (list #'(() (values)) #'() #'())
      ;; Renames and macros needed:
      (with-syntax ([(((int-ivar . ext-ivar) ...)
                      ((((int-vid . ext-vid) ...) . vbody) ...)
                      ((((int-sid . ext-sid) ...) . sbody) ...)
                      _
                      _)
                     (map-sig (lambda (x) x)
                              (let ([i (make-syntax-introducer)])
                                (lambda (x)
                                  (intro (i x))))
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

;; build-post-val-defs+ctcs : sig -> (list/c stx-list? (listof syntax?))
(define-for-syntax (build-post-val-defs+ctcs sig)
  (define introduced-sig (map-sig (lambda (x) x)
                                  (make-syntax-introducer)
                                  sig))
  (with-syntax ([(((int-ivar . ext-ivar) ...)
                  ((((int-vid . ext-vid) ...) . _) ...)
                  ((((int-sid . ext-sid) ...) . _) ...)
                  _
                  (((post-id ...) . post-rhs) ...))
                 introduced-sig])
    (with-syntax ([post-renames
                   #'((ext-ivar ... ext-vid ... ... ext-sid ... ...)
                      (make-rename-transformers
                       (quote-syntax
                        (int-ivar ...
                         int-vid ... ...
                         int-sid ... ...))))])
      (list
       #'((let-syntaxes (post-renames) post-rhs) ...)
       ; Absence of a contract is represented by #f, but a contract can be present
       ; and be #'f, so we have to be extra careful not to coerce to syntax objects
       ; too early. After wrapping in (contract . _), converting to syntax is okay.
       (map (λ (ctc)
              (and ctc (cons 'contract #`(let-syntaxes (post-renames) #,ctc))))
            (cadddr introduced-sig))))))

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

(define-signature-form (open stx enclosing-intro)
  (define (build-sig-elems ps cs)
    (map (λ (p c)
           (if (syntax-e c)
               #`(contracted [#,(car p) #,(cdr (syntax-e c))])
               (car p)))
         ps
         cs))
  (parameterize ([error-syntax stx])
    (syntax-case stx ()
      ((_ export-spec)
       (let ([sig (process-spec #'export-spec)])
         (with-syntax ([(renames
                         (((mac-name ...) mac-body) ...) 
                         (((val-name ...) val-body) ...))
                        ((build-val+macro-defs enclosing-intro) sig)]
                       [(((e-post-id ...) . _) ...) (list-ref sig 4)]
                       [((e-post-rhs ...) (e-ctc ...)) (build-post-val-defs+ctcs sig)])
           (with-syntax ([(sig-elem ...)
                          (build-sig-elems (car sig) (syntax->list #'(e-ctc ...)))])
             (syntax->list
              #'(sig-elem ...
                 (define-syntaxes . renames)
                 (define-syntaxes (mac-name ...) mac-body) ...
                 (define-values (val-name ...) val-body) ...
                 (define-values-for-export (e-post-id ...) e-post-rhs)
                 ...))))))
      (_
       (raise-stx-err (format "must match (~a export-spec)"
                              (syntax-e (stx-car stx))))))))

(define-signature-form (define-values-for-export stx)
  (raise-syntax-error #f "internal error" stx))

(define-for-syntax (introduce-def d)
  (cons (map syntax-local-introduce (car d))
        (syntax-local-introduce (cdr d))))

(begin-for-syntax
  (define-syntax-class (signature-element intro-proc)
    #:description "signature element"
    #:attributes [{var-id 1} {ctc 1}
                  {val-def-id 2} {val-def-rhs 1}
                  {post-val-def-id 2} {post-val-def-rhs 1}
                  {stx-def-id 2} {stx-def-rhs 1}]
    #:commit
    #:literals [contracted define-values define-values-for-export define-syntaxes]

    (pattern id:id
      #:attr {var-id 1} (list #'id)
      #:attr {ctc 1} (list #f)
      #:attr {val-def-id 2} '()
      #:attr {val-def-rhs 1} '()
      #:attr {post-val-def-id 2} '()
      #:attr {post-val-def-rhs 1} '()
      #:attr {stx-def-id 2} '()
      #:attr {stx-def-rhs 1} '())

    (pattern (contracted ~! {~describe "[id contract] pair" [var-id:id ctc:expr]} ...)
      #:attr {val-def-id 2} '()
      #:attr {val-def-rhs 1} '()
      #:attr {post-val-def-id 2} '()
      #:attr {post-val-def-rhs 1} '()
      #:attr {stx-def-id 2} '()
      #:attr {stx-def-rhs 1} '())

    (pattern (define-values ~! (id:id ...) rhs:expr)
      #:attr {var-id 1} '()
      #:attr {ctc 1} '()
      #:attr {val-def-id 2} (list (attribute id))
      #:attr {val-def-rhs 1} (list (attribute rhs))
      #:attr {post-val-def-id 2} '()
      #:attr {post-val-def-rhs 1} '()
      #:attr {stx-def-id 2} '()
      #:attr {stx-def-rhs 1} '())

    (pattern (define-values-for-export ~! (id:id ...) rhs:expr)
      #:attr {var-id 1} '()
      #:attr {ctc 1} '()
      #:attr {val-def-id 2} '()
      #:attr {val-def-rhs 1} '()
      #:attr {post-val-def-id 2} (list (attribute id))
      #:attr {post-val-def-rhs 1} (list (attribute rhs))
      #:attr {stx-def-id 2} '()
      #:attr {stx-def-rhs 1} '())

    (pattern (define-syntaxes ~! (id:id ...) rhs:expr)
      #:attr {var-id 1} '()
      #:attr {ctc 1} '()
      #:attr {val-def-id 2} '()
      #:attr {val-def-rhs 1} '()
      #:attr {post-val-def-id 2} '()
      #:attr {post-val-def-rhs 1} '()
      #:attr {stx-def-id 2} (list (attribute id))
      #:attr {stx-def-rhs 1} (list (attribute rhs)))

    (pattern {~describe "signature form" (form:id . _)}
      #:do [(define trans (set!-trans-extract
                           (syntax-local-value
                            ;; redirect struct~ to struct~r
                            (if (free-identifier=? #'form #'struct~)
                                #'struct~r
                                (syntax-local-introduce #'form))
                            (λ () #f))))]
      #:fail-when (and (not (signature-form? trans)) #'form) "not a signature form"
      #:and ~!
      #:do [(syntax-parse-state-cons! 'literals #'form)
            (define results ((signature-form-f trans) this-syntax intro-proc))
            (unless (list? results)
              (wrong-syntax this-syntax
                            "expected list of results from signature form, got ~e"
                            results))]
      #:with {~var || (signature-elements intro-proc)} results))

  (define-syntax-class (signature-elements intro-proc)
    #:description "signature elements"
    #:attributes [{var-id 1} {ctc 1}
                  {val-def-id 2} {val-def-rhs 1}
                  {post-val-def-id 2} {post-val-def-rhs 1}
                  {stx-def-id 2} {stx-def-rhs 1}]
    #:commit
    (pattern [{~var elem (signature-element intro-proc)} ...]
      #:attr {var-id 1} (append* (attribute elem.var-id))
      #:attr {ctc 1} (append* (attribute elem.ctc))
      #:attr {val-def-id 2} (append* (attribute elem.val-def-id))
      #:attr {val-def-rhs 1} (append* (attribute elem.val-def-rhs))
      #:attr {post-val-def-id 2} (append* (attribute elem.post-val-def-id))
      #:attr {post-val-def-rhs 1} (append* (attribute elem.post-val-def-rhs))
      #:attr {stx-def-id 2} (append* (attribute elem.stx-def-id))
      #:attr {stx-def-rhs 1} (append* (attribute elem.stx-def-rhs)))))

(define-syntax (define-signature stx)
  ;; For historical reasons, signature forms are backwards:
  ;; they're non-hygenic by default, and they accept an optional
  ;; introducer to mark introduced pieces --- but the end result
  ;; is flipped around, because we apply `intro` to the whole
  ;; signature, for the same reason as described below at
  ;; "INTRODUCED FORMS AND MACROS".
  (define intro (make-syntax-introducer))
  (syntax-parse stx
    #:track-literals
    #:literals [extends]
    [(_ sig-id:id
        {~optional {~seq extends ~! super-id:signature-id}}
        {~describe "signature elements" [given-elem ...]})
     #:with {~var elems (signature-elements intro)}
            (if (attribute super-id)
                #'[(open super-id) given-elem ...]
                #'[given-elem ...])
     #:fail-when (check-duplicate-identifier
                  (flatten (list (attribute elems.var-id)
                                 (attribute elems.val-def-id)
                                 (attribute elems.stx-def-id)
                                 ; Why not also `elem.post-val-def-id`? I’m not quite
                                 ; sure, but it breaks `struct` in signature forms if
                                 ; those identifiers are included in this check.
                                 ; Something to look into.
                                 )))
                 "duplicate identifier"
     #:with signature-tag-id (generate-temporary #'sig-id)
     #`(begin
         (define signature-tag-id (gensym))
         (define-syntax sig-id
           (make-set!-transformer
            #,(intro
               #'(make-signature
                  (make-siginfo (list (quote-syntax sig-id)
                                      {~? {~@ (quote-syntax super-id.info.id)
                                              (quote-syntax super-id.info.super-id) ...}})
                                (list (quote-syntax signature-tag-id)
                                      {~? {~@ (quote-syntax super-id.info.rtime-id) ...}}))
                  (list (quote-syntax elems.var-id) ...)
                  (list (cons (list (quote-syntax elems.val-def-id) ...)
                              (quote-syntax elems.val-def-rhs))
                        ...)
                  (list (cons (list (quote-syntax elems.stx-def-id) ...)
                              (quote-syntax elems.stx-def-rhs))
                        ...)
                  (list (cons (list (quote-syntax elems.post-val-def-id) ...)
                              (quote-syntax elems.post-val-def-rhs))
                        ...)
                  (list {~? (quote-syntax elems.ctc) #f} ...)
                  (quote-syntax sig-id)))))
         ;; dummy expression for Check Syntax:
         (define-values ()
           (begin
             (λ (elems.var-id ...)
               (letrec-syntaxes+values
                   ([(elems.stx-def-id ...) elems.stx-def-rhs] ...)
                   ([(elems.val-def-id ...) elems.val-def-rhs] ...
                    [(elems.post-val-def-id ...) elems.post-val-def-rhs] ...)
                 {~? elems.ctc} ...
                 (void)))
             (values))))]))

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
  (internal-definition-context-introduce def-ctx exp 'add))

(define-for-syntax (tagged-sigid->tagged-siginfo x)
  (cons (car x)
        (signature-siginfo (lookup-signature (cdr x)))))

(define-for-syntax (make-import-make-unboxing var renamings loc ctc)
  (if ctc
      (with-syntax ([ctc-stx (syntax-property ctc 'inferred-name var)])
        (quasisyntax/loc (error-syntax)
          (lambda (stx)
            (with-syntax ([app (datum->syntax (quote-syntax here)
                                              (list (quote-syntax #,loc))
                                              stx)])
              (syntax (let ([v/c app])
                        (if (pair? v/c)
                            (contract (let-syntax #,renamings ctc-stx) (car v/c) (cdr v/c)
                                      (current-contract-region)
                                      (quote #,var) (quote-srcloc #,var))
                            (error 'unit "contracted import ~a used before definition"
                                   (quote #,(syntax->datum var))))))))))
      (quasisyntax/loc (error-syntax)
        (lambda (stx)
          (datum->syntax (quote-syntax here)
                         (list (quote-syntax #,loc))
                         stx)))))

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

       ;; INTRODUCED FORMS AND MACROS:
       ;; We need to distinguish the original body from any
       ;; forms that are introduced from signatures 
       ;; (via `define-values`, etc., in a signature body).
       ;; The `intro` mark should be added to everything except
       ;; the introduced parts, which we implement by adding the
       ;; mark to the introduced parts and then flipping it
       ;; everywhere.
       (define intro (make-syntax-introducer #t))
       
       (with-syntax ((((dept . depr) ...)
                      (map
                       (lambda (tinfo)
                         (cons (car tinfo)
                               (syntax-local-introduce (car (siginfo-rtime-ids (cdr tinfo))))))
                       dep-tagged-siginfos))
                     [((renames (mac ...) (val ...)) ...)
                      (map (build-val+macro-defs intro) import-sigs)]
                     [(((int-ivar . ext-ivar) ...) ...) (map car import-sigs)]
                     [(((int-evar . ext-evar) ...) ...) (map car export-sigs)]
                     [((((e-post-id ...) . _) ...) ...) (map (lambda (s) (list-ref s 4)) export-sigs)]
                     [(((e-post-rhs ...) (e-ctc ...)) ...) (map build-post-val-defs+ctcs export-sigs)]
                     [((iloc ...) ...)
                      (map (lambda (x) (generate-temporaries (car x))) import-sigs)]
                     [((eloc ...) ...)
                      (map (lambda (x) (generate-temporaries (car x))) export-sigs)]
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
          (syntax-protect
          (intro
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
                                                                    (make-import-make-unboxing iv #'renamings l c))
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
                                                      (e-ctc ... ...)
                                                      (begin . body)
                                                      (define-values (e-post-id ...)
                                                        e-post-rhs) ... ...)))))
                    (unit-export ((export-key ...)
                                  (vector-immutable (λ () (check-not-unsafe-undefined (unbox eloc) 'int-evar))
                                                    ...))
                                 ...)))))))))
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
                 ;; Expand the body enough
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
                       (define (track e)
                         (syntax-case defn-or-expr ()
                           [(id . _) (syntax-track-origin e defn-or-expr #'id)]))
                       (syntax-case defn-or-expr (begin define-values define-syntaxes)
                         [(begin . l)
                          (let ([l (parameterize ((error-syntax defn-or-expr))
                                     (checked-syntax->list #'l))])
                            (expand-all (map track l)))]
                         [(define-syntaxes (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (with-syntax ([rhs (local-transformer-expand
                                              #'rhs
                                              'expression
                                              null)])
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
                            (with-syntax ([(id ...) (map (lambda (id) (syntax-local-identifier-as-binding id def-ctx))
                                                    (syntax->list #'(id ...)))])
                              (list (track #'(define-syntaxes (id ...) rhs)))))]
                         [(define-values (id ...) rhs)
                          (andmap identifier? (syntax->list #'(id ...)))
                          (begin
                            (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f def-ctx)
                            (with-syntax ([(id ...) (map (lambda (id) (syntax-local-identifier-as-binding id def-ctx))
                                                         (syntax->list #'(id ...)))])
                              (list (track #'(define-values (id ...) rhs)))))]
                         [else (list defn-or-expr)])))
                   defns&exprs)))]
              [ends-in-defn?
               (syntax-case expanded-body ()
                 [(_ ... (x . _))
                  (and (identifier? #'x)
                       (member #'x
                               (list #'define-values #'define-syntaxes #'define-syntax)
                               free-identifier=?))]
                 [_ #f])]
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

         ;; Handles redirection of exported definitions and collects
         ;; positive-blaming `contract` expressions
         (define (process-defn-or-expr defn-or-expr)
           (syntax-case defn-or-expr (define-values)
             [(define-values (id ...) body)
              (let* ([ids (syntax->list #'(id ...))]
                     [tmps (generate-temporaries ids)]
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
                                 (values
                                  #`(begin
                                      #,(quasisyntax/loc defn-or-expr
                                          (set-box! #,export-loc
                                                    #,(if ctc
                                                          #`(cons #,tmp (current-contract-region))
                                                          tmp)))
                                      #,(quasisyntax/loc defn-or-expr
                                          (define-syntax #,id
                                            (make-id-mapper (lambda (stx) (quote-syntax #,tmp))))))
                                  (and ctc
                                       #`(contract #,ctc #,tmp
                                                   (current-contract-region)
                                                   'cant-happen
                                                   (quote #,id)
                                                   (quote-srcloc #,id))))))]
                            [else (values (quasisyntax/loc defn-or-expr
                                            (define-syntax #,id
                                              (make-rename-transformer (quote-syntax #,tmp))))
                                          #f)])))])
                (define-values (defns-and-exprs ctc-exprs)
                  (for/lists [defns-and-exprs ctc-exprs]
                             ([id (in-list ids)]
                              [tmp (in-list tmps)])
                    (do-one id tmp)))
                (list (cons (syntax-track-origin
                             (quasisyntax/loc defn-or-expr
                               (define-values #,tmps
                                 #,(if (and (pair? ids) (null? (cdr ids)))
                                       (syntax-property #'body 'inferred-name (car ids))
                                       #'body)))
                             defn-or-expr
                             (syntax-case defn-or-expr () [(d-v . _) #'d-v]))
                            defns-and-exprs)
                      (filter values ctc-exprs)))]
             [else (list (list defn-or-expr) '())]))

         (internal-definition-context-track
          def-ctx
          (if (null? expanded-body)
              #'(void)
              (with-syntax ([([(defn-or-expr ...) (ctc-expr ...)] ...)
                             (map process-defn-or-expr expanded-body)])
                (if ends-in-defn?
                    #'(let ()
                        defn-or-expr ... ...
                        ctc-expr ... ...
                        (void))
                    (with-syntax ([(defn-or-expr ... last-defn-or-expr) #'(defn-or-expr ... ...)])
                      #'(let ()
                          defn-or-expr ...
                          (begin0
                            last-defn-or-expr
                            ctc-expr ... ...))))))))))))

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
                                                                      #'(current-contract-region)
                                                                      #t)])
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
            (syntax-protect
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
                                                   export-sigs)))))))))
            import-tagged-sigids
            export-tagged-sigids
            dep-tagged-sigids)))))))


(define-syntax/err-param (unit/new-import-export stx)
  (syntax-case stx ()
    ((_ . x)
     (begin
       (let-values (((u x y z) (build-unit/new-import-export (check-unit-syntax #'x))))
         u)))))

(begin-for-syntax
  (define-syntax-class tagged-link-id
    #:description "tagged link identifier"
    #:attributes [tag-id tag-sym link-id]
    #:commit
    #:literals [tag]
    (pattern (tag ~! tag-id:id link-id:id)
      #:attr tag-sym (syntax-e #'tag-id))
    (pattern link-id:id
      #:attr tag-id #f
      #:attr tag-sym #f))

  (define-syntax-class link-binding
    #:description "link binding"
    #:auto-nested-attributes
    #:commit
    #:datum-literals [:]
    (pattern [link-id:id : sig:tagged-signature-id]))

  (define-syntax-class linkage-decl
    #:description "linkage decl"
    #:auto-nested-attributes
    #:commit
    (pattern [(export:link-binding ...) unit-expr:expr import:tagged-link-id ...]))

  ;; In compound-unit, link-ids are bound by both the `import` clause and by
  ;; the exports section of a linkage-decl in a `link` clause. Information about
  ;; each link-id is stored in a link-id-binding record.
  (struct link-id-binding
    (order-index  ; The initialization order index of this link-id (0 for link-ids
                  ;   bound by imports and >0 for those bound by subunits).
     sig-id       ;\ The signature and siginfo of this link-id.
     siginfo      ;/
     access-expr) ; An expression that extracts the right signature vector
                  ;   from the appropriate unit table.
    #:transparent)

  ;; Returns #t if this link-id is bound by an import, #f if it is bound by a subunit.
  (define (link-id-binding-import? binding)
    (zero? (link-id-binding-order-index binding)))

  (define (link-id-binding->key-exprs binding tag)
    (for/list ([rtime-id (in-list (siginfo-rtime-ids (link-id-binding-siginfo binding)))])
      (build-key tag rtime-id)))

  ;; build-compound-unit : (->* [syntax?] [static-dep-info]
  ;;                            (values syntax?
  ;;                                    (listof identifier)
  ;;                                    (listof identifier)
  ;;                                    (listof identifier)))
  ;; Constructs the code for a compound-unit expression. The input syntax should
  ;; match the body of the compound-unit form (i.e. leaving off the initial
  ;; compound-unit identifier). The three additional return values are the
  ;; identifiers of the compound-unit's import and export signatures, plus
  ;; identifiers for initialization dependencies.
  (define (build-compound-unit stx [static-dep-info '()])
    (define/syntax-parse who:id (syntax-e (stx-car (error-syntax))))
    (define link-id-ctx (syntax-local-make-definition-context))
    (syntax-parse (internal-definition-context-add-scopes link-id-ctx stx)
      #:context (error-syntax)
      #:literals [import export link]
      [({~alt {~once (import in:link-binding ...)
                     #:too-few "missing import clause"
                     #:too-many "multiple import clauses"}
              {~once (export out:tagged-link-id ...)
                     #:too-few "missing export clause"
                     #:too-many "multiple export clauses"}
              {~once (link sub:linkage-decl ...)
                     #:too-few "missing link clause"
                     #:too-many "multiple link clauses"}}
        ...)
       #:fail-when (check-duplicate-identifier (append (attribute in.link-id)
                                                       (append* (attribute sub.export.link-id))))
                   "duplicate linking identifier definition"

       ;; Step 0: Bind some temporaries for use in the generated code.
       (define/syntax-parse deps-id (generate-temporary 'deps))
       (define/syntax-parse import-table-id (generate-temporary 'import-table))
       (define/syntax-parse [sub-unit-id ...] (generate-temporaries (attribute sub.unit-expr)))
       (define/syntax-parse [sub-export-table-id ...] (generate-temporaries (attribute sub.unit-expr)))

       ;; Step 1: Bind each `link-id` to a `link-id-binding` record so we can query it.
       (for ([order-index (in-naturals)]
             [link-ids (in-list (cons (attribute in.link-id)
                                      (attribute sub.export.link-id)))]
             [sig-ids (in-list (cons (attribute in.sig.sig-id)
                                     (attribute sub.export.sig.sig-id)))]
             [siginfos (in-list (cons (attribute in.sig.info)
                                      (attribute sub.export.sig.info)))]
             [tag-syms (in-list (cons (attribute in.sig.tag-sym)
                                      (attribute sub.export.sig.tag-sym)))]
             [table-id (in-list (cons (attribute import-table-id)
                                      (attribute sub-export-table-id)))])
         (for ([link-id (in-list link-ids)]
               [sig-id (in-list sig-ids)]
               [siginfo (in-list siginfos)]
               [tag-sym (in-list tag-syms)])
           (define sig-tag-id (car (siginfo-rtime-ids siginfo)))
           (syntax-local-bind-syntaxes
            (list link-id)
            #`(quote #,(link-id-binding order-index
                                        sig-id
                                        siginfo
                                        #`(hash-ref #,table-id
                                                    #,(build-key tag-sym sig-tag-id))))
            link-id-ctx)))

       (define (lookup-link link-id)
         (define v (syntax-local-value link-id (λ () #f) link-id-ctx))
         (unless (link-id-binding? v)
           (wrong-syntax link-id "unknown linking identifier"))
         v)

       ;; Step 2: Do some simple compile-time checks.
       (for ([out-link-id (in-list (attribute out.link-id))])
         (define binding (lookup-link out-link-id))
         (when (link-id-binding-import? (lookup-link out-link-id))
           (wrong-syntax out-link-id "cannot directly export an import")))

       (define (do-check-duplicate-subs source-stxs link-bindings tag-syms)
         (check-duplicate-subs
          (for/list ([link-binding (in-list link-bindings)]
                     [tag-sym (in-list tag-syms)])
            (cons tag-sym (link-id-binding-siginfo link-binding)))
          source-stxs))

       (do-check-duplicate-subs (attribute out)
                                (map lookup-link (attribute out.link-id))
                                (attribute out.tag-sym))

       ;; Step 3: Resolve import/export linkages.
       (define (get-import-output-linkage link-ids tag-syms)
         (for/list ([link-id (in-list link-ids)]
                    [tag-sym (in-list tag-syms)])
           (define binding (lookup-link link-id))
           (list* (car (siginfo-names (link-id-binding-siginfo binding)))
                  (link-id-binding-access-expr binding)
                  (link-id-binding->key-exprs binding tag-sym))))

       (define/syntax-parse ([in-name-id _ in-key-expr ...] ...)
         (get-import-output-linkage (attribute in.link-id) (attribute in.sig.tag-sym)))
       (define/syntax-parse ([out-name-id out-access-expr out-key-expr ...] ...)
         (get-import-output-linkage (attribute out.link-id) (attribute out.tag-sym)))

       ;; Step 4: Resolve sub-unit imports linkages. The `check-sub-expr`s are
       ;; evaluated when the compound-unit form is evaluated and check that the
       ;; linkages are valid, and the `sub-import-table-expr`s build the import
       ;; tables passed to the unit when it is invoked.
       (define/syntax-parse [(check-sub-expr . sub-import-table-expr) ...]
         (for/list ([order-index (in-naturals 1)] ; imports have index 0, so start at 1
                    [unit-expr (in-list (attribute sub.unit-expr))]
                    [unit-id (in-list (attribute sub-unit-id))]
                    [in-stxs (in-list (attribute sub.import))]
                    [in-link-ids (in-list (attribute sub.import.link-id))]
                    [in-tags (in-list (attribute sub.import.tag-sym))]
                    [out-link-ids (in-list (attribute sub.export.link-id))]
                    [out-tags (in-list (attribute sub.export.sig.tag-sym))])
           (define in-bindings (map lookup-link in-link-ids))
           (define out-bindings (map lookup-link out-link-ids))
           (do-check-duplicate-subs in-stxs in-bindings in-tags)

           (define (get-names+keys bindings tags)
             (map (λ (binding tag)
                    (cons (car (siginfo-names (link-id-binding-siginfo binding)))
                          (link-id-binding->key-exprs binding tag)))
                  bindings
                  tags))
           (define/syntax-parse ([in-sig-name in-key-expr ...] ...) (get-names+keys in-bindings in-tags))
           (define/syntax-parse ([out-sig-name out-key-expr ...] ...) (get-names+keys out-bindings out-tags))

           ;; Analyze this sub-unit’s position in the unit initialization order
           ;; to construct the appropriate init-dep checks.
           (define/syntax-parse ([import-dep-expr ...]
                                 [(forward-dep-key-expr . forward-dep-names-expr) ...])
             (for/fold ([import-deps '()]
                        [forward-deps '()]
                        #:result (list import-deps forward-deps))
                       ([in-link-id (in-list in-link-ids)]
                        [in-binding (in-list in-bindings)]
                        [in-tag (in-list in-tags)])
               ;; TODO: It would be nice to use the result of link-id-binding->key-exprs
               ;; here, but we can’t, because `unit-deps` uses a different format of the
               ;; form (cons/c signature-id? (or/c tag? #f)), rather than the usual
               ;; signature-key? format used everywhere else. It would be nice to fix this.
               (define siginfo (link-id-binding-siginfo in-binding))
               (define dep-key-exprs (map (λ (id) #`(cons '#,in-tag #,id)) (siginfo-rtime-ids siginfo)))

               (cond
                 [(link-id-binding-import? in-binding)
                  (values (append dep-key-exprs import-deps)
                          forward-deps)]
                 [(>= (link-id-binding-order-index in-binding) order-index)
                  (values import-deps
                          (for/fold ([forward-deps forward-deps])
                                    ([dep-key-expr (in-list dep-key-exprs)]
                                     [sig-name (in-list (siginfo-names siginfo))])
                            (cons (cons dep-key-expr #`(cons '#,sig-name '#,in-link-id))
                                  forward-deps)))]
                 [else
                  (values import-deps forward-deps)])))

           (cons
            #`(begin
                ;; check that the unit expression is actually a unit
                #,(quasisyntax/loc unit-expr
                    (check-unit #,unit-id 'who))
                ;; check that the unit imports/exports the right signatures
                #,(quasisyntax/loc unit-expr
                    (check-sigs #,unit-id
                                (vector-immutable (cons 'in-sig-name
                                                        (vector-immutable in-key-expr ...))
                                                  ...)
                                (vector-immutable (cons 'out-sig-name
                                                        (vector-immutable out-key-expr ...))
                                                  ...)
                                'who))
                ;; check that the unit’s init-depends constraints are satisfied
                #,(quasisyntax/loc unit-expr
                    (check-deps (hash {~@ forward-dep-key-expr forward-dep-names-expr} ...)
                                #,unit-id
                                'who))
                ;; record any of the unit’s init-depends on imports
                (let ([import-deps (hash {~@ import-dep-expr #t} ...)])
                  (for-each (lambda (dep)
                              (when (hash-has-key? import-deps dep)
                                (set! deps-id (cons dep deps-id))))
                            (unit-deps #,unit-id))))

            (let ()
              (define/syntax-parse (([key-expr . access-expr] ...) ...)
                (for/list ([binding (in-list in-bindings)]
                           [key-exprs (in-list (attribute in-key-expr))])
                  (define access-expr (link-id-binding-access-expr binding))
                  (map (λ (key-expr) (cons key-expr access-expr)) key-exprs)))
              #`(hash {~@ key-expr access-expr} ... ...)))))

       ;; Step 6: Assemble the generated expression.
       (define compound-unit-expr
         (quasisyntax/loc this-syntax
           (let ([deps-id '()]
                 [sub-unit-id sub.unit-expr] ...)
             check-sub-expr ...
             (make-unit
              '#,(syntax-local-infer-name (current-syntax-context))
              (vector-immutable (cons 'in-name-id (vector-immutable in-key-expr ...)) ...)
              (vector-immutable (cons 'out-name-id (vector-immutable out-key-expr ...)) ...)
              (remove-duplicates deps-id)
              (lambda ()
                (let-values ([(sub-unit-id sub-export-table-id) ((unit-go sub-unit-id))] ...)
                  (values (lambda (import-table-id)
                            (void) ; just in case there are no sub-units
                            (sub-unit-id sub-import-table-expr) ...)
                          (unit-export ([out-key-expr ...] out-access-expr) ...))))))))

       ;; Step 7: Build static information.
       ;; TODO: These values come in a strange format. Really, it would make
       ;; sense for them to match the results of `unit-static-signatures` from
       ;; racket/unit-exptime, but instead they have an extra layer of syntax
       ;; wrapping around the `car` of each pair. It would be nice to fix this.
       (define static-imports (map syntax-e (syntax->list #'(({~? in.sig.tag-id #f} . in.sig.sig-id) ...))))
       (define/syntax-parse [out-sig-id ...] (for/list ([out-link-id (in-list (attribute out.link-id))])
                                               (link-id-binding-sig-id (lookup-link out-link-id))))
       (define static-exports (map syntax-e (syntax->list #'(({~? out.tag-id #f} . out-sig-id) ...))))

       ;; We’re done!
       (values (syntax-parse-track-literals
                (syntax-protect
                 (syntax-property
                  compound-unit-expr
                  'unit:inferred-init-depends
                  (build-init-depend-property static-dep-info static-imports))))
               static-imports
               static-exports
               static-dep-info)])))

(define-syntax/err-param (compound-unit stx)
  (syntax-parse stx
    [(_ . body)
     (define-values [expr imports exports deps] (build-compound-unit #'body))
     expr]))

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
                      (map (build-val+macro-defs values) out-sigs))
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
                               (get-member-bindings def-table os #'(quote-module-name) #t))
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
       (define (push-rename-in spec renames)
         (syntax-case spec (bind-at tag)
           [(bind-at u spec)
            #`(bind-at u #,(push-rename-in #'spec renames))]
           [(tag t spec)
            #`(tag t #,(push-rename-in #'spec renames))]
           [_ #`(rename #,spec . #,renames)]))
       (with-syntax ((((int-id . ext-id) ...) int+ext-ids)
                     ((def-name ...) (generate-temporaries (map car int+ext-ids))))
         (values
          (syntax-protect
           #`(:unit (import) (export #,(push-rename-in #'export-spec #'((def-name int-id) ...)))
                    (define def-name int-id)
                    ...))
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
                                     (build #'rest))))
           (with-syntax ((((itag . isig) ...) i)
                         (((etag . esig) ...) e)
                         (((deptag . depsig) ...) d)
                         (contracted? contracted?))
             (syntax-protect
              (quasisyntax/loc (error-syntax)
                (begin
                  (define u #,exp)
                  (define-syntax name
                    (make-set!-transformer
                     (make-unit-info (quote-syntax u)
                                     (list (cons 'itag (quote-syntax isig)) ...)
                                     (list (cons 'etag (quote-syntax esig)) ...)
                                     (list (cons 'deptag (quote-syntax depsig)) ...)
                                     (quote-syntax name)
                                     contracted?))))))))))
      ((_)
       (raise-stx-err err-msg)))))

;; build-define-unit : syntax-object
;;                     (syntax-object -> (values syntax-object (listof identifier) (listof identifier) (listof identifier))
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
          (syntax-protect
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
               unit-tmp))
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
  (build-define-unit stx build-compound-unit "missing unit name"))

(define-syntax/err-param (define-unit-from-context stx)
  (build-define-unit stx (lambda (sig)
                           (check-ufc-syntax sig)
                           (build-unit-from-context sig))
                     "missing unit name and signature"))

;; A marker used when the result of invoking a unit should not be contracted
(define-for-syntax no-invoke-contract (gensym))
(define-for-syntax (build-unit/contract stx)
  (syntax-parse stx
                [(:import-clause/contract :export-clause/contract dep:dep-clause :body-clause/contract . bexps)
                 (define splicing-body-contract
                   (if (eq? (syntax-e #'b) no-invoke-contract) #'() #'(b)))
                 (let-values ([(exp isigs esigs deps) 
                               (build-unit
                                (check-unit-syntax
                                 (syntax/loc stx
                                   ((import i.s ...) (export e.s ...) dep . bexps))))])
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
                                     (quasisyntax/loc stx
                                       ((import (import-tagged-sig-id [i.x i.c] ...) ...)
                                        (export (export-tagged-sig-id [e.x e.c] ...) ...)
                                        dep
                                        #,@splicing-body-contract)))])
                       (values
                        (syntax-protect
                         (syntax/loc stx
                           (contract unit-contract new-unit '(unit name) (current-contract-region) (quote name) (quote-srcloc name))))
                        isigs esigs deps))))]
                [(ic:import-clause/contract ec:export-clause/contract dep:dep-clause . bexps)
                 (build-unit/contract
                  (quasisyntax/loc stx
                    (ic ec dep #:invoke/contract #,no-invoke-contract . bexps)))]
                [(ic:import-clause/contract ec:export-clause/contract bc:body-clause/contract . bexps)
                 (build-unit/contract
                  (quasisyntax/loc stx
                    (ic ec (init-depend) #,@(syntax->list #'bc) . bexps)))]
                [(ic:import-clause/contract ec:export-clause/contract . bexps)
                 (build-unit/contract
                  (quasisyntax/loc stx
                    (ic ec (init-depend) #:invoke/contract #,no-invoke-contract . bexps)))]))

(define-syntax/err-param (define-unit/contract stx)
  (build-define-unit/contracted stx (λ (stx)
                                      (build-unit/contract stx))
                                "missing unit name"))

(define-for-syntax (unprocess-tagged-id ti)
  (if (car ti)
      #`(tag #,(car ti) #,(cdr ti))
      (cdr ti)))

(define-for-syntax (temp-id-with-tags id i)
  (let loop ([i i] [at #f])
    (syntax-case i (tag bind-at)
      [(tag t sig)
       (list id #`(tag t #,id) (let ([l #'(tag t sig)])
                                 (if at
                                     #`(bind-at #,at #,l)
                                     l)))]
      [(bind-at u i)
       (loop #'i #'u)]
      [_else
       (list id id (if at
                       #`(bind-at #,at #,i)
                       i))])))

(define-syntax/err-param (define-values/invoke-unit stx)
  (syntax-case stx (import export)
    ((_ u (import) (export e ...))
     (quasisyntax/loc stx
       (define-values/invoke-unit/core u e ...)))
    ((_ u (import i ...) (export e ...))
     (with-syntax ((((EU EUl e) ...) (map temp-id-with-tags
                                          (generate-temporaries #'(e ...))
                                          (syntax->list #'(e ...))))
                   (((IU IUl i) ...) (map temp-id-with-tags
                                          (generate-temporaries #'(i ...))
                                          (syntax->list #'(i ...))))
                   ((iu ...) (generate-temporaries #'(i ...)))
                   ((i-id ...) (map (lambda (p) (unprocess-tagged-id (cadr p)))
                                    (map process-tagged-import
                                         (syntax->list #'(i ...)))))
                   ((e-id ...) (map (lambda (p) (unprocess-tagged-id (cadr p)))
                                    (map process-tagged-export
                                         (syntax->list #'(e ...))))))
       (quasisyntax/loc stx
         (begin
           (define-unit-from-context iu i)
           ...
           (define-compound-unit u2 (import)
             (export EUl ...)
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
    (((import-clause ...) 
      (export-clause ...)
      (((out ...) u l ...) ...))
     (let* ([us (syntax->list #'(u ...))]
            [units (map lookup-def-unit us)]
            [import-sigs (map process-signature 
                              (syntax->list #'(import-clause ...)))]
            [sig-introducers (map (lambda (unit u) values) units us)]
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
               (syntax->list #'(export-clause ...)))])
         
         (define init-deps
           (for/fold ([init-deps '()]) ([u (in-list units)]
                                        [sub-in (in-list sub-ins)]
                                        [u-pos (in-naturals)])
             (for/fold ([init-deps init-deps]) ([dep (in-list (unit-info-deps u))])
               ;; Find the link for this dependency:
               (define lr
                 (for/or ([lr (in-list sub-in)])
                   (and (eq? (link-record-tag lr)
                             (car dep))
                        (siginfo-subtype (signature-siginfo (lookup-signature (link-record-sigid lr)))
                                         (signature-siginfo (lookup-signature (cdr dep))))
                        lr)))
               ;; If `lr` refers to an import, then propoagate the dependency.
               ;; If it refers to a linked unit, make sure that unit is earlier.
               (cond
                [(for/or ([import-sig (in-list import-sigs)])
                   (and (free-identifier=? (link-record-linkid import-sig)
                                           (link-record-linkid lr))
                        import-sig))
                 ;; imported
                 => (lambda (import-sig)
                      (cons (cons (link-record-tag import-sig)
                                  (link-record-sigid import-sig))
                            init-deps))]
                [(for/or ([sub-out (in-list sub-outs)]
                          [i-pos (in-naturals)])
                   (for/or ([olr (in-list sub-out)])
                     (and (free-identifier=? (link-record-linkid olr)
                                             (link-record-linkid lr))
                          i-pos)))
                 => (lambda (i-pos)
                      (unless (i-pos . < . u-pos)
                        (raise-stx-err "unit depends on initialization of later unit" 
                                       (link-record-linkid lr)))
                      init-deps)]
                [else
                 (error "internal error: cannot find link source for init-dependency check")]))))
         
         (with-syntax (((import-clause ...)
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
                                         (quasisyntax/loc stx #,(syntax-local-introduce (unit-info-unit-id u))))
                                       units (syntax->list #'(u ...)))))
           (build-compound-unit #`((import import-clause ...)
                                   (export #,@exports)
                                   (link ((out ...) unit-id in ...) ...))
                                init-deps)))))
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
           [unprocess (lambda (p)
                        #`(bind-at #,u #,(unprocess-tagged-id p)))]
           [isigs (map unprocess (unit-info-import-sig-ids ui))]
           [esigs (map (if exports unprocess-tagged-id unprocess) (unit-info-export-sig-ids ui))])
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
             (syntax-protect
              (if define?
                  (syntax/loc (error-syntax) (define-values/invoke-unit u (import isig ...) (export esig ...)))
                  (syntax/loc (error-syntax) (invoke-unit u (import isig ...)))))))]
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
                                                 (link unit ...))))])
                                u)])
               (syntax-protect
                (if define?
                    (syntax/loc (error-syntax)
                      (define-values/invoke-unit u
                        (import isig ...) (export esig ...)))
                    (syntax/loc (error-syntax)
                      (invoke-unit u
                                   (import isig ...))))))))]
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
