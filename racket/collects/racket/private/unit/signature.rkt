#lang racket/base

;; This module implements signature-related forms.

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info
                     racket/syntax
                     syntax/parse/pre
                     syntax/private/struct
                     syntax/stx
                     "exptime/import-export.rkt"
                     "exptime/signature.rkt")
         racket/contract/base
         (rename-in racket/private/struct [struct struct~])
         "keywords.rkt"
         "util.rkt")

(provide define-signature
         provide-signature-elements

         define-signature-form
         define-values-for-export
         open
         (rename-out [struct~r/ctc struct/ctc]))

(module+ compat
  ;; export only for compatibility with `mzlib/unit`
  (provide (protect-out struct) struct/ctc
           (protect-out struct~s) struct~r
           (protect-out struct~s/ctc) struct~r/ctc))

;; -----------------------------------------------------------------------------
;; define-signature

(begin-for-syntax
  ;; (make-signature-form (syntax? -> any))
  (define-struct signature-form (f)
    #:property prop:procedure
    (lambda (_ stx)
      (parameterize ((current-syntax-context stx))
        (raise-stx-err "illegal use of signature form"))))

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
                                 ; Why not also `elem.post-val-def-id`? Because export
                                 ; definitions don’t conflict with signature elements, see
                                 ; Note [Generated export definitions] in "exptime/signature.rkt".
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
                                      {~? {~@ (quote-syntax super-id.info.id) ...}})
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
                  (list {~? (quote-syntax elems.ctc) #f} ...)))))
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

(define-syntax provide-signature-elements
  (syntax-parser
    #:track-literals
    [(_ spec:import-spec ...)
     (define nameses (map signature-ie-int-names (attribute spec.value)))
     ;; Export only the names that would be visible to uses
     ;;  with the same lexical context as p. Otherwise, we
     ;;  can end up with collisions with renamings that are
     ;;  symbolically the same, such as those introduced by
     ;;  `open'.
     (define names
       (append* (for/list ([sig-stx (in-list (attribute spec))]
                           [names (in-list nameses)])
                  (filter (lambda (name)
                            (bound-identifier=?
                             name
                             (datum->syntax sig-stx (syntax-e name))))
                          names))))

     (define dup (check-duplicate-identifier names))
     (when dup
       (raise-stx-err (format "duplicate binding for ~.s" (syntax-e dup))))

     (quasisyntax/loc this-syntax
       (provide #,@names))]))

;; -----------------------------------------------------------------------------
;; signature forms

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

(define-signature-form (define-values-for-export stx)
  (raise-syntax-error #f "internal error" stx))

(define-signature-form (open stx enclosing-intro)
  (syntax-parse stx
    [(_ spec:export-spec)
     (define sig (attribute spec.value))
     (define/syntax-parse [rename-bind [stx-bind ...] [val-bind ...]] ((build-val+macro-defs enclosing-intro) sig))
     (define-values [post-rhss ctcs] (build-post-val-defs+ctcs sig))
     (define/syntax-parse [post-rhs ...] post-rhss)
     (define/syntax-parse [sig-elem ...] (for/list ([int-id (in-list (attribute spec.var.int-id))]
                                                    [ctc (in-list ctcs)])
                                           (if ctc
                                               #`(contracted [#,int-id #,ctc])
                                               int-id)))
     (syntax->list
      #'[sig-elem ...
         (define-syntaxes . rename-bind)
         (define-syntaxes . stx-bind) ...
         (define-values . val-bind) ...
         (define-values-for-export [spec.post-def.id ...] post-rhs) ...])]))

(begin-for-syntax
 (define-struct self-name-struct-info (id)
   #:super struct:struct-info
   #:property prop:procedure
   (lambda (me stx)
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
  (define-signature-form (struct stx)
    (parameterize ((current-syntax-context stx))
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

;; this binding is used by `racket/unit` for `define-signature`
(define-signature-form (struct~r stx)
  (do-struct~ stx #f))

(module+ compat
  (define-signature-form (struct~s stx)
    (do-struct~ stx #t)))

(define-signature-form (struct/ctc stx)
  (parameterize ((current-syntax-context stx))
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

(define-signature-form (struct~r/ctc stx)
  (do-struct~/ctc stx #f))

(module+ compat
  (define-signature-form (struct~s/ctc stx)
    (do-struct~/ctc stx #t)))
