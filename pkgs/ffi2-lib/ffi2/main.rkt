#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (rename-in '#%foreign
                    [ffi2-lib-ref ffi2-lib-ref*]
                    [ffi2-free ffi2-free*]
                    [ffi2-memcpy ffi2-memcpy*]
                    [ffi2-memmove ffi2-memmove*]
                    [ffi2-memset ffi2-memset*])
         ffi/unsafe/private/not-available
         racket/fixnum
         "private/type.rkt"
         "private/base-type.rkt"
         "private/system-type-case.rkt"
         "private/error.rkt"
         "private/lib.rkt"
         "private/cpointer.rkt")

(provide ffi2-lib
         ffi2-lib?
         ffi2-lib-ref
         define-ffi2-type
         define-ffi2-type-syntax
         define-ffi2-abi
         (protect-out
          ffi2-procedure
          define-ffi2-procedure
          ffi2-callback
          define-ffi2-definer
          ffi2-ref
          ffi2-set!
          ffi2-cast
          ffi2-add
          ffi2-malloc
          ffi2-free
          ffi2-sizeof
          ffi2-offsetof
          ffi2-memcpy
          ffi2-memmove
          ffi2-memset)
         ->
         struct
         union
         array
         system-type-case
         default_abi
         cdecl_abi
         stdcall_abi
         ptr_t?
         ptr_t/gcable?
         void_t*?
         void_t*/gcable?
         uintptr->ptr_t
         ptr_t->uintptr
         ptr_t->cpointer
         cpointer->ptr_t
         make-not-available
         (all-from-out "private/base-type.rkt"))

(begin-for-syntax
  (define-syntax-class (:abi stx)
    #:attributes (a)
    (pattern name:id
             #:do [(define abi (syntax-local-value #'name (lambda () #f)))
                   (unless (procedure-abi? abi)
                     (raise-syntax-error #f "expected an ffi2 abi" stx #'name))]
             #:attr a (procedure-abi-vm-abi abi))
    (pattern (~and all ((~datum system-type-case) . _))
             #:attr a (parse-system-type-case/abi #'all)))

  (define-syntax-class :maybe-type
    #:description "an ffi2 type"
    #:literals (-> struct union array)
    (pattern t:id
             #:when (ffi2-type-or-constructor-or-macro? (syntax-local-value #'t (lambda () #f))))
    (pattern (t:id _ ...)
             #:when (ffi2-type-or-constructor-or-macro? (syntax-local-value #'t (lambda () #f))))
    (pattern (-> _ ...))
    (pattern (struct _ ...))
    (pattern (union _ ...))
    (pattern (array _ ...)))

  (struct arrow-type (in-ts out-t convs errno? async-apply?))

  (define-syntax-class (:arrow-type stx)
    #:description "an ffi2 arrow type"
    #:literals (->)
    #:attributes (t)
    (pattern (-> ~!
                 in-maybe-type::maybe-type ...
                 (~optional (~seq #:varargs var-in-maybe-type::maybe-type ...))
                 out-maybe-type::maybe-type (~optional (~and errno (~or #:errno
                                                                        #:get-last-error)))
                 (~alt (~optional (~seq #:abi (~var abi (:abi stx))))
                       (~optional (~and atomic #:atomic))
                       (~optional (~and collect-safe #:collect-safe))
                       (~optional (~and callback-exns #:allow-callback-exn))
                       (~optional (~and in-original #:in-original)))
                 ...)
             #:with ((~var in-type (:type stx #f #t)) ...) #'(in-maybe-type ...)
             #:with ((~var var-in-type (:type stx #f #t)) ...) (if (attribute var-in-maybe-type)
                                                                   #'(var-in-maybe-type ...)
                                                                   #'())
             #:with (~var out-type (:type stx #t)) #'out-maybe-type
             #:attr t (arrow-type (append (attribute in-type.t)
                                          (attribute var-in-type.t))
                                  (attribute out-type.t)
                                  (append
                                   (if (attribute abi)
                                       (list (attribute abi.a))
                                       null)
                                   (if (attribute var-in-maybe-type)
                                       (list (list '__varargs_after (length (attribute in-maybe-type))))
                                       null)
                                   (if (and (attribute atomic)
                                            ;; not inherently incompatible with `atomic`,
                                            ;; but not supported in combination at lower levels:
                                            (not (attribute collect-safe)))
                                       (list '__atomic)
                                       null)
                                   (if (attribute collect-safe)
                                       (list '__collect_safe)
                                       null)
                                   (if (attribute callback-exns)
                                       (list '__callback_exns)
                                       null)
                                   (if (attribute in-original)
                                       (if (attribute callback-exns)
                                           (raise-syntax-error #f
                                                               "incompatible with collect-safe mode"
                                                               stx
                                                               #'in-original)
                                           (list '__original_place))
                                       null)
                                   (if (attribute errno)
                                       (list (if (eq? (syntax-e #'errno) '#:errno)
                                                 '__errno
                                                 '(__select os (windows) __get_last_error __errno)))
                                       null))
                                  (and (attribute errno) #t)
                                  (and (attribute in-original) #t))))

  (define-syntax-class (:type stx [for-return? #f] [for-argument? #f])
    #:description "an ffi2 type"
    #:attributes (t)
    #:literals (-> struct union array system-type-case)
    (pattern type-name:id
             #:attr t (expand-type stx #'type-name #'type-name
                                   #:for-return? for-return?
                                   #:for-argument? for-argument?))
    (pattern (~and all (-> ~! . _))
             #:with (~var a (:arrow-type stx)) #'all
             #:with arity #`#,(length (arrow-type-in-ts (attribute a.t)))
             #:attr t (make-ffi2-type #f 'pointer #'(lambda (proc)
                                                      (and (procedure? proc)
                                                           (procedure-arity-includes? proc arity)))
                                      #:racket->c #`(lambda (proc)
                                                      #,(build-ffi2-callback '-> #'proc (attribute a.t)))
                                      #:c->racket #`(lambda (ptr)
                                                      #,(build-ffi2-procedure '-> #'ptr (attribute a.t) #t))
                                      #:release #'black-box
                                      #:category 'arrow))
    (pattern ((~and compound (~or struct union)) ~!
                                                 (~optional tag:id)
                                                 [field-name:id (~var field-type (:type stx))]
                                                 ...)
             #:with (field-vm-type ...) (map ffi2-type-vm-type (attribute field-type.t))
             #:with tag*s (if (attribute tag)
                              #`(#,(string->symbol (format "~a*" (syntax-e #'tag))))
                              #'())
             #:attr t (make-ffi2-type (syntax-e #'(~? tag compound)) (syntax->datum #'(compound tag*s (field-name field-vm-type) ...))
                                      (if (attribute tag)
                                          #'(lambda (v)
                                              (or ((#%foreign-inline (ffi2-ptr?-maker pointer tag*s) #:copy*) v)
                                                  ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tag*s) #:copy*) v)))
                                          #'ffi2-ptr?)
                                      #:release #'black-box))
    (pattern (array ~! (~var elem-type (:type stx)) n::array-size)
             #:with tag*s (let ([ptr-vm-type (ffi2-type-pointer-vm-type (attribute elem-type.t))])
                            (if (pair? ptr-vm-type)
                                (cadr ptr-vm-type)
                                '()))
             #:attr t (make-ffi2-type #f (if (eq? '* (syntax-e #'n))
                                             `(pointer ,(syntax->datum #'tag*s))
                                             `(array ,(syntax->datum #'tag*s) ,(syntax-e #'n) ,(ffi2-type-vm-type (attribute elem-type.t))))
                                      (if (null? (syntax-e #'tag*s))
                                          #'ffi2-ptr?
                                          #'(lambda (v)
                                              (or ((#%foreign-inline (ffi2-ptr?-maker pointer tag*s) #:copy*) v)
                                                  ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tag*s) #:copy*) v))))
                                      #:release #'black-box))
    (pattern (~and all (system-type-case . _))
             #:attr t (parse-system-type-case/type #'all))
    (pattern (~and all (type-ctr-name:id . _))
             #:attr t (expand-type stx #'type-ctr-name #'all
                                   #:for-return? for-return?
                                   #:for-argument? for-argument?))))

(define-syntax (define-ffi2-abi stx)
  (syntax-parse stx
    [(_ name:id (~var abi (:abi stx)))
     #`(define-syntax name (procedure-abi '#,(attribute abi.a)))]))

(define-syntax (define-ffi2-type stx)
  (syntax-parse stx
    #:literals (struct union array)
    [(_ name:id ((~and compound (~or (~and is-s? struct) (~and is-u? union)))
                 (~optional tag:id)
                 [field-name:id (~var field-type (:type stx))]
                 ...))
     (with-syntax ([name* (datum->syntax #'name
                                         (string->symbol (format "~a*" (syntax-e #'name)))
                                         #'name)]
                   [name*/gcable (datum->syntax #'name
                                                (string->symbol (format "~a*/gcable" (syntax-e #'name)))
                                                #'name)]
                   [tag* (string->symbol (format "~a*" (syntax-e #'(~? tag name))))]
                   [fill-name (if (attribute is-s?)
                                  (car (generate-temporaries (list (format "fill-~a" (syntax-e #'name)))))
                                  #f)]
                   [(fill-field-name ...) (if (attribute is-u?)
                                              (generate-temporaries (for/list ([field-name (in-list (attribute field-name))])
                                                                      (format "fill-~a-~a" (syntax-e #'name) (syntax-e field-name))))
                                              '())]
                   [tag-ptr? (datum->syntax #'name
                                            (string->symbol (format "~a*?" (syntax-e #'name)))
                                            #'name)]
                   [tag-ptr?-str (format "~a*?" (syntax-e #'name))]
                   [([field-vm-type (field-defn ...) field-c->racket field-racket->c field-ok? field-type-name
                                    field-compound? field-ptr-vm-type
                                    field-release]
                     ...)
                    (map (lambda (t)
                           (list (ffi2-type-vm-type t)
                                 (ffi2-type-defns t)
                                 (ffi2-type-c->racket t)
                                 (ffi2-type-racket->c t)
                                 (ffi2-type-predicate t)
                                 (ffi2-type-name t)
                                 (ffi2-type-compound? t)
                                 (ffi2-type-pointer-vm-type t)
                                 (ffi2-type-release t)))
                         (attribute field-type.t))]
                   [(name-field ...) (map (lambda (field-name)
                                            (datum->syntax field-name
                                                           (string->symbol (format "~a-~a" (syntax-e #'name) (syntax-e field-name)))
                                                           field-name))
                                          (attribute field-name))]
                   [(set-name-field! ...) (map (lambda (field-name)
                                                 (datum->syntax field-name
                                                                (string->symbol (format "set-~a-~a!" (syntax-e #'name) (syntax-e field-name)))
                                                                field-name))
                                               (attribute field-name))]
                   [(set-name-field!/unchecked ...) (generate-temporaries #'(field-name ...))])
       #'(begin
           (define (tag-ptr? v) (or ((#%foreign-inline (ffi2-ptr?-maker pointer (tag*)) #:copy*) v)
                                    ((#%foreign-inline (ffi2-ptr?-maker pointer/gc (tag*)) #:copy*) v)))
           (define-syntax name* (make-ffi2-type 'name* '(pointer (tag*)) #'tag-ptr?
                                                #:release #'black-box
                                                #:category 'ptr))
           (define-syntax name*/gcable (make-ffi2-type 'name*/gcable '(pointer/gc (tag*)) #'tag-ptr?
                                                       #:release #'black-box
                                                       #:category 'ptr))
           (define-syntax name
             (make-ffi2-type 'name '(compound (tag*) (field-name field-vm-type) ...) #'tag-ptr?
                             #:release #'black-box
                             #:procedure
                             (lambda (stx)
                               (~? (syntax-parse stx
                                     [(_ (~optional kind::malloc-kind) field-name ...)
                                      'is-s?
                                      (with-syntax ([kind (or (attribute kind) #'#:gcable)])
                                        #'(fill-name (ffi2-malloc kind name)
                                                     field-name ...))])
                                   (syntax-parse stx
                                     [(_ (~datum field-name) (~optional kind::malloc-kind) expr)
                                      (with-syntax ([kind (or (attribute kind) #'#:gcable)])
                                        #'(fill-field-name (ffi2-malloc kind name)
                                                           expr))]
                                     ...)))))
           field-defn ... ...
           (define (name-field v)
             (unless (tag-ptr? v) (raise-argument-error 'name-field tag-ptr?-str v))
             (do-ffi2-ptr-ref field-compound?
                              field-ptr-vm-type
                              field-c->racket field-vm-type
                              v (~? (begin 'is-u? 0)
                                    (#%foreign-inline (ffi2-offsetof (compound (tag*) (field-name field-vm-type) ...) field-name) #:copy))))
           ...
           (define (set-name-field!/unchecked v val)
             (do-ffi2-ptr-set! field-compound?
                               field-racket->c field-vm-type
                               v (~? (begin 'is-u? 0)
                                     (#%foreign-inline (ffi2-offsetof (compound (tag*) (field-name field-vm-type) ...) field-name) #:copy))
                               val
                               field-release))
           ...
           (define (set-name-field! v val)
             (unless (tag-ptr? v) (raise-argument-error 'set-name-field! tag-ptr?-str v))
             (unless (field-ok? val) (bad-assign-value 'set-name-field! 'field-type-name val))
             (set-name-field!/unchecked v val))
           ...
           (~? (define (fill-name p field-name ...)
                 'is-s?
                 (unless (field-ok? field-name) (bad-assign-value 'name 'field-type-name field-name))
                 ...
                 (set-name-field!/unchecked p field-name)
                 ...
                 p)
               (begin
                 (define (fill-field-name p v)
                   (unless (field-ok? v) (bad-assign-value 'name 'field-type-name v))
                   (set-name-field!/unchecked p v)
                   p)
                 ...))))]
    [(_ name:id (array (~var elem-type (:type stx)) n::array-size)
        (~optional (~seq #:tag tag:id)))
     (define elem-t (attribute elem-type.t))
     (with-syntax ([tag*s (if (attribute tag)
                              (list (attribute tag))
                              (let ([ptr-vm-type (ffi2-type-pointer-vm-type elem-t)])
                                (if (pair? ptr-vm-type)
                                    (let ([tags (cadr ptr-vm-type)])
                                      (if (and (pair? tags)
                                               (eq? (syntax-e #'name) (car tags)))
                                          tags
                                          (cons #'name tags)))
                                    (list #'name))))]
                   [tag-ptr? (datum->syntax #'name
                                            (string->symbol (format "~a?" (syntax-e #'name)))
                                            #'name)]
                   [tag-ptr?-str (format "~a*?" (syntax-e #'name))]
                   [name/gcable (datum->syntax #'name
                                               (string->symbol (format "~a/gcable" (syntax-e #'name)))
                                               #'name)]
                   [name-set! (datum->syntax #'name
                                             (string->symbol (format "~a-set!" (syntax-e #'name)))
                                             #'name)]
                   [name-ref (datum->syntax #'name
                                            (string->symbol (format "~a-ref" (syntax-e #'name)))
                                            #'name)]
                   [range-str (and (not (eq? '* (syntax-e #'n)))
                                   (format "(integer-in 0 ~a)" (sub1 (syntax-e #'n))))])
       #`(begin
           (define (tag-ptr? v) #,(if (null? (syntax-e #'tag*s))
                                      #'(ffi2-ptr? v)
                                      #`(or ((#%foreign-inline (ffi2-ptr?-maker pointer tag*s) #:copy*) v)
                                            ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tag*s) #:copy*) v))))
           (define-syntax name
             (make-ffi2-type 'name #,(if (eq? '* (syntax-e #'n))
                                         #`'(pointer tag*s)
                                         #`'(array tag*s n #,(ffi2-type-vm-type elem-t))) #'tag-ptr?
                             #:release #'black-box
                             #:category #,(if (eq? '* (syntax-e #'n))
                                              #''ptr
                                              #'#f)))
           #,@(if (eq? '* (syntax-e #'n))
                  #`((define-syntax name/gcable
                       (make-ffi2-type 'name '(pointer/gc tag*s) #'tag-ptr?
                                       #:release #'black-box
                                       #:category 'ptr)))
                  '())
           #,@(ffi2-type-defns elem-t)
           (define (name-ref ptr idx)
             (unless (tag-ptr? ptr) (raise-argument-error 'name-ref tag-ptr?-str ptr))
             #,(if (eq? '* (syntax-e #'n))
                   #`(unless (exact-integer? idx) (raise-argument-error 'name-ref "exact-integer?" idx))
                   #`(unless (and (fixnum? idx) (fx<= 0 idx (sub1 n))) (raise-argument-error 'name-ref 'range-str idx)))
             (do-ffi2-ptr-ref #,(ffi2-type-compound? elem-t)
                              #,(ffi2-type-pointer-vm-type elem-t)
                              #,(ffi2-type-c->racket elem-t) #,(ffi2-type-vm-type elem-t)
                              ptr (* idx (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type elem-t)) #:copy))))
           (define (name-set! ptr idx val)
             (unless (tag-ptr? ptr) (raise-argument-error 'name-set! tag-ptr?-str ptr))
             #,(if (eq? '* (syntax-e #'n))
                   #`(unless (exact-integer? idx) (raise-argument-error 'name-ref "exact-integer?" idx))
                   #`(unless (and (fixnum? idx) (fx<= 0 idx (sub1 n))) (raise-argument-error 'name-set! 'range-str idx)))
             (unless (#,(ffi2-type-predicate elem-t) val) (bad-assign-value 'name-set! '#,(ffi2-type-name elem-t) val))
             (do-ffi2-ptr-set! #,(ffi2-type-compound? elem-t)
                               #,(ffi2-type-racket->c elem-t) #,(ffi2-type-vm-type elem-t)
                               ptr (* idx (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type elem-t)) #:copy)) val
                               #,(ffi2-type-release elem-t)))))]
    [(form-id (~or name:id
                   (name:id arg:id ...))
              (~var parent (:type stx))
              (~alt (~optional (~seq #:tag tag::tag))
                    (~optional (~seq #:predicate predicate-expr:expr))
                    (~optional (~seq #:racket->c racket->c-expr:expr))
                    (~optional (~seq #:c->racket c->racket-expr:expr))
                    (~optional (~seq #:release release-expr:expr)))
              ...)
     (define parent-t (attribute parent.t))
     (with-syntax ([name? (datum->syntax #'name
                                         (string->symbol (format "~a?" (syntax-e #'name)))
                                         #'name)])
       (cond
         [(and (ffi2-type-immediate-pointer? parent-t)
               (not (or (attribute arg)
                        (attribute racket->c-expr)
                        (attribute c->racket-expr)
                        (attribute release-expr))))
          (with-syntax ([name/gcable (datum->syntax #'name
                                                    (string->symbol (format "~a/gcable" (syntax-e #'name)))
                                                    #'name)]
                        [tags (let ([parent-tags (let ([vm-type (ffi2-type-vm-type parent-t)])
                                                   (if (pair? vm-type)
                                                       (cadr vm-type)
                                                       null))])
                                (if (or (not (attribute tag))
                                        (syntax-e #'tag))
                                    (cons #'(~? tag name)
                                          parent-tags)
                                    parent-tags))]
                        [(name-ptr? predicate-def ...) (if (attribute predicate-expr)
                                                           #'(name-ptr?
                                                              (define name? predicate-expr))
                                                           #'(name?))])
            #'(begin
                (define (name-ptr? v) (or ((#%foreign-inline (ffi2-ptr?-maker pointer tags) #:copy*) v)
                                          ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tags) #:copy*) v)))
                (define-syntax name (make-ffi2-type 'name '(pointer tags) #'name?
                                                    #:release #'black-box
                                                    #:category 'ptr))
                (define-syntax name/gcable (make-ffi2-type 'name/gcable '(pointer/gc tags) #'name?
                                                           #:release #'black-box
                                                           #:category 'ptr))
                predicate-def ...))]
         [else
          (when (and (attribute tag)
                     (syntax-e #'tag))
            (raise-syntax-error #f "base type for new tag is not an immediate pointer type" stx #'parent))
          (with-syntax ([([wrapper-pre-def wrapper-def wrapper ...] ...)
                         (append (if (attribute racket->c-expr)
                                     (list
                                      (with-syntax ([name-ptr? (if (attribute predicate-expr)
                                                                   #'name-ptr?
                                                                   #'name?)])
                                        #'((define-syntaxes (new-racket->c) (values))
                                           (define new-racket->c (compose-racket->c 'form-id racket->c-expr name-ptr?))
                                           #:racket->c (quote-syntax new-racket->c))))
                                     null)
                                 (if (attribute c->racket-expr)
                                     (list
                                      #'((define-syntaxes (new-c->racket) (values))
                                         (define new-c->racket (check-c->racket 'form-id c->racket-expr))
                                         #:c->racket (quote-syntax new-c->racket)))
                                     null)
                                 (if (attribute release-expr)
                                     (list
                                      #'((define-syntaxes (new-release) (values))
                                         (define new-release (check-release 'form-id release-expr))
                                         #:release (quote-syntax new-release)))
                                     null))]
                        [new-racket->c (if (attribute racket->c-expr)
                                           #'new-racket->c
                                           #'begin)]
                        [new-c->racket (if (attribute c->racket-expr)
                                           #'new-c->racket
                                           #'begin)]
                        [new-release (if (attribute release-expr)
                                         #'new-release
                                         #'begin)]
                        [(name-ptr? predicate-def ...)
                         (if (attribute predicate-expr)
                             #'(name-ptr?
                                (define name? predicate-expr))
                             #'(name?))])
            (with-syntax ([(body-defn ...)
                           #`(wrapper-def
                              ...
                              predicate-def
                              ...
                              (define (racket->c v) (#,(ffi2-type-racket->c parent-t) (new-racket->c v)))
                              (define (c->racket v) (new-c->racket (#,(ffi2-type-c->racket parent-t) v)))
                              (define (release v) (new-release (#,(ffi2-type-release parent-t) v))))])
              (if (attribute arg)
                  (with-syntax ([arity (length (attribute arg))])
                    #`(begin
                        (define-syntax name (ffi2-type-constructor
                                             (make-type-maker #'make-procs arity
                                                              'name
                                                              '#,(ffi2-type-vm-type parent-t)
                                                              '#,(ffi2-type-category parent-t))))
                        (define (make-procs arg ...)
                          #,@(ffi2-type-defns parent-t)
                          (define (name-ptr? v) (#,(ffi2-type-predicate parent-t) v))
                          body-defn
                          ...
                          (values name?
                                  racket->c
                                  c->racket
                                  release))))
                #`(begin
                    #,@(ffi2-type-defns parent-t)
                    (define (name-ptr? v) (#,(ffi2-type-predicate parent-t) v))
                    (define-syntax name (make-ffi2-type 'name '#,(ffi2-type-vm-type parent-t) #'name?
                                                        #:category '#,(ffi2-type-category parent-t)
                                                        #:racket->c #'racket->c
                                                        #:c->racket #'c->racket
                                                        #:release #'release))
                    body-defn
                    ...))))]))]))

(define-syntax (define-ffi2-type-syntax stx)
  (syntax-parse stx
    [(form-id name:id rhs:expr)
     #'(define-syntax name (make-ffi2-type-macro 'form-id rhs))]
    [(form-id (name:id arg ...) body ...)
     #'(define-syntax name (make-ffi2-type-macro 'form-id (lambda (arg ...) body ...)))]))

(define-for-syntax (make-type-maker maker-id arity name vm-type category)
  (lambda (stx arg-exprs)
    (unless (= (length arg-exprs) arity)
      (raise-syntax-error #f
                          "incorrect number of arguments to ffi2 type constructor"
                          stx))
    (with-syntax ([(name? racket->c c->racket release)
                   (generate-temporaries '(name? racket->c c->racket release))])
      (make-ffi2-type name vm-type #'name?
                      #:category category
                      #:defns #`((define-values (name? racket->c c->racket release)
                                   (#,maker-id #,@arg-exprs)))
                      #:racket->c #'racket->c
                      #:c->racket #'c->racket
                      #:release #'release))))

(define-syntax (ffi2-ref stx)
  (syntax-parse stx
    [(form-id ptr-expr:expr (~var type (:type stx)) (~optional (~seq offset-expr:expr (~optional (~and abs #:bytes)))))
     (define t (attribute type.t))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ptr_t?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         #,@(ffi2-type-defns t)
         (do-ffi2-ptr-ref #,(ffi2-type-compound? t)
                          #,(ffi2-type-pointer-vm-type t)
                          #,(ffi2-type-c->racket t)  #,(ffi2-type-vm-type t)
                          ptr #,(if (attribute abs)
                                    #'offset
                                    #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy)))))]))

(define-syntax (do-ffi2-ptr-ref stx)
  (syntax-parse stx
    [(_ field-compound?
        field-ptr-vm-type
        field-c->racket field-vm-type
        ptr offset)
     (if (syntax-e #'field-compound?)
         (with-syntax ([field-ptr-vm-type/gcable (pointer-vm-type->gcable (syntax->datum #'field-ptr-vm-type))])
           #`((#%foreign-inline (ffi2-ptr-cast-maker field-ptr-vm-type field-ptr-vm-type/gcable) #:copy*)
              ptr
              offset))
         #'(field-c->racket
            ((#%foreign-inline (begin-unsafe (ffi2-ptr-ref-maker field-vm-type)) #:copy)
             ptr
             offset)))]))

(define-syntax (ffi2-set! stx)
  (syntax-parse stx
    [(form-id ptr-expr:expr (~var type (:type stx))
              (~optional (~seq offset-expr:expr (~optional (~and abs #:bytes))))
              val-expr:expr)
     (define t (attribute type.t))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)]
             [val val-expr])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ptr_t?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset))
           (unless (#,(ffi2-type-predicate t) val) (bad-assign-value 'form-id '#,(ffi2-type-name t) val)))
         #,@(ffi2-type-defns t)
         (do-ffi2-ptr-set! #,(ffi2-type-compound? t)
                           #,(ffi2-type-racket->c t)  #,(ffi2-type-vm-type t)
                           ptr
                           #,(if (attribute abs)
                                 #'offset
                                 #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy)))
                           val
                           #,(ffi2-type-release t)))]))

(define-syntax (do-ffi2-ptr-set! stx)
  (syntax-parse stx
    [(_ field-compound?
        field-racket->c field-vm-type
        ptr offset val
        field-release)
     #`(let ([c (field-racket->c val)])
         #,(if (syntax-e #'field-compound?)
               #`(ffi2-memcpy* ptr offset
                               c 0
                               (#%foreign-inline (ffi2-sizeof field-vm-type) #:copy))
               #`((#%foreign-inline (begin-unsafe (ffi2-ptr-set!-maker field-vm-type)) #:copy)
                  ptr offset
                  c))
         (field-release c)
         (void))]))

(define-syntax (ffi2-malloc stx)
  (define (build form-id kind-stx t n-expr-stx abs? as-t as-t-stx)
    (define kind-sym (string->symbol (keyword->string (syntax-e kind-stx))))
    (define size-vm-type (if abs?
                             'integer-8
                             (ffi2-type-vm-type t)))
    (define ptr-vm-type (cond
                          [as-t
                           (unless (ffi2-type-pointer? as-t)
                             (raise-syntax-error #f "result type is not a pointer type" stx as-t-stx))
                           (if (eq? kind-sym 'manual)
                               (ffi2-type-vm-type as-t)
                               (pointer-vm-type->gcable (ffi2-type-vm-type as-t)))]
                          [(and t (ffi2-type-pointer-vm-type t))
                           => (lambda (vm-type) (if (eq? kind-sym 'manual)
                                                    vm-type
                                                    (pointer-vm-type->gcable vm-type)))]
                          [(eq? kind-sym 'manual) 'pointer]
                          [else 'pointer/gc]))
    #`(let ([n #,n-expr-stx])
        (unless (exact-nonnegative-integer? n) (raise-argument-error '#,form-id "exact-nonnegative-integer?" n))
        #,@(if as-t
               (ffi2-type-defns as-t)
               '())
        (#,(if as-t
               (ffi2-type-c->racket as-t)
               #'values)
         ((#%foreign-inline (ffi2-malloc-maker #,size-vm-type #,ptr-vm-type #,kind-sym) #:copy) n))))
  (syntax-parse stx
    [(form-id (~optional kind::malloc-kind)
              maybe-type::maybe-type
              (~optional (~seq n-expr:expr (~optional (~and abs #:bytes))))
              (~optional (~seq #:as (~var as-type (:type stx)))))
     #:with (~var type (:type stx)) #'maybe-type
     (build #'form-id
            #'(~? kind #:gcable)
            (attribute type.t)
            #'(~? n-expr 1)
            (attribute abs)
            (attribute as-type.t)
            (attribute as-type))]
    [(form-id (~optional kind::malloc-kind)
              n-expr:expr (~optional #:bytes)
              (~optional (~seq #:as (~var as-type (:type stx)))))
     (build #'form-id
            #'(~? kind #:gcable)
            #f
            #'n-expr
            #t
            (attribute as-type.t)
            (attribute as-type))]))

(define (ffi2-free v)
  (unless (ffi2-ptr? v) (raise-argument-error 'ffi2-free "ptr_t?" v))
  (ffi2-free* v))

(define (ffi2-memcpy dest src len
                     #:dest-offset [dest-offset 0]
                     #:src-offset [src-offset 0])
  (define who 'ffi2-memcpy)
  (unless (ffi2-ptr? dest) (raise-argument-error who "ptr_t?" dest))
  (unless (ffi2-ptr? src) (raise-argument-error who "ptr_t?" src))
  (unless (exact-integer? len) (raise-argument-error who "exact-integer?" len))
  (unless (exact-integer? dest-offset) (raise-argument-error who "exact-integer?" dest-offset))
  (unless (exact-integer? src-offset) (raise-argument-error who "exact-integer?" src-offset))
  (ffi2-memcpy* dest dest-offset src src-offset len))

(define (ffi2-memmove dest src len
                      #:dest-offset [dest-offset 0]
                      #:src-offset [src-offset 0])
  (define who 'ffi2-memmove)
  (unless (ffi2-ptr? dest) (raise-argument-error who "ptr_t?" dest))
  (unless (ffi2-ptr? src) (raise-argument-error who "ptr_t?" src))
  (unless (exact-integer? len) (raise-argument-error who "exact-integer?" len))
  (unless (exact-integer? dest-offset) (raise-argument-error who "exact-integer?" dest-offset))
  (unless (exact-integer? src-offset) (raise-argument-error who "exact-integer?" src-offset))
  (ffi2-memmove* dest dest-offset src src-offset len))

(define (ffi2-memset dest byte len
                     #:dest-offset [dest-offset 0])
  (define who 'ffi2-memset)
  (unless (ffi2-ptr? dest) (raise-argument-error who "ptr_t?" dest))
  (unless (byte? byte) (raise-argument-error who "byte?" byte))
  (unless (exact-integer? len) (raise-argument-error who "exact-integer?" len))
  (unless (exact-integer? dest-offset) (raise-argument-error who "exact-integer?" dest-offset))
  (ffi2-memset* dest dest-offset byte len))

(define-syntax (ffi2-sizeof stx)
  (syntax-parse stx
    [(form-id (~var type (:type stx)))
     (define t (attribute type.t))
     #`(#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy)]))

(define-syntax (ffi2-offsetof stx)
  (syntax-parse stx
    [(form-id (~var type (:type stx)) field-name:id)
     (define t (attribute type.t))
     (define vm-type (ffi2-type-vm-type t))
     (unless (for/or ([field (in-list (if (pair? vm-type) (cddr vm-type) null))])
               (eq? (car field) (syntax-e #'field-name)))
       (raise-syntax-error #f "field name not found in type" stx #'field-name))
     (cond
       [(and (pair? vm-type) (eq? (car vm-type) 'union))
        #'0]
       [else
        #`(#%foreign-inline (ffi2-offsetof #,vm-type field-name) #:copy)])]))

(define-syntax (ffi2-procedure stx)
  (parse-ffi2-procedure stx stx #t))
  
(define-for-syntax (parse-ffi2-procedure stx stx-for-type check-ptr?)
  (syntax-parse stx
    [(form-id ptr-expr:expr (~var a (:arrow-type stx-for-type)))
     (build-ffi2-procedure #'form-id #'ptr-expr (attribute a.t) check-ptr?)]
    [(form-id ptr-expr:expr (~var type (:type stx-for-type)))
     (define t (attribute type.t))
     (unless (eq? (ffi2-type-category t) 'arrow)
       (raise-syntax-error #f "not a procedure type" stx #'type))
     #`(let ([ptr ptr-expr])
         #,@(if check-ptr?
                #`((unless (variable-reference-from-unsafe? (#%variable-reference))
                     (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ptr_t?" ptr))))
                #'())
         #,@(ffi2-type-defns t)
         (#,(ffi2-type-c->racket t) ptr))]))

(define-for-syntax (build-ffi2-procedure form-id ptr-expr a-t check-ptr?)
  (define in-ts (arrow-type-in-ts a-t))
  (define out-t (arrow-type-out-t a-t))
  (with-syntax ([form-id form-id]
                [ptr-expr ptr-expr]
                [(in ...) (generate-temporaries in-ts)]
                [(in-ok? ...) (map ffi2-type-predicate in-ts)]
                [(in_t-name ...) (map ffi2-type-name in-ts)]
                [((in-defn ...) ...) (map ffi2-type-defns in-ts)]
                [(in-racket->c ...) (map ffi2-type-racket->c in-ts)]
                [(in-release ...) (map ffi2-type-release in-ts)]
                [(out-errno ...) (if (arrow-type-errno? a-t)
                                     #'(out-errno)
                                     #'())]
                [(conv ...) (arrow-type-convs a-t)])
    (with-syntax ([adjust-proc (cond
                                 [(ffi2-type-compound? out-t)
                                  (define kind-sym 'gcable-immobile)
                                  (define ptr-vm-type (let ([vm-type (ffi2-type-pointer-vm-type out-t)])
                                                        (if (eq? kind-sym 'manual)
                                                            vm-type
                                                            (pointer-vm-type->gcable vm-type))))
                                  #`(lambda (in ...)
                                      (define r
                                        ((#%foreign-inline (ffi2-malloc-maker #,(ffi2-type-vm-type out-t) #,ptr-vm-type #,kind-sym) #:copy) 1))
                                      (define-values (out-void out-errno ...)
                                        (proc r in ...))
                                      (values r out-errno ...))]
                                 [else #'proc])])
      #`(let ([ptr ptr-expr])
          #,@(if check-ptr?
                 #`((unless (variable-reference-from-unsafe? (#%variable-reference))
                      (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ptr_t?" ptr))))
                 '())
          in-defn ... ...
          #,@(ffi2-type-defns out-t)
          (let ([proc ((#%foreign-inline (begin-unsafe
                                           (ffi2-procedure-maker (conv ...)
                                                                 #,(map ffi2-type-vm-type in-ts)
                                                                 #,(ffi2-type-vm-type out-t)))
                                         #:copy*)
                       ptr)])
            (let ([proc adjust-proc])
              (lambda (in ...)
                (unless (in-ok? in) (bad-argument 'in_t-name in))
                ...
                (let ([in (in-racket->c in)] ...)
                  (let-values ([(out out-errno ...) (proc in ...)])
                    ;; the `release` function can usefully be something like `black-box` to
                    ;; retain a converted argument until the foreign procedure returns
                    (in-release in) ...
                    (values (#,(ffi2-type-c->racket out-t) out) out-errno ...))))))))))

(define-for-syntax (parse-define-ffi2-procedure stx use-lib-expr
                                                #:default-fail [default-fail #f]
                                                #:default-wrap [default-wrap #f]
                                                #:provide? [provide? #f])
  (syntax-parse stx
    [(form-id name:id maybe-type::maybe-type
              (~alt (~optional (~seq #:lib lib-expr:expr))
                    (~optional (~seq #:c-id c-name:id))
                    (~optional (~seq #:fail fail-expr:expr))
                    (~optional (~seq #:wrap wrap-expr:expr)))
              ...)
     (cond
       [(not use-lib-expr)
        (unless (attribute lib-expr)
          (raise-syntax-error #f "missing a `#:lib` clause to specify the source foreign library" stx))]
       [else
        (when (attribute lib-expr)
          (raise-syntax-error #f "redundant or conflicting `#:lib`" stx #'lib-expr))])
     (with-syntax ([lib-expr (or (attribute lib-expr) use-lib-expr)]
                   [c-name #'(~? c-name name)])
       (with-syntax ([name-bstr (string->bytes/utf-8 (symbol->string (syntax-e #'c-name)))]
                     [wrapper (if (attribute wrap-expr) #'wrap (or default-wrap #'begin))]
                     [build-default-fail (if default-fail
                                             #`(lambda () (failure-result (#,default-fail 'c-name)))
                                             #'#f)]
                     [(name-provide ...) (if provide?
                                             #'((provide (protect-out name)))
                                             #'())])
         #`(begin
             name-provide ...
             (~? (define wrap (check-wrap-proc 'form-id wrap-expr)))
             (define name-ptr (ffi2-lib-ref lib-expr name-bstr
                                            #:fail (~? (build-fail 'form-id fail-expr 'c-name)
                                                       build-default-fail)))             
             (define name (wrapper
                           #,(let ([proc (parse-ffi2-procedure #'(form-id name-ptr maybe-type)
                                                               stx
                                                               #f)])
                               (if (or (attribute fail-expr) default-fail)
                                   #`(if (failure-result? name-ptr)
                                         (failure-result-v name-ptr)
                                         #,proc)
                                   proc)))))))]))

(define-syntax (define-ffi2-procedure stx)
  (parse-define-ffi2-procedure stx #f))

(define-syntax (define-ffi2-definer stx)
  (syntax-parse stx
    [(form-id name:id
              (~alt (~optional (~seq #:lib lib-expr))
                    (~optional (~seq #:default-fail fail-expr))
                    (~optional (~seq #:default-wrap wrap-expr))
                    (~optional (~and provide? #:provide)
                               #:defaults ([provide? #'#f])))
              ...)
     (unless (attribute lib-expr)
       (raise-syntax-error #f "missing a `#:lib` clause" stx))
     (with-syntax ([fail-id (if (attribute fail-expr) #'(quote-syntax fail) #'#f)]
                   [wrap-id (if (attribute wrap-expr) #'(quote-syntax wrap) #'#f)])
       #'(begin
           (define lib lib-expr)
           (~? (define wrap (check-wrap-proc 'form-id wrap-expr)))
           (~? (define fail (check-fail-proc 'form-id fail-expr)))
           (define-syntax name
             (lambda (stx)
               (parse-define-ffi2-procedure stx #'lib
                                            #:default-fail fail-id
                                            #:default-wrap wrap-id
                                            #:provide? 'provide?)))))]))

(define-syntax (ffi2-callback stx)
  (syntax-parse stx
    #:literals (->)
    [(form-id proc-expr:expr (~var a (:arrow-type stx)))
     (build-ffi2-callback #'form-id #'proc-expr (attribute a.t))]
    [(form-id proc-expr:expr (~var type (:type stx)))
     (define t (attribute type.t))
     (unless (eq? (ffi2-type-category t) 'arrow)
       (raise-syntax-error #f "not a procedure type" stx #'type))
     #`(let ([proc proc-expr])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (procedure? proc) (raise-argument-error 'form-id "procedure?" proc)))
         #,@(ffi2-type-defns t)
         (#,(ffi2-type-racket->c t) proc))]))

(define-for-syntax (build-ffi2-callback form-id proc-expr a-t)
  (define in-ts (arrow-type-in-ts a-t))
  (define out-t (arrow-type-out-t a-t))
  (with-syntax ([form-id form-id]
                [proc-expr proc-expr]
                [(in ...) (generate-temporaries in-ts)]
                [(in-c->racket ...) (map ffi2-type-c->racket in-ts)]
                [(conv ...) (arrow-type-convs a-t)]
                [async-apply-expr (if (arrow-type-async-apply? a-t)
                                      #'async-apply-for-callback
                                      #'#f)])
    (with-syntax ([adjust-proc (cond
                                 [(ffi2-type-compound? out-t)
                                  #`(lambda (r in ...)
                                      (define out (proc in ...))
                                      (ffi2-memcpy r out (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type out-t))
                                                                           #:copy))
                                      (#,(ffi2-type-release out-t) out)
                                      r)]
                                 [else #'proc])])
      #`(let ([proc proc-expr]
              [async-apply async-apply-expr])
          (let ([proc (lambda (in ...)
                        (define out (proc (in-c->racket in) ...))
                        (unless (#,(ffi2-type-predicate out-t) out)
                          (bad-result '#,(ffi2-type-name out-t) out))
                        (#,(ffi2-type-racket->c out-t) out))])
            (let ([proc adjust-proc])
              ((#%foreign-inline (ffi2-callback-maker (__disable_interrupts conv ...)
                                                      #,(map ffi2-type-vm-type in-ts)
                                                      #,(ffi2-type-vm-type out-t))
                                 #:copy*)
               proc
               async-apply)))))))

(define (async-apply-for-callback thunk)
  (thunk))

(define-syntax (ffi2-cast stx)
  (syntax-parse stx
    [(form-id expr:expr
              (~alt (~optional (~seq #:from (~var from (:type stx))))
                    (~optional (~seq #:offset (~optional maybe-off-type::maybe-type) offset-expr:expr))
                    (~optional (~seq #:to (~var to (:type stx)))))
              ...)
     (define offset-t (and (attribute maybe-off-type)
                           (syntax-parse #'maybe-off-type
                             [(~var off-type (:type stx)) (attribute off-type.t)])))
     (define from-t (attribute from.t))
     (define t (attribute to.t))
     (when from-t
       (unless (ffi2-type-pointer? from-t)
         (raise-syntax-error #f "target type is not a pointer type" stx #'from)))
     (when t
       (unless (ffi2-type-pointer? t)
         (raise-syntax-error #f "target type is not a pointer type" stx #'to)))
     (define vm-type (if t (ffi2-type-vm-type t) 'pointer))
     (define gcable-vm-type (pointer-vm-type->gcable vm-type))
     #`(let ([ptr expr]
             [offset (~? offset-expr 0)])
         #,@(if from-t
                (ffi2-type-defns from-t)
                '())
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           #,(if from-t
                 #`(unless (#,(ffi2-type-predicate from-t) ptr) (bad-cast-value 'form-id '#,(ffi2-type-name from-t) ptr))
                 #`(unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ptr_t?" ptr)))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         (let ([c #,(if from-t
                        #`(#,(ffi2-type-racket->c from-t) ptr)
                        #`ptr)])
           (let ([v ((#%foreign-inline (ffi2-ptr-cast-maker #,vm-type #,gcable-vm-type) #:copy*)
                     c
                     #,(if (not offset-t)
                           #'offset
                           #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type offset-t)) #:copy))))])
             #,@(if from-t
                    #`(#,(ffi2-type-release from-t) c)
                    '())
             #,@(if t
                    (ffi2-type-defns t)
                    '())
             #,(if t
                   #`(#,(ffi2-type-c->racket t) v)
                   #'v))))]))

(define-syntax (ffi2-add stx)
  (syntax-parse stx
    [(form-id expr:expr offset-expr:expr)
     #`(let ([ptr expr]
             [offset offset-expr])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ptr_t?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         ((#%foreign-inline (ffi2-ptr-cast-maker pointer pointer/gc) #:copy*)
          ptr
          offset))]
    [(form-id expr:expr (~var to (:type stx)) offset-expr:expr)
     (define t (attribute to.t))
     (define vm-type (ffi2-type-pointer-vm-type t))
     (define gcable-vm-type (pointer-vm-type->gcable vm-type))
     #`(let ([ptr expr]
             [offset (~? offset-expr 0)])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ptr_t?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         ((#%foreign-inline (ffi2-ptr-cast-maker #,vm-type #,gcable-vm-type) #:copy*)
          ptr
          (* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy))))]))

(define-for-syntax (parse-system-type-case/type stx)
  (define p
    (parse-system-type-case stx
                            (lambda (stx rhs-stx)
                              (syntax-parse rhs-stx
                                [(~var rhs (:type stx))
                                 (unless (ffi2-type-scalar? (attribute rhs.t))
                                   (raise-syntax-error #f "expected a scalar type" stx rhs-stx))
                                 (attribute rhs.t)]))
                            (lambda (rhs-t)
                              (cons (ffi2-type-vm-type rhs-t)
                                    (ffi2-type-predicate rhs-t)))
                            (lambda (key vals left right)
                              (cons (list 'select key vals (car left) (car right))
                                    #`(#%foreign-inline
                                       (ffi2-system-type--select #,key #,vals #,(cdr left) #,(cdr right)))))))

    (make-ffi2-type 'system-type-case (car p) (cdr p)
                    #:category 'scalar))

(define-for-syntax (parse-system-type-case/abi stx)
  (parse-system-type-case stx
                          (lambda (stx rhs-stx)
                            (syntax-parse rhs-stx
                              [(~var abi (:abi stx))
                               (attribute abi.a)]))
                          (lambda (rhs-a) rhs-a)
                          (lambda (key vals left right)
                            (list '__select key vals left right))))
