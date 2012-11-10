#lang racket/base
(require ffi/unsafe
         racket/stxparam
         (for-syntax racket/base)
         "atomic.rkt"
         "define.rkt")

(define objc-lib (ffi-lib "libobj" #:fail (lambda () #f)))

(define-ffi-definer define-objc objc-lib
  #:provide provide-protected
  #:default-make-fail make-not-available)
(define-ffi-definer define-objc/private objc-lib
  #:default-make-fail make-not-available)

;; ----------------------------------------

(provide _id _Class _Protocol _BOOL _SEL _Ivar
         make-objc_super _objc_super)

(define _id (_cpointer/null 'id))

(define _SEL (_cpointer/null 'SEL))
(define _Ivar (_cpointer/null 'Ivar))
(define _Class (make-ctype _id
                           (lambda (v) v)
                           (lambda (p)
                             (when p (cpointer-push-tag! p 'Class))
                             p)))
(define _Protocol (make-ctype _id
                              (lambda (v) v)
                              (lambda (p)
                                (when p (cpointer-push-tag! p 'Protocol))
                                p)))
(define _BOOL (make-ctype _byte
                          (lambda (v) (if v 1 0))
                          (lambda (v) (not (eq? v 0)))))

(define _Method (_cpointer/null 'Method))
(define _IMP (_fun #:async-apply (lambda (f) (f)) _id _SEL -> _id))

(define-cstruct _objc_super ([receiver _id][class _Class]))

(provide YES NO)
(define YES #t)
(define NO #f)

;; ----------------------------------------

;; These structures and functions are used for Objective-C 
;; version prior to 2.0:

(define-cstruct _objc_ivar
  ([name _pointer]
   [ivar_type _pointer]
   [ivar_offset _int]))

(define-cstruct _objc_ivar_list
  ([ivar_count _int]
   ;; and then an array of objc_ivar
   ))

(define-cstruct _objc_class
  ([isa _pointer] ; _objc_class-pointer
   [super_class _pointer] ; _objc_class-pointer
   [name _pointer] ; string
   [version _long]
   [info _long]
   [instance_size _long]
   [ivars (_or-null _objc_ivar_list-pointer)]
   [methodLists _pointer]
   [cache _pointer]
   [protocols _pointer]))

(define-cstruct _objc_protocol_list
  ([next _pointer]
   [count _int] ; 1
   [protocol _pointer]))

(define-cstruct _objc_cache
  ([mask _uint] ; 0, since one bucket allocated
   [occupied _uint]
   [buckets _pointer] ; really an array
   ))

(define-cstruct _objc_method
  ([method_name _SEL]
   [method_types _pointer]
   [method_imp _fpointer]))

(define-cstruct _objc_method_list
  ([obsolete _pointer]
   [method_count _int] ; 1
   [method _objc_method]))

(define CLS_CLASS               #x1)
(define CLS_META                #x2)

(define (strcpy s)
  (let* ([n (cast s _string _bytes)]
         [p (malloc 'raw (add1 (bytes-length n)))])
    (memcpy p n (add1 (bytes-length n)))
    p))

(define (allocate-class-pair-the-hard-way superclass name)
  (let* ([super (cast superclass _Class _objc_class-pointer)]
         [root (let loop ([super super])
                 (let ([s (objc_class-super_class super)])
                   (if s
                       (loop (cast s _pointer _objc_class-pointer))
                       super)))]
         [name (strcpy name)]
         [malloc+memcpy (lambda (c)
                          (let ([p (malloc 'raw 1 _objc_class)])
                            (memcpy p c 1 _objc_class)
                            (set-cpointer-tag! p objc_class-tag)
                            p))]
         [empty-cache (lambda ()
                        ;; If you try things the hard way with Obj-C 2.0,
                        ;;  you need to set up the cache. For ealier
                        ;;  versions, you need to set the cache to #f.
                        #;
                        (let ([p (malloc 'raw 1 _objc_cache)])
                          (memset p 0 1 _objc_cache)
                          p)
                        #f)]
         [meta-super (cast (objc_class-isa super) _pointer _objc_class-pointer)]
         [new-meta (malloc+memcpy
                    (make-objc_class (objc_class-isa root)
                                     meta-super
                                     name
                                     0
                                     CLS_META
                                     (objc_class-instance_size meta-super)
                                     #f
                                     #f
                                     (empty-cache)
                                     #f))]
         [new (malloc+memcpy
               (make-objc_class new-meta
                                super
                                name
                                0
                                CLS_CLASS
                                (objc_class-instance_size super)
                                #f
                                #f
                                (empty-cache)
                                #f))])
    (cast new _objc_class-pointer _Class)))

(define (add-ivar-the-hard-way class field-name field-name-type)
  (let* ([class (cast class _Class _objc_class-pointer)]
         [ivars (or (objc_class-ivars class)
                    (make-objc_ivar_list 0))]
         [count (objc_ivar_list-ivar_count ivars)]
         [array-start (+ (ctype-sizeof _int)
                         (- (max (ctype-alignof _objc_ivar)
                                 (ctype-sizeof _int))
                            (ctype-sizeof _int)))]
         [old-size (+ array-start
                      (* count
                         (ctype-sizeof _objc_ivar)))]
         [new-ivars (malloc 'raw (+ old-size (ctype-sizeof _objc_ivar)))]
         [new-ivar (ptr-add new-ivars old-size)])
    (set-cpointer-tag! new-ivars objc_ivar_list-tag)
    (set-cpointer-tag! new-ivar objc_ivar-tag)
    (memcpy new-ivars ivars old-size)
    (set-objc_ivar_list-ivar_count! new-ivars (add1 (objc_ivar_list-ivar_count ivars)))
    (set-objc_ivar-name! new-ivar (strcpy field-name))
    (set-objc_ivar-ivar_type! new-ivar (strcpy field-name-type))
    (set-objc_ivar-ivar_offset! new-ivar (objc_class-instance_size class))
    (set-objc_class-ivars! class new-ivars)
    (set-objc_class-instance_size! class (+ (objc_class-instance_size class)
                                            ;; Assumes pointer size:
                                            (ctype-sizeof _pointer)))))

(define (add-protocol-the-hard-way c p)
  (let* ([c (cast c _Class _objc_class-pointer)]
         [malloc+memcpy (lambda (c)
                          (let ([p (malloc 'raw 1 _objc_protocol_list)])
                            (memcpy p c 1 _objc_protocol_list)
                            p))]
         [protos (malloc+memcpy
                  (make-objc_protocol_list
                   (objc_class-protocols c)
                   1
                   p))])
    (set-objc_class-protocols! c protos)
    #t))

(define (add-method-the-hard-way class sel imp types)
  (let* ([malloc+memcpy (lambda (c)
                          (let ([p (malloc 'raw 1 _objc_method_list)])
                            (memcpy p c 1 _objc_method_list)
                            (set-cpointer-tag! p objc_method_list-tag)
                            p))]
         [methods (malloc+memcpy
                   (make-objc_method_list
                    #f
                    1
                    (make-objc_method sel (strcpy types) imp)))])
    (class_addMethods class methods)))

;; ----------------------------------------

(define-objc objc_lookUpClass (_fun _string -> _Class)
  #:fail (lambda () (lambda (name) #f)))

(define-objc objc_getProtocol (_fun _string -> _Protocol)
  #:fail (lambda () (lambda (name)
                      (cast (objc_lookUpClass name) _Class _Protocol))))

(define-objc sel_registerName (_fun _string -> _SEL)
  #:fail (lambda () (lambda (name)
                      ;; Fake registration using interned symbols
                      (cast (string->symbol name) _racket _gcpointer))))

(define-objc objc_allocateClassPair (_fun _Class _string _long -> _Class)
  #:fail (lambda () #f))
(define-objc objc_registerClassPair (_fun _Class -> _void)
  #:fail (lambda () #f))

(define-objc objc_addClass (_fun _objc_class-pointer -> _void)
  #:fail (lambda () #f))

(define-objc object_getClass (_fun _id -> _Class)
  #:fail (lambda () #f))

(define-objc class_addMethod/raw (_fun _Class _SEL _fpointer _string -> _BOOL)
  #:c-id class_addMethod
  #:fail (lambda () #f))
(define-objc class_addMethods (_fun _Class _objc_method_list-pointer -> _void)
  #:fail (lambda () #f))

(define (class_addMethod cls sel imp ty enc)
  (let ([imp (function-ptr imp ty)])
    (if class_addMethod/raw
        (class_addMethod/raw cls sel imp enc)
        (add-method-the-hard-way cls sel imp enc))))


(define-objc class_addIvar (_fun _Class _string _long _uint8 _string -> _BOOL)
  #:fail (lambda () #f))

(define-objc object_getInstanceVariable (_fun _id _string [p : (_ptr o _pointer)]
                                              -> [ivar : _Ivar] 
                                              -> (values ivar p)))
(define-objc object_setInstanceVariable (_fun _id _string _pointer -> _Ivar))

(define-objc class_addProtocol (_fun _Class _Protocol -> _BOOL)
  #:fail (lambda () #f))

(define-objc/private objc_msgSend _fpointer)
(define-objc/private objc_msgSend_fpret _fpointer
  #:fail (lambda ()
           ;; If objc_msgSend_fpret is not available, assume that
           ;; it's the same as objc_msgSend
           objc_msgSend))
(define-objc/private objc_msgSend_stret _fpointer)
(define-objc/private objc_msgSendSuper _fpointer)
(define objc_msgSendSuper_fpret objc_msgSendSuper) ; why no fpret variant?
(define-objc/private objc_msgSendSuper_stret _fpointer)

(define sizes-for-direct-struct-results
  (case (string->symbol (path->string (system-library-subpath #f)))
    [(i386-macosx i386-darwin) (lambda (v) (memq (ctype-sizeof v) '(1 2 4 8)))]
    [(ppc-macosx ppc-darwin) (lambda (v) (memq (ctype-sizeof v) '(1 2 3 4)))]
    [(x86_64-macosx x86_64-darwin) 
     (lambda (v)
       ;; Remarkably complex rules govern sizes > 8 and <= 32.
       ;; But if we assume no unaligned data and that fancy types
       ;; like _m256 won't show up with ObjC, it seems to be as
       ;; simple as this:
       ((ctype-sizeof v) . <= . 16))]))

;; Make `msgSends' access atomic, so that a thread cannot be suspended
;; or killed during access, which would block other threads.
(define-syntax-rule (as-atomic e)
  (begin (start-atomic) (begin0 e (end-atomic))))

(define (lookup-send types msgSends msgSend msgSend_fpret msgSend_stret first-arg-type)
  ;; First type in `types' vector is the result type
  (or (as-atomic (hash-ref msgSends types #f))
      (let ([ret-layout (ctype->layout (vector-ref types 0))])
        (if (and (list? ret-layout)
                 (not (sizes-for-direct-struct-results (vector-ref types 0))))
            ;; Structure return type:
            (let* ([pre-m (function-ptr msgSend_stret
                                        (_cprocedure
                                         (list* _pointer first-arg-type _SEL (cdr (vector->list types)))
                                         _void))]
                   [m (lambda args
                        (let ([v (malloc (vector-ref types 0))])
                          (apply pre-m v args)
                          (ptr-ref v (vector-ref types 0))))])
              (as-atomic (hash-set! msgSends types m))
              m)
            ;; Non-structure return type:
            (let ([m (function-ptr (if (memq ret-layout
                                             '(float double double*))
                                       msgSend_fpret
                                       msgSend)
                                   (_cprocedure
                                    (list* first-arg-type _SEL (cdr (vector->list types)))
                                    (vector-ref types 0)))])
              (as-atomic (hash-set! msgSends types m))
              m)))))

(define msgSends (make-hash))
(define (objc_msgSend/typed types)
  (lookup-send types msgSends objc_msgSend objc_msgSend_fpret objc_msgSend_stret _id))
(provide objc_msgSend/typed)

(define msgSendSupers (make-hash))
(define (objc_msgSendSuper/typed types)
  (lookup-send types msgSendSupers objc_msgSendSuper objc_msgSendSuper_fpret objc_msgSendSuper_stret _pointer))
(provide objc_msgSendSuper/typed)

;; ----------------------------------------

(provide import-class)
(define-syntax (import-class stx)
  (syntax-case stx ()
    [(_ id)
     (quasisyntax/loc stx
       (define id (objc_lookUpClass #,(symbol->string (syntax-e #'id)))))]
    [(_ id ...)
     (syntax/loc stx (begin (import-class id) ...))]))

(provide import-protocol)
(define-syntax (import-protocol stx)
  (syntax-case stx ()
    [(_ id)
     (quasisyntax/loc stx
       (define id (objc_getProtocol #,(symbol->string (syntax-e #'id)))))]
    [(_ id ...)
     (syntax/loc stx (begin (import-protocol id) ...))]))

;; ----------------------------------------
;; iget-value and set-ivar! work only with fields that contain Racket values

(provide get-ivar set-ivar!)

(define-for-syntax (check-ivar ivar stx)
  (unless (identifier? ivar)
    (raise-type-error #f
                      "expected an identifier for an instance-variable name"
                      stx
                      ivar)))

(define-syntax (get-ivar stx)
  (syntax-case stx ()
    [(_ obj ivar)
     (begin
       (check-ivar #'ivar stx)
       (quasisyntax/loc stx
         (get-ivar-value obj #,(symbol->string (syntax-e #'ivar)))))]))

(define (get-ivar-value obj name)
  (let-values ([(ivar p) (object_getInstanceVariable obj name)])
    (and p (ptr-ref p _racket))))
      

(define-syntax (set-ivar! stx)
  (syntax-case stx ()
    [(_ obj ivar val)
     (begin
       (check-ivar #'ivar stx)
       (quasisyntax/loc stx
         (set-ivar-value obj #,(symbol->string (syntax-e #'ivar)) val)))]))

(define (set-ivar-value obj name val)
  (let-values ([(ivar p) (object_getInstanceVariable obj name)])
    (if p
        (ptr-set! p _racket val)
        (let ([p (malloc-immobile-cell val)])
          (void (object_setInstanceVariable obj name p))))))

(define (free-fields obj names)
  (for-each (lambda (name)
              (let-values ([(ivar p) (object_getInstanceVariable obj name)])
                (when p
                  (object_setInstanceVariable obj name #f)
                  (free-immobile-cell p))))
            names))

;; ----------------------------------------

(define-for-syntax method-sels (make-hash))

(define-for-syntax (register-selector sym)
  (or (hash-ref method-sels (cons (syntax-local-lift-context) sym) #f)
      (let ([id (syntax-local-lift-expression
                 #`(sel_registerName #,(symbol->string sym)))])
        (hash-set! method-sels sym id)
        id)))

(provide selector)
(define-syntax (selector stx)
  (syntax-case stx ()
    [(_ id)
     (begin
       (unless (identifier? #'id)
         (raise-syntax-error #f
                             "expected an identifier"
                             stx
                             #'id))
       (register-selector (syntax-e #'id)))]))

;; ----------------------------------------

(define-for-syntax (combine stxes)
  (string->symbol
   (apply
    string-append
    (map (lambda (e) (symbol->string (syntax-e e)))
         (syntax->list stxes)))))

(define-for-syntax (check-method-name m stx)
  (unless (identifier? m)
    (raise-syntax-error #f
                        "expected an identifier for the method name"
                        stx
                        m)))

(define-for-syntax (check-id-colon id stx)
  (unless (regexp-match #rx":$" (symbol->string (syntax-e id)))
    (raise-syntax-error #f
                        "expected an identifier that ends in `:' to tag an argument"
                        stx
                        id)))

(define-for-syntax (parse-arg-list l stx formals?)
  (define (is-typed? l)
    (if formals?
        (and (pair? (cdr l))
             (let ([l (syntax->list (cadr l))])
               (and (list? l)
                    (= 2 (length l)))))
        (and (pair? (cdr l))
             (eq? '#:type (syntax-e (cadr l))))))
  (let loop ([l l])
    (if (null? l)
        null
        (begin
          (unless (identifier? (car l))
            (raise-syntax-error #f
                                "expected an identifier to tag an argument"
                                stx
                                (car l)))
          (check-id-colon (car l) stx)
          (let ([tag (car l)]
                [type (if (is-typed? l)
                          (if formals?
                              (car (syntax-e (cadr l)))
                              (if (pair? (cddr l))
                                  (caddr l)
                                  (raise-syntax-error #f
                                                      "missing type expression after tag with #:type"
                                                      stx
                                                      (car l))))
                          #'_id)]
                [rest (if formals?
                          (cdr l)
                          (if (is-typed? l)
                              (cdddr l)
                              (cdr l)))])
            (unless (pair? rest)
              (raise-syntax-error #f
                                  (format "missing an argument~a after tag"
                                          (if formals? " identifier" " expression"))
                                  stx
                                  tag))
            (cons
             (list tag type (let ([arg (car rest)])
                              (if formals?
                                  (if (identifier? arg)
                                      arg
                                      (let ([l (syntax->list arg)])
                                        (unless (and (list? l)
                                                     (= 2 (length l))
                                                     (identifier? (cadr l)))
                                          (raise-syntax-error #f
                                                              (string-append
                                                               "exepected an identifier for an argument name"
                                                               " or a parenthesized type--identifier sequence")
                                                              stx
                                                              arg))
                                        (cadr l)))
                                  arg)))
             (loop (cdr rest))))))))

(provide tell tellv)
(define-for-syntax (build-send stx result-type send/typed send-args l-stx)
  (let ([l (syntax->list  l-stx)])
    (with-syntax ([((tag type arg) ...) (parse-arg-list l stx #f)]
                  [send send/typed]
                  [(send-arg ...) send-args])
      (quasisyntax/loc stx
        ((send (type-vector #,result-type type ...))
         send-arg ... #,(register-selector (combine #'(tag ...)))
         arg ...)))))

(define-syntax (tell stx)
  (syntax-case stx ()
    [(_ target)
     (raise-syntax-error #f
                         "method identifier missing"
                         stx)]
    [(_ #:type t)
     (raise-syntax-error #f
                         "method target object missing"
                         stx)]
    [(_ #:type t target)
     (raise-syntax-error #f
                         "method identifier missing"
                         stx)]
    [(_ #:type t target method)
     (let ([m #'method])
       (check-method-name m stx)
       (quasisyntax/loc stx
         ((objc_msgSend/typed (type-vector t)) target #,(register-selector (syntax-e m)))))]
    [(_ target method)
     (not (keyword? (syntax-e #'target)))
     (let ([m #'method])
       (check-method-name m stx)
       (quasisyntax/loc stx
         ((objc_msgSend/typed (type-vector _id)) target #,(register-selector (syntax-e m)))))]
    [(_ #:type result-type target method/arg ...)
     (build-send stx #'result-type 
                 #'objc_msgSend/typed #'(target)
                 #'(method/arg ...))]
    [(_ target method/arg ...)
     (build-send stx #'_id 
                 #'objc_msgSend/typed #'(target)
                 #'(method/arg ...))]))

(define-syntax-rule (tellv a ...)
  (tell #:type _void a ...))

(define-for-syntax liftable-type?
  (let ([prims 
         (syntax->list #'(_id _Class _SEL _void _int _long _float _double _double* _BOOL))])
    (lambda (t)
      (and (identifier? t)
           (ormap (lambda (p) (free-identifier=? t p))
                  prims)))))

(define-syntax (type-vector stx)
  (let ([types (cdr (syntax->list stx))])
    ((if (andmap liftable-type? (cdr (syntax->list stx)))
         (lambda (e)
           (syntax-local-lift-expression #`(intern-type-vector #,e)))
         values)
     (quasisyntax/loc stx (vector . #,types)))))

(define type-vectors (make-hash))
(define (intern-type-vector v)
  (or (hash-ref type-vectors v #f)
      (begin
        (hash-set! type-vectors v v)
        v)))

;; ----------------------------------------

(provide define-objc-class
         define-objc-mixin
         self super-tell)

(define-for-syntax ((check-id stx what) id)
  (unless (identifier? id)
    (raise-syntax-error #f
                        (format "expected an identifier for ~a" what)
                        stx
                        id)))

(define-syntax (define-objc-class stx)
  (syntax-case stx ()
    [(_ id superclass #:mixins (mixin ...) #:protocols (proto ...) (ivar ...) method ...)
     (begin
       ((check-id stx "class definition") #'id)
       (for-each (check-id stx "instance variable")
                 (syntax->list #'(ivar ...)))
       (let ([ivars (syntax->list #'(ivar ...))]
             [methods (syntax->list #'(method ...))])
         (with-syntax ([id-str (symbol->string (syntax-e #'id))]
                       [whole-stx stx]
                       [(dealloc-method ...)
                        (if (null? ivars)
                            ;; no need to override dealloc:
                            #'()
                            ;; add dealloc if it's not here:
                            (if (ormap (lambda (m)
                                         (syntax-case m ()
                                           [(+/- result-type (id . _) . _)
                                            (eq? (syntax-e #'id) 'dealloc)]))
                                       methods)
                                ;; Given a dealloc extension:
                                #'()
                                ;; Need to add one explicitly:
                                #'((-a _void (dealloc) (void)))))])
           (syntax/loc stx
             (begin
               (define superclass-id superclass)
               (define id (allocate-class-pair superclass-id id-str))
               (void (add-protocol id proto)) ...
               (add-ivar id 'ivar) ...
               (let-syntax ([ivar (make-ivar-form 'ivar)] ...)
                 (add-method whole-stx id superclass-id method) ...
                 (mixin id superclass-id '(ivar ...)) ...
                 (add-method whole-stx id superclass-id dealloc-method) ...
                 (void))
               (register-class-pair id))))))]
    [(_ id superclass (ivar ...) method ...)
     #'(define-objc-class id superclass #:mixins () #:protocols () (ivar ...) method ...)]
    [(_ id superclass #:mixins (mixin ...) (ivar ...) method ...)
     #'(define-objc-class id superclass #:mixins (mixin ...) #:protocols () (ivar ...) method ...)]
    [(_ id superclass #:protocols (proto ...) (ivar ...) method ...)
     #'(define-objc-class id superclass #:mixins () #:protocols (proto ...) (ivar ...) method ...)]))

(define (check-expected-ivars id got-ivars expected-ivars)
  (when (ormap (lambda (s) (not (memq s got-ivars)))
               expected-ivars)
    (error id "expected to mix into class with at least ivars: ~s; mixed into class with ivars: ~s"
           expected-ivars
           got-ivars)))

(define-syntax (define-objc-mixin stx)
  (syntax-case stx ()
    [(_ (id superclass-id) #:mixins (mixin ...) #:protocols (proto ...) (ivar ...) method ...)
     (begin
       ((check-id stx "class definition") #'id)
       ((check-id stx "superclass") #'superclass-id)
       (for-each (check-id stx "instance variable")
                 (syntax->list #'(ivar ...)))
       (with-syntax ([whole-stx stx]
                     [(mixin-id ...) (generate-temporaries #'(mixin ...))]
                     [(proto-id ...) (generate-temporaries #'(proto ...))])
         (syntax/loc stx
           (define id
             (let ([mixin-id mixin] ...
                   [proto-id proto] ...)
               (lambda (to-id superclass-id ivars)
                 (check-expected-ivars 'id ivars '(ivar ...))
                 (void (add-protocol to-id proto-id)) ...
                 (let-syntax ([ivar (make-ivar-form 'ivar)] ...)
                   (add-method whole-stx to-id superclass-id method) ...
                   (void))
                 (mixin-id to-id superclass-id ivars) ...))))))]
    [(_ (id superclass) (ivar ...) method ...)
     #'(define-objc-mixin (id superclass) #:mixins () #:protocols () (ivar ...) method ...)]
    [(_ (id superclass) #:mixins (mixin ...) (ivar ...) method ...)
     #'(define-objc-mixin (id superclass) #:mixins (mixin ...) #:protocols () (ivar ...) method ...)]
    [(_ (id superclass) #:protocols (proto ...) (ivar ...) method ...)
     #'(define-objc-mixin (id superclass) #:mixins () #:protocols (proto ...) (ivar ...) method ...)]))

(define-for-syntax (make-ivar-form sym)
  (with-syntax ([sym sym])
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! _ val)
          (syntax/loc stx (set-ivar! self sym val))]
         [(_ arg ...)
          (quasisyntax/loc stx (#,(quasisyntax/loc #'sym #'(get-ivar self sym)) 
                                arg ...))]
         [_ (quasisyntax/loc #'sym (get-ivar self sym))])))))

(define (allocate-class-pair superclass-id id-str)
  (if objc_allocateClassPair
      (objc_allocateClassPair superclass-id id-str 0)
      (allocate-class-pair-the-hard-way superclass-id id-str)))

(define (register-class-pair id)
  (if objc_registerClassPair
      (objc_registerClassPair id)
      (objc_addClass (cast id _Class _objc_class-pointer))))

(define (add-protocol id proto)
  (if class_addProtocol
      (class_addProtocol id proto)
      (add-protocol-the-hard-way id proto)))

(define (object-get-class id)
  (if object_getClass
      (object_getClass id)
      (ptr-ref id _Class)))

(define (layout->string l)
  (case l
    [(uint8) "C"]
    [(int8) "c"]
    [(float) "f"]
    [(double) "d"]
    [(bool) "B"]
    [(void) "v"]
    [(bytes) "*"]
    [(pointer fpointer string/ucs-4 string/utf-16) "?"]
    [else
     (cond
      [(list? l)
       (apply string-append 
              (for/list ([l (in-list l)]
                         [i (in-naturals)])
                (format "f~a=~a" i (layout->string l))))]
      [(eq? l (ctype->layout _int)) "i"]
      [(eq? l (ctype->layout _uint)) "I"]
      [(eq? l (ctype->layout _short)) "s"]
      [(eq? l (ctype->layout _ushort)) "S"]
      [(eq? l (ctype->layout _long)) "l"]
      [(eq? l (ctype->layout _ulong)) "L"]
      [else (error 'generate-layout "unknown layout: ~e" l)])]))

(define (generate-layout rt arg-types)
  (let ([rl (ctype->layout rt)]
        [al (map ctype->layout arg-types)])
    (apply
     string-append
     (layout->string rl)
     "@:"
     (map layout->string al))))

(define-syntax-parameter self
  (lambda (stx)
    (raise-syntax-error #f
                        "valid only within a `define-objc-class' method"
                        stx)))

(define-syntax-parameter super-class
  (lambda (stx) #f))

(define-syntax-parameter super-tell
  (lambda (stx)
    (raise-syntax-error #f
                        "valid only within a `define-objc-class' method"
                        stx)))

(define-for-syntax (make-id-stx orig-id)
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! id v) (raise-syntax-error #f
                                        "assignment to self identifier disallowed"
                                        stx)]
       [(id arg ...) (quasisyntax/loc stx (#,orig-id arg ...))]
       [id (datum->syntax orig-id (syntax-e orig-id) stx orig-id orig-id)]))))

(define-syntax (add-method stx)
  (syntax-case stx ()
    [(_ whole-stx cls superclass-id m)
     (let ([stx #'whole-stx])
       (syntax-case #'m ()
         [(kind result-type (id arg ...) body0 body ...)
          (or (free-identifier=? #'kind #'+)
              (free-identifier=? #'kind #'-)
              (free-identifier=? #'kind #'+a)
              (free-identifier=? #'kind #'-a))
          (let ([id #'id]
                [args (syntax->list #'(arg ...))]
                [in-class? (or (free-identifier=? #'kind #'+)
                               (free-identifier=? #'kind #'+a))])
            (when (null? args)
              (unless (identifier? id)
                (raise-syntax-error #f
                                    "expected an identifier for method name"
                                    stx
                                    id)))
            (with-syntax ([((arg-tag arg-type arg-id) ...)
                           (if (null? args)
                               null
                               (parse-arg-list (cons id args) stx #t))])
              (with-syntax ([id-str (if (null? args)
                                        (symbol->string (syntax-e id))
                                        (symbol->string (combine #'(arg-tag ...))))]
                            [(dealloc-body ...)
                             (if (eq? (syntax-e id) 'dealloc)
                                 (syntax-case stx ()
                                   [(_ _ _ #:mixins _ #:protocols _ [ivar ...] . _)
                                    (with-syntax ([(ivar-str ...)
                                                   (map (lambda (ivar)
                                                          (symbol->string (syntax-e ivar)))
                                                        (syntax->list #'(ivar ...)))])
                                      #'((free-fields self '(ivar-str ...))
                                         (super-tell #:type _void dealloc)))]
                                   [_ (error "oops")])
                                 '())]
                            [(async ...)
                             (if (eq? (syntax-e id) 'dealloc)
                                 ;; so that objects can be destroyed in foreign threads:
                                 #'(#:async-apply apply-directly)
                                 #'())]
                            [in-cls (if in-class?
                                        #'(object-get-class cls)
                                        #'cls)]
                            [atomic? (or (free-identifier=? #'kind #'+a)
                                         (free-identifier=? #'kind #'-a))])
                (quasisyntax/loc stx
                  (let ([rt result-type]
                        [arg-id arg-type] ...)
                    (void (class_addMethod in-cls
                                           (sel_registerName id-str)
                                           #,(syntax/loc #'m
                                               (lambda (self-id cmd arg-id ...)
                                                 (syntax-parameterize ([self (make-id-stx #'self-id)]
                                                                       [super-class (make-id-stx #'superclass-id)]
                                                                       [super-tell do-super-tell])
                                                   body0 body ...
                                                   dealloc-body ...)))
                                           (_fun #:atomic? atomic? #:keep save-method! async ...
                                                 _id _id arg-type ... -> rt)
                                           (generate-layout rt (list arg-id ...)))))))))]
         [else (raise-syntax-error #f
                                   "bad method form"
                                   stx
                                   #'m)]))]))

(define (apply-directly f) (f))

(define methods (make-hasheq))
(define (save-method! m)
  ;; Methods are never GCed, since classes are never unregistered
  (hash-set! methods m #t)
  m)

(define (add-ivar cls name)
  (if class_addIvar
      (void (class_addIvar cls
                           (symbol->string name)
                           (ctype-sizeof _pointer)
                           (sub1 (integer-length (ctype-alignof _pointer)))
                           (layout->string (ctype->layout _pointer))))
      (add-ivar-the-hard-way cls
                             (symbol->string name)
                             (layout->string (ctype->layout _pointer)))))

(define-for-syntax (do-super-tell stx)
  (syntax-case stx ()
    [(_ #:type t)
     (raise-syntax-error #f
                         "method name missing"
                         stx)]
    [(_ #:type t method)
     (let ([m #'method])
       (check-method-name m stx)
       (quasisyntax/loc stx
         ((objc_msgSendSuper/typed (type-vector t))
          (make-objc_super self super-class) 
          #,(register-selector (syntax-e m)))))]
    [(_ method)
     (not (keyword? (syntax-e #'method)))
     (let ([m #'method])
       (check-method-name m stx)
       (quasisyntax/loc stx
         ((objc_msgSendSuper/typed (type-vector _id)) 
          (make-objc_super self super-class)
          #,(register-selector (syntax-e m)))))]
    [(_ #:type result-type method/arg ...)
     (build-send stx #'result-type 
                 #'objc_msgSendSuper/typed
                 #'((make-objc_super self super-class))
                 #'(method/arg ...))]
    [(_ method/arg ...)
     (build-send stx #'_id
                 #'objc_msgSendSuper/typed
                 #'((make-objc_super self super-class))
                 #'(method/arg ...))]))

;; --------------------------------------------------

(provide objc-is-a?)

(define (objc-is-a? v c)
  (ptr-equal? (object-get-class v) c))

;; --------------------------------------------------

(define-objc class_getInstanceMethod (_fun _Class _SEL -> _Method))
(define-objc method_setImplementation (_fun _Method _IMP -> _IMP)
  #:fail (lambda () (lambda (meth imp)
                      (set-objc_method-method_imp! 
                       (cast meth _Method _objc_method-pointer) 
                       (function-ptr imp _IMP)))))
