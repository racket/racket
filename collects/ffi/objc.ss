#lang scheme/base
(require scheme/foreign
         scheme/stxparam
         (for-syntax scheme/base))
(unsafe!)

(define objc-lib (ffi-lib "libobjc"))

(define-syntax define-objc/private
  (syntax-rules ()
    [(_ id type)
     (define-objc/private id id type)]
    [(_ id c-id type)
     (define id (get-ffi-obj 'c-id objc-lib type))]))

(define-syntax-rule (define-objc id type)
  (begin
    (provide* (unsafe id))
    (define-objc/private id id type)))

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
(define _IMP (_fun _id _id -> _id))

(define-cstruct _objc_super ([receiver _id][class _Class]))

(provide YES NO)
(define YES #t)
(define NO #f)

;; ----------------------------------------

(define-objc objc_lookUpClass (_fun _string -> _Class))
(define-objc objc_getProtocol (_fun _string -> _Protocol))

(define-objc sel_registerName (_fun _string -> _SEL))

(define-objc objc_allocateClassPair (_fun _Class _string _long -> _Class))
(define-objc objc_registerClassPair (_fun _Class -> _void))

(define-objc object_getClass (_fun _id -> _Class))

(provide class_addMethod)
(define (class_addMethod cls sel imp ty enc)
  ((get-ffi-obj 'class_addMethod objc-lib (_fun _Class _SEL ty _string -> _BOOL))
   cls sel imp enc))

(define-objc class_addIvar (_fun _Class _string _long _uint8 _string -> _BOOL))
(define-objc object_getInstanceVariable (_fun _id _string [p : (_ptr o _pointer)]
                                              -> [ivar : _Ivar] 
                                              -> (values ivar p)))
(define-objc object_setInstanceVariable (_fun _id _string _pointer -> _Ivar))

(define-objc class_addProtocol (_fun _Class _Protocol -> _BOOL))

(define-objc/private objc_msgSend _fpointer)
(define-objc/private objc_msgSend_fpret _fpointer)
(define-objc/private objc_msgSend_stret _fpointer)
(define-objc/private objc_msgSendSuper _fpointer)
(define objc_msgSendSuper_fpret objc_msgSendSuper) ; why no fpret variant?
(define-objc/private objc_msgSendSuper_stret _fpointer)

(define sizes-for-direct-struct-results
  (case (string->symbol (path->string (system-library-subpath #f)))
    [(i386-macosx i386-darwin) '(1 2 4 8)]
    [(ppc-macosx ppc-darwin) '(1 2 3 4)]
    [(x86_64-macosx x86_86-darwin) 
     ;; Do we need more analysis for unaligned fields?
     '(1 2 3 4 5 6 7 8)]))

(define (lookup-send types msgSends msgSend msgSend_fpret msgSend_stret first-arg-type)
  ;; First type in `types' vector is the result type
  (or (hash-ref msgSends types #f)
      (let ([ret-layout (ctype->layout (vector-ref types 0))])
        (if (and (list? ret-layout)
                 (not (memq (ctype-sizeof (vector-ref types 0)) 
                            sizes-for-direct-struct-results)))
            ;; Structure return type:
            (let* ([pre-m (function-ptr msgSend_stret
                                        (_cprocedure
                                         (list* _pointer first-arg-type _SEL (cdr (vector->list types)))
                                         _void))]
                   [m (lambda args
                        (let ([v (malloc (vector-ref types 0))])
                          (apply pre-m v args)
                          (ptr-ref v (vector-ref types 0))))])
              (hash-set! msgSends types m)
              m)
            ;; Non-structure return type:
            (let ([m (function-ptr (if (memq ret-layout
                                             '(float double double*))
                                       msgSend_fpret
                                       msgSend)
                                   (_cprocedure
                                    (list* first-arg-type _SEL (cdr (vector->list types)))
                                    (vector-ref types 0)))])
              (hash-set! msgSends types m)
              m)))))

(define msgSends (make-hash))
(define (objc_msgSend/typed types)
  (lookup-send types msgSends objc_msgSend objc_msgSend_fpret objc_msgSend_stret _id))
(provide* (unsafe objc_msgSend/typed))

(define msgSendSupers (make-hash))
(define (objc_msgSendSuper/typed types)
  (lookup-send types msgSendSupers objc_msgSendSuper objc_msgSendSuper_fpret objc_msgSendSuper_stret _pointer))
(provide* (unsafe objc_msgSendSuper/typed))

;; ----------------------------------------

(provide* (unsafe import-class))
(define-syntax (import-class stx)
  (syntax-case stx ()
    [(_ id)
     (quasisyntax/loc stx
       (define id (objc_lookUpClass #,(symbol->string (syntax-e #'id)))))]
    [(_ id ...)
     (syntax/loc stx (begin (import-class id) ...))]))

(provide* (unsafe import-protocol))
(define-syntax (import-protocol stx)
  (syntax-case stx ()
    [(_ id)
     (quasisyntax/loc stx
       (define id (objc_getProtocol #,(symbol->string (syntax-e #'id)))))]
    [(_ id ...)
     (syntax/loc stx (begin (import-protocol id) ...))]))

;; ----------------------------------------
;; iget-value and set-ivar! work only with fields that contain Scheme values

(provide* (unsafe get-ivar) (unsafe set-ivar!))

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
    (and p (ptr-ref p _scheme))))
      

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
        (ptr-set! p _scheme val)
        (let ([p (malloc-immobile-cell val)])
          (void (object_setInstanceVariable obj name p))))))

(define (free-fields obj names)
  (for-each (lambda (name)
              (let-values ([(ivar p) (object_getInstanceVariable obj name)])
                (when p (free-immobile-cell p))))
            names))

;; ----------------------------------------

(define-for-syntax method-sels (make-hash))

(define-for-syntax (register-selector sym)
  (or (hash-ref method-sels (cons (syntax-local-lift-context) sym) #f)
      (let ([id (syntax-local-lift-expression
                 #`(sel_registerName #,(symbol->string sym)))])
        (hash-set! method-sels sym id)
        id)))

(provide* (unsafe selector))
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

(provide* (unsafe tell) (unsafe tellv))
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

(provide* (unsafe define-objc-class) 
          (unsafe define-objc-mixin) 
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
               (define id (objc_allocateClassPair superclass-id id-str 0))
               (void (class_addProtocol id proto)) ...
               (add-ivar id 'ivar) ...
               (let-syntax ([ivar (make-ivar-form 'ivar)] ...)
                 (add-method whole-stx id superclass-id method) ...
                 (mixin id superclass-id '(ivar ...)) ...
                 (add-method whole-stx id superclass-id dealloc-method) ...
                 (void))
               (objc_registerClassPair id))))))]
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
                   [protocol-id proto] ...)
               (lambda (to-id superclass-id ivars)
                 (check-expected-ivars 'id ivars '(ivar ...))
                 (void (class_addProtocol to-id proto-id)) ...
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
                            [in-cls (if in-class?
                                        #'(object_getClass cls)
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
                                           (_fun #:atomic? atomic? #:keep save-method! _id _id arg-type ... -> rt)
                                           (generate-layout rt (list arg-id ...)))))))))]
         [else (raise-syntax-error #f
                                   "bad method form"
                                   stx
                                   #'m)]))]))

(define methods (make-hasheq))
(define (save-method! m)
  ;; Methods are never GCed, since classes are never unregistered
  (hash-set! methods m #t)
  m)

(define (add-ivar cls name)
  (void (class_addIvar cls
                       (symbol->string name)
                       (ctype-sizeof _pointer)
                       (sub1 (integer-length (ctype-alignof _pointer)))
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

(provide* (unsafe objc-is-a?))

(define (objc-is-a? v c)
  (ptr-equal? (object_getClass v) c))

;; ----------------------------------------

(define-unsafer objc-unsafe!)

