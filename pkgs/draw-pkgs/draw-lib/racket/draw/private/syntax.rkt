#lang racket/base
(require racket/class
         racket/stxparam
         (for-syntax racket/base))

(provide defclass defclass*
         def/public def/pubment def/public-final def/override def/override-final define/top case-args
         def/public-unimplemented define-unimplemented
         maybe-box? any? bool? nonnegative-real? positive-real? make-or-false make-box make-list make-alts 
         make-literal symbol-in integer-in real-in make-procedure
         method-name init-name
         let-boxes
         properties field-properties init-properties
         ->long
         assert)

(define-syntax-parameter class-name #f)

(define-syntax-rule (defclass name super . body)
  (defclass* name super () . body))
(define-syntax-rule (defclass* name super intfs . body)
  (define name
    (syntax-parameterize ([class-name 'name])
      (class* super intfs . body))))

(define-syntax (def/public stx)
  #`(def/thing define/public #,stx))
(define-syntax (def/pubment stx)
  #`(def/thing define/pubment #,stx))
(define-syntax (def/public-final stx)
  #`(def/thing define/public-final #,stx))
(define-syntax (def/override stx)
  #`(def/thing define/override #,stx))
(define-syntax (def/override-final stx)
  #`(def/thing define/override-final #,stx))
(define-syntax (define/top stx)
  #`(def/thing define #,stx))

(define (method-name class method)
  (string->symbol (format "~a in ~a" method class)))
(define (init-name class)
  (string->symbol (format "initialization for ~a" class)))

(define-syntax just-id
  (syntax-rules ()
    [(_ [id default]) id]
    [(_ id) id]))

(define-struct named-pred (pred make-name)
  #:property prop:procedure (struct-field-index pred))

(define (apply-pred pred val)
  (cond
   [(procedure? pred) (pred val)]
   [(class? pred) (val . is-a? . pred)]
   [(interface? pred) (val . is-a? . pred)]
   [else (error 'check-arg "unknown predicate type: ~e" pred)]))

(define (make-or-false pred)
  (make-named-pred (lambda (v)
                     (or (not v) (apply-pred pred v)))
                   (lambda ()
                     (string-append (predicate-name pred)
                                    " or #f"))))

(define (make-box pred)
  (make-named-pred (lambda (v)
                     (and (box? v) (apply-pred pred (unbox v))))
                   (lambda ()
                     (string-append "boxed " (predicate-name pred)))))

(define (make-list pred)
  (make-named-pred (lambda (v)
                     (and (list? v) (andmap (lambda (v) (apply-pred pred v)) v)))
                   (lambda ()
                     (string-append "list of " (predicate-name pred)))))

(define (make-alts a b)
  (make-named-pred (lambda (v)
                     (or (apply-pred a v) (apply-pred b v)))
                   (lambda ()
                     (string-append (predicate-name a)
                                    " or "
                                    (predicate-name b)))))

(define (make-literal lit)
  (make-named-pred (lambda (v) (equal? v lit))
                   (lambda () (if (symbol? lit)
                                  (format "'~s" lit)
                                  (format "~s" lit)))))

(define (make-symbol syms)
  (make-named-pred (lambda (v) (memq v syms))
                   (lambda ()
                     (let loop ([syms syms])
                       (cond
                        [(null? (cdr syms))
                         (format "'~s" (car syms))]
                        [(null? (cddr syms))
                         (format "'~s, or '~s" (car syms) (cadr syms))]
                        [else
                         (format "'~s, ~a" (car syms) (loop (cdr syms)))])))))
(define-syntax-rule (symbol-in sym ...)
  (make-symbol '(sym ...)))

(define (integer-in lo hi)
  (make-named-pred (lambda (v) (and (exact-integer? v)
                                    (<= lo v hi)))
                   (lambda ()
                     (format "exact integer in [~a, ~a]" lo hi))))
(define (real-in lo hi)
  (make-named-pred (lambda (v) (and (real? v)
                                    (<= lo v hi)))
                   (lambda ()
                     (format "real in [~a, ~a]" lo hi))))

(define (make-procedure arity)
  (make-named-pred (lambda (p)
                     (and (procedure? p)
                          (procedure-arity-includes? p arity)))
                   (lambda ()
                     (format "procedure (arity ~a)" arity))))

(define (check-arg val pred pos)
  (if (apply-pred pred val)
      #f
      (cons (predicate-name pred)
            (if (keyword? pos)
                (list val)
                pos))))

(define (predicate-name pred)
  (cond
   [(named-pred? pred) ((named-pred-make-name pred))]
   [(procedure? pred) (let ([s (symbol->string (object-name pred))])
                        (substring s 0 (sub1 (string-length s))))]
   [(or (class? pred) (interface? pred))
    (format "~a instance" (object-name pred))]
   [else "???"]))

(define maybe-box? (make-named-pred (lambda (v) (or (not v) (box? v)))
                                    (lambda () "box or #f")))
(define (any? v) #t)
(define (bool? v) #t)
(define (nonnegative-real? v) (and (real? v) (v . >= . 0)))
(define (positive-real? v) (and (real? v) (v . > . 0)))

(define (method-of cls nam)
  (if cls
      (string->symbol (format "~a method of ~a" nam cls))
      nam))

(define-syntax (def/thing stx)
  (syntax-case stx ()
    [(_ define/orig (_ (id arg ...)))
     (raise-syntax-error #f "missing body" stx)]
    [(_ define/orig (_ (id orig-arg ...) . body))
     (let ([extract (lambda (keep-kw? mode)
                      (let loop ([args (syntax->list #'(orig-arg ...))]
                                 [pos 0]
                                 [prev-kw #f])
                        (cond
                         [(null? args) null]
                         [(keyword? (syntax-e (car args)))
                          (if keep-kw?
                              (cons (car args) (loop (cdr args) pos (car args)))
                              (loop (cdr args) pos (car args)))]
                         [else (cons (syntax-case (car args) ()
                                       [[arg-type arg] (case mode
                                                         [(type) #'arg-type]
                                                         [(arg) #'arg]
                                                         [(id) (syntax-case #'arg ()
                                                                 [[id val] #'id]
                                                                 [_ #'arg])]
                                                         [(pos) (or prev-kw pos)])])
                                     (loop (cdr args) (if prev-kw pos (add1 pos)) #f))])))])
       (with-syntax ([(arg-id ...) (extract #f 'id)]
                     [(arg-rep ...) (extract #t 'id)]
                     [(arg ...) (extract #t 'arg)]
                     [(arg-type ...) (extract #f 'type)]
                     [(pos ...) (extract #f 'pos)])
         (with-syntax ([(_ _ orig-stx) stx]
                       [cname (syntax-parameter-value #'class-name)])
           (syntax/loc #'orig-stx
             (define/orig (id arg ...)
               (let ([bad (or (check-arg arg-id arg-type 'pos)
                              ...)])
                 (when bad
                   (type-error (method-of 'cname 'id) (car bad) (cdr bad) arg-rep ...)))
               (let ()
                 . body))))))]))

(define type-error
  (make-keyword-procedure
   (lambda (kws kw-args name expected pos . args)
     (if (number? pos)
         (apply raise-type-error name expected pos args)
         (raise-type-error name expected (car pos))))))
     

(define-for-syntax lifted (make-hash))
(define-syntax (lift-predicate stx)
  (syntax-case stx ()
    [(_ id) (identifier? #'id) #'id]
    [(_ expr)
     (let ([d (syntax->datum #'expr)])
       (or (hash-ref lifted d #f)
           (let ([id (syntax-local-lift-expression #'expr)])
             (hash-set! lifted d id)
             id)))]))

(define-syntax (case-args stx)
  (syntax-case stx ()
    [(_ expr [([arg-type arg] ...) rhs ...] ... who)
     (with-syntax ([((min-args-len . max-args-len) ...)
                    (map (lambda (args)
                           (let ([args (syntax->list args)])
                             (cons (let loop ([args args])
                                     (if (or (null? args)
                                             (not (identifier? (car args))))
                                         0
                                         (add1 (loop (cdr args)))))
                                   (length args))))
                         (syntax->list #'((arg ...) ...)))])
       #'(let* ([args expr]
                [len (length args)])
           (find-match
            (lambda (next)
              (if (and (len . >= . min-args-len)
                       (len . <= . max-args-len))
                  (apply
                   (lambda (arg ...)
                     (if (and (not (check-arg (just-id arg) (lift-predicate arg-type) 0)) ...)
                         (lambda () rhs ...)
                         next))
                   args)
                  next))
            ...
            (lambda (next)
              (bad-args who args)))))]))

(define (bad-args who args)
  (error who "bad argument combination:~a"
         (apply string-append (map (lambda (x) (format " ~e" x))
                                   args))))

(define-syntax find-match
  (syntax-rules ()
    [(_ proc)
     ((proc #f))]
    [(_ proc1 proc ...)
     ((proc1 (lambda () (find-match proc ...))))]))

(define-syntax-rule (let-boxes ([id init] ...)
                               call
                               body ...)
  (let ([id (box init)] ...)
    call
    (let ([id (unbox id)] ...)
      body ...)))

(define-syntax (do-properties stx)
  (syntax-case stx ()
    [(_ define-base check-immutable [[type id] expr ...] ...)
     (let ([ids (syntax->list #'(id ...))])
       (with-syntax ([(getter ...)
                      (map (lambda (id)
                             (datum->syntax id
                                            (string->symbol
                                             (format "get-~a" (syntax-e id)))
                                            id))
                           ids)]
                     [(setter ...)
                      (map (lambda (id)
                             (datum->syntax id
                                            (string->symbol
                                             (format "set-~a" (syntax-e id)))
                                            id))
                           ids)])
         #'(begin
             (define-base id expr ...) ...
             (define/public (getter) id) ...
             (def/public (setter [type v]) (check-immutable 'setter) (set! id (coerce type v))) ...)))]))

(define-syntax coerce
  (syntax-rules (bool?)
    [(_ bool? v) (and v #t)]
    [(_ _ v) v]))

(define-syntax properties
  (syntax-rules ()
    [(_ #:check-immutable check-immutable . props)
     (do-properties define check-immutable . props)]
    [(_ . props)
     (do-properties define void . props)]))
(define-syntax field-properties
  (syntax-rules ()
    [(_ #:check-immutable check-immutable . props)
     (do-properties define-field check-immutable . props)]
    [(_ . props)
     (do-properties define-field void . props)]))
(define-syntax-rule (define-field id val) (field [id val]))
(define-syntax init-properties
  (syntax-rules ()
    [(_ #:check-immutable check-immutable . props)
     (do-properties define-init check-immutable . props)]
    [(_ . props)
     (do-properties define-init void . props)]))
(define-syntax define-init
  (syntax-rules ()
    [(_ id val) (begin
                  (init [(internal id) val])
                  (define id internal))]
    [(_ id) (begin
              (init [(internal id)])
              (define id internal))]))
    

(define (->long i) 
  (cond
   [(eqv? -inf.0 i) (- (expt 2 64))]
   [(eqv? +inf.0 i) (expt 2 64)]
   [(eqv? +nan.0 i) 0]
   [else (inexact->exact (floor i))]))


(define-syntax-rule (assert e) (void))
; (define-syntax-rule (assert e) (unless e (error 'assert "failed: ~s" 'e)))

(define (unimplemented c m args) (error (if c (method-name c m) m) "unimplemented; args were ~e"
                                        args))

(define-syntax (def/public-unimplemented stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([cname (syntax-parameter-value #'class-name)])
       #'(define/public (id . args) (unimplemented 'cname 'id args)))]))

(define-syntax-rule (define-unimplemented id)
  (define (id . args) (unimplemented #f 'id args)))
