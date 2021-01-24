;; Force inlining of the fast path for various primitives that are
;; otherwise wrapped with impersonator checks

(define-syntax (define-inline stx)
  (syntax-case stx ()
    [(_ proto guard op)
     #'(define-inline proto guard op #3%$app/no-inline)]
    [(_ (orig-id arg ...) guard op orig-app)
     (with-syntax ([(tmp ...) (generate-temporaries #'(arg ...))]
                   [id (datum->syntax #'orig-id
                                      (#%string->symbol
                                       (string-append "inline:"
                                                      (#%symbol->string (syntax->datum #'orig-id)))))])
       #'(define-syntax (id stx)
           (syntax-case stx ()
             [(_ tmp ...)
              #'(let ([arg tmp] ...)
                  (if guard
                      op
                      (pariah (orig-app orig-id arg ...))))]
             [(_ . args)
              #'(orig-id . args)]
             [_ #'orig-id])))]))

(define-inline (vector-length v)
  (#%vector? v)
  (#3%vector-length v))

(define-inline (vector-ref v i)
  (#%$vector-ref-check? v i)
  (#3%vector-ref v i))

(define-inline (vector-set! v i n)
  (#%$vector-set!-check? v i)
  (#3%vector-set! v i n))

(define-inline (unsafe-vector-length v)
  (#%vector? v)
  (#3%vector-length v))

(define-inline (unsafe-vector-ref v i)
  (#%vector? v)
  (#3%vector-ref v i))

(define-inline (unsafe-vector-set! v i n)
  (#%vector? v)
  (#3%vector-set! v i n))

(define-inline (unbox b)
  (#%box? b)
  (#3%unbox b))

(define-inline (set-box! b v)
  (#%mutable-box? b)
  (#3%set-box! b v))

(define-inline (unsafe-unbox b)
  (#%box? b)
  (#3%unbox b))

(define-inline (unsafe-set-box! b v)
  (#%box? b)
  (#3%set-box! b v))

(define-inline (mcar p)
  (mpair? p)
  (unsafe-mcar p)
  |#%app/no-return|)

(define-inline (mcdr p)
  (mpair? p)
  (unsafe-mcdr p)
  |#%app/no-return|)

(define-inline (set-mcar! p v)
  (mpair? p)
  (unsafe-set-mcar! p v)
  |#%app/no-return|)

(define-inline (set-mcdr! p v)
  (mpair? p)
  (unsafe-set-mcdr! p v)
  |#%app/no-return|)

(define-inline (unsafe-struct-ref s i)
  (not (impersonator? s))
  (unsafe-struct*-ref s i))

(define-inline (unsafe-struct-set! s i v)
  (not (impersonator? s))
  (unsafe-struct*-set! s i v))
