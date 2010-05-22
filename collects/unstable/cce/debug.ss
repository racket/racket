#lang scheme

(provide debug
         dprintf
         begin/debug
         define/debug
         define/private/debug
         define/public/debug
         define/override/debug
         define/augment/debug
         let/debug
         let*/debug
         letrec/debug
         let-values/debug
         let*-values/debug
         letrec-values/debug
         with-syntax/debug
         with-syntax*/debug
         parameterize/debug
         with-debugging)

(require unstable/srcloc
         unstable/location
         unstable/syntax
         (for-syntax scheme/match syntax/parse unstable/syntax))

(define-syntax (let/debug stx)
  (syntax-parse stx
    [(_ (~optional loop:id) ([lhs:id rhs:expr] ...) body:expr ...+)
     #`(with-debugging
        #:name '#,(if (attribute loop) #'loop #'let/debug)
        #:source (quote-srcloc #,stx)
        (let #,@(if (attribute loop) (list #'loop) null)
          ([lhs (with-debugging #:name 'lhs rhs)] ...)
          (debug body) ...))]))

(define-syntaxes
  [ let*/debug
    letrec/debug
    let-values/debug
    let*-values/debug
    letrec-values/debug
    with-syntax/debug
    with-syntax*/debug
    parameterize/debug ]

  (let ()

    (define ((expander binder-id) stx)
      (with-syntax ([binder binder-id])
        (syntax-parse stx
          [(binder/debug:id ([lhs rhs:expr] ...) body:expr ...+)
           #`(with-debugging
              #:name 'binder/debug
              #:source (quote-srcloc #,stx)
              (binder
               ([lhs (with-debugging #:name 'lhs rhs)] ...)
               (debug body) ...))])))

    (values (expander #'let*)
            (expander #'letrec)
            (expander #'let-values)
            (expander #'let*-values)
            (expander #'letrec-values)
            (expander #'with-syntax)
            (expander #'with-syntax*)
            (expander #'parameterize))))

(define-syntaxes
  [ define/debug
    define/private/debug
    define/public/debug
    define/override/debug
    define/augment/debug ]

  (let ()

    (define-syntax-class header
      #:attributes [name]
      (pattern (name:id . _))
      (pattern (inner:header . _) #:attr name (attribute inner.name)))

    (define ((expander definer-id) stx)
      (with-syntax ([definer definer-id])
        (syntax-parse stx
          [(definer/debug:id name:id body:expr)
           #`(definer name
               (with-debugging
                #:name 'name
                #:source (quote-srcloc #,stx)
                body))]
          [(definer/debug:id spec:header body:expr ...+)
           #`(definer spec
               (with-debugging
                #:name 'spec.name
                #:source (quote-srcloc #,stx)
                (let () body ...)))])))

    (values (expander #'define)
            (expander #'define/private)
            (expander #'define/public)
            (expander #'define/override)
            (expander #'define/augment))))

(define-syntax (begin/debug stx)
  (syntax-parse stx
    [(_ term:expr ...)
     #`(with-debugging
        #:name 'begin/debug
        #:source (quote-srcloc #,stx)
        (begin (debug term) ...))]))

(define-syntax (debug stx)
  (syntax-parse stx
    [(_ term:expr)
     (syntax (with-debugging term))]))

(define-syntax (with-debugging stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:name name:expr))
             (~optional (~seq #:source source:expr)))
        ...
        body:expr)
     (with-syntax* ([name (or (attribute name) #'(quote body))]
                    [source (or (attribute source) #'(quote-srcloc body))])
       #'(with-debugging/proc
          name
          source
          (quote body)
          (lambda () (#%expression body))))]))

(define (with-debugging/proc name source term thunk)
  (let* ([src (source-location->prefix source)])
    (begin
      (dprintf ">> ~a~s" src name)
      (begin0
        (parameterize ([current-debug-depth
                        (add1 (current-debug-depth))])
          (call-with-values thunk
            (lambda results
              (match results
                [(list v) (dprintf "~s" v)]
                [(list vs ...)
                 (dprintf "(values~a)"
                          (apply string-append
                            (for/list ([v (in-list vs)])
                              (format " ~s" v))))])
              (apply values results))))
        (dprintf "<< ~a~s" src name)))))

(define (dprintf fmt . args)
  (let* ([message (apply format fmt args)]
         [prefix (make-string (* debug-indent (current-debug-depth)) #\space)]
         [indented
          (string-append
           prefix
           (regexp-replace* "\n" message (string-append "\n" prefix)))])
    (log-debug indented)))

(define current-debug-depth (make-parameter 0))
(define debug-indent 2)
