#lang racket/base

(provide debug
         dprintf
         debugf
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
         parameterize/debug)

(require racket/match
         unstable/pretty
         syntax/srcloc
         syntax/location
         racket/syntax
         (for-syntax racket/base syntax/parse racket/syntax))

(define-syntax (let/debug stx)
  (syntax-parse stx
    [(_ (~optional loop:id) ([lhs:id rhs:expr] ...) body:expr ...+)
     #`(debug
        #:name '#,(if (attribute loop) #'loop #'let/debug)
        #:source (quote-srcloc #,stx)
        (let #,@(if (attribute loop) (list #'loop) null)
          ([lhs (debug #:name 'lhs rhs)] ...)
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
           #`(debug
              #:name 'binder/debug
              #:source (quote-srcloc #,stx)
              (binder
               ([lhs (debug #:name 'lhs rhs)] ...)
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
               (debug
                #:name 'name
                #:source (quote-srcloc #,stx)
                body))]
          [(definer/debug:id spec:header body:expr ...+)
           #`(definer spec
               (debug
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
     #`(debug
        #:name 'begin/debug
        #:source (quote-srcloc #,stx)
        (begin (debug term) ...))]))

(define-syntax (debugf stx)

  (define-splicing-syntax-class argument
    #:attributes ([debugged 1])
    (pattern arg:expr
             #:attr [debugged 1] (list #'(debug arg)))
    (pattern (~seq kw:keyword arg:expr)
             #:attr [debugged 1] (list #'kw #'(debug arg))))

  (syntax-parse stx
    [(_ f:expr arg:argument ...)
     #`(debug
        #:name 'debugf
        #:source (quote-srcloc #,stx)
        (#%app (debug f) arg.debugged ... ...))]))

(define-syntax (debug stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:name name:expr))
             (~optional (~seq #:source source:expr)))
        ...
        body:expr)
     (with-syntax* ([name (or (attribute name) #'(quote body))]
                    [source (or (attribute source) #'(quote-srcloc body))])
       #'(debug/proc
          name
          source
          (lambda () (#%expression body))))]))

(define (debug/proc name source thunk)

  (define src (source-location->prefix source))

  (define (err e)
    (if (exn? e)
      (dprintf "raised exception: ~a" (exn-message e))
      (dprintf "raised non-exception: ~a" (pretty-format/print e)))
    (raise e))

  (define depth (current-debug-depth))

  (dynamic-wind

    (lambda ()
      (parameterize ([current-debug-depth depth])
        (dprintf ">> ~a~a" src (pretty-format/write name 'infinity))))

    (lambda ()
      (parameterize ([current-debug-depth (add1 depth)])
        (with-handlers ([(lambda _ #t) err])
          (call-with-values thunk
            (match-lambda*
              [(list v)
               (dprintf "result: ~a"
                 (pretty-format/print v 'infinity))
               v]
              [(list vs ...)
               (dprintf "results: (values~a)"
                 (apply string-append
                   (for/list ([v (in-list vs)])
                     (string-append " " (pretty-format/print v 'infinity)))))
               (apply values vs)])))))

    (lambda ()
      (parameterize ([current-debug-depth depth])
        (dprintf "<< ~a~a" src (pretty-format/write name 'infinity))))))

(define (dprintf fmt . args)
  (define message (apply format fmt args))
  (define terminated
    (if (regexp-match? "\n$" message) message (string-append message "\n")))
  (define prefix
    (make-string (* debug-indent (current-debug-depth)) #\space))
  (define indented
    (regexp-replace* "(?m:.+)" terminated (string-append prefix "&")))
  (write-string indented (current-error-port))
  (void))

(define current-debug-depth (make-parameter 0))
(define debug-indent 2)
