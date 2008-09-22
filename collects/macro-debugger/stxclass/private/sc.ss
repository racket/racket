
#lang scheme/base
(require (for-syntax scheme/base
                     scheme/match
                     scheme/private/sc
                     "rep.ss"
                     "parse.ss"
                     "util.ss")
         scheme/match
         syntax/stx
         "kws.ss")
(provide define-syntax-class
         define-basic-syntax-class
         define-basic-syntax-class*
         parse-sc
         attrs-of

         debug-rhs
         debug-pattern

         syntax-parse
         syntax-parser
         with-patterns

         pattern
         ...*

         fail-sc
         (struct-out failed)

         current-expression
         current-macro-name)

(define-syntax (define-syntax-class stx)
  (syntax-case stx ()
    [(define-syntax-class (name arg ...) . rhss)
     #`(begin (define-syntax name
                (let ([the-rhs (parse-rhs (quote-syntax rhss) #t (quote-syntax #,stx))])
                  (make sc 'name
                        '(arg ...)
                        (rhs-attrs the-rhs)
                        ((syntax-local-certifier) #'parser)
                        (rhs:union-description the-rhs))))
              (define parser (rhs->parser name rhss (arg ...) #,stx)))]
    [(define-syntax-class name . rhss)
     (syntax/loc stx
       (define-syntax-class (name) . rhss))]))

#;
(define-syntax (define-syntax-splice-class stx)
  (syntax-case stx ()
    [(define-syntax-splice-class (name arg ...) . rhss)
     #`(begin (define-syntax name
                (make ssc 'name
                      '(arg ...)
                      (rhs-attrs
                       (parse-splice-rhs (quote-syntax rhss) #t (quote-syntax #,stx)))
                      ((syntax-local-certifier) #'parser)))
              (define parser (splice-rhs->parser name rhss (arg ...) #,stx)))]
    [(define-syntax-splice-class name . rhss)
     (syntax/loc stx (define-syntax-splice-class (name) . rhss))]))

(define-syntax define-basic-syntax-class
  (syntax-rules ()
    [(define-basic-syntax-class (name arg ...)
       ([attr-name attr-depth] ...)
       parser-expr)
     (define-basic-syntax-class* (name arg ...)
       ([attr-name attr-depth] ...)
       (let ([name parser-expr])
         (let ([name 
                (lambda (x arg ...)
                  (let ([r (name x arg ...)])
                    (if (ok? r)
                        (cons x r)
                        r)))])
           name)))]
    [(define-basic-syntax-class name
       ([attr-name attr-depth] ...)
       parser-expr)
     (define-basic-syntax-class (name)
       ([attr-name attr-depth] ...)
       parser-expr)]))

(define-syntax define-basic-syntax-class*
  (syntax-rules ()
    [(define-basic-syntax-class* (name arg ...)
       ([attr-name attr-depth] ...)
       parser-expr)
     (begin (define parser (let ([name parser-expr]) name))
            (define-syntax name
              (make sc 'name
                    '(arg ...)
                    (list (make-attr 'attr-name 'attr-depth null) ...)
                    ((syntax-local-certifier) #'parser)
                    #f)))]))

(define-syntax (rhs->parser stx)
  (syntax-case stx ()
    [(rhs->parser name rhss (arg ...) ctx)
     (let ([rhs (parse-rhs #'rhss #f #'ctx)]
           [sc (syntax-local-value #'name)])
       (parse:rhs rhs
                  (sc-attrs sc)
                  (syntax->list #'(arg ...))))]))

(define-syntax (parse-sc stx)
  (syntax-case stx ()
    [(parse s x arg ...)
     (let* ([stxclass (get-stxclass #'s)]
            [attrs (flatten-sattrs (sc-attrs stxclass))])
       (with-syntax ([parser (sc-parser-name stxclass)]
                     [(name ...) (map attr-name attrs)]
                     [(depth ...) (map attr-depth attrs)])
         #'(let ([raw (parser x arg ...)])
             (if (ok? raw)
                 (map vector '(name ...) '(depth ...) (cdr raw))
                 raw))))]))

(define-syntax (attrs-of stx)
  (syntax-case stx ()
    [(attrs-of s)
     (let ([attrs (flatten-sattrs (sc-attrs (get-stxclass #'s)))])
       (with-syntax ([(a ...) (map attr-name attrs)]
                     [(depth ...) (map attr-depth attrs)])
         #'(quote ((a depth) ...))))]))

(define-syntax (debug-rhs stx)
  (syntax-case stx ()
    [(debug-rhs rhs)
     (let ([rhs (parse-rhs #'rhs #f)])
       #`(quote #,rhs))]))

(define-syntax (debug-pattern stx)
  (syntax-case stx ()
    [(debug-pattern p)
     (let ([pattern (parse-pattern #'p)])
       #`(quote #,pattern))]))

(define-syntax (syntax-parser stx)
  (syntax-case stx ()
    [(syntax-parser . clauses)
     #`(lambda (x)
         (parameterize ((current-expression (or (current-expression) x)))
           #,(parse:clauses #'clauses #'x #'syntax-patterns-fail)))]))

(define-syntax (syntax-parse stx)
  (syntax-case stx ()
    [(syntax-parse expr . clauses)
     #`(let ([x expr])
         (parameterize ((current-expression (or (current-expression) x)))
           #,(parse:clauses #'clauses #'x #'syntax-patterns-fail)))]))

(define-syntax with-patterns
  (syntax-rules ()
    [(with-patterns () . b)
     (let () . b)]
    [(with-patterns ([p x] . more) . b)
     (syntax-parse x [p (with-patterns more . b)])]))

(define (syntax-patterns-fail x expected reason frontier)
  (define (err msg stx)
    (raise (make-exn:fail:syntax (string->immutable-string msg)
                                 (current-continuation-marks)
                                 (list stx))))
  (define-values (stx n) (frontier->syntax frontier))
  (cond [(stx-null? x)
         (err (format "missing ~s" expected)
              (datum->syntax stx x
                             (list (syntax-source stx)
                                   #f
                                   #f
                                   (and (syntax-position stx)
                                        (syntax-span stx)
                                        (+ (syntax-position stx)
                                           (syntax-span stx)
                                           -1))
                                   1)))]
        [(equal? expected '())
         ;; FIXME: "extra term(s) after <pattern>"
         (syntax-case x ()
           [(one)
            (err "unexpected term" #'one)]
           [(first . more)
            (err "unexpected terms starting here" #'first)]
           [_
            (err "expected end of list" x)])]
        [expected
         (err (format "~a~a"
                      expected
                      (cond [(zero? n) ""]
                            [(= n +inf.0) " after matching main pattern"]
                            [else (format " after ~s ~a"
                                          n
                                          (if (= 1 n) "form" "forms"))]))
              stx)]
        [reason
         (format "~a" reason)]
        [else
         (err "failed" stx)]))

(define (frontier->syntax f)
  (match f
    [(list x n)
     (values x n)]
    [(list-rest _ _ rest)
     (frontier->syntax rest)]))

(define (fail-sc stx #:pattern [pattern #f] #:reason [reason #f])
  (make-failed stx pattern reason))

(define (syntax-class-fail stx #:reason [reason #f])
  (make-failed stx #f reason))
