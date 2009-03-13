#lang scheme/base
(require (for-syntax scheme/base
                     scheme/match
                     scheme/private/sc
                     "rep-data.ss"
                     "rep.ss"
                     "codegen.ss"
                     "../util.ss")
         scheme/list
         scheme/match
         syntax/stx
         "runtime.ss")

(provide define-syntax-class
         parse-sc
         attrs-of

         syntax-parse
         syntax-parser
         with-patterns

         pattern
         ~and
         ~or
         ...*

         attribute

         (struct-out failed)

         this-syntax

         current-expression
         current-macro-name)

;; (define-syntax-class name SyntaxClassDirective* SyntaxClassRHS*)
;; (define-syntax-class (name id ...) SyntaxClassDirective* SyntaxClassRHS*)

;; A SCDirective is one of
;;   #:description String
;;   #:transparent

;; A SyntaxClassRHS is
;;   (pattern Pattern PatternDirective ...)

;; A Pattern is one of
;;   name:syntaxclass
;;   (Pattern . Pattern)
;;   (Pattern ... . Pattern)
;;   (((Pattern*) HeadDirective* *) ...* . Pattern)
;;   datum, including ()

;; A PatternDirective is one of
;;   #:declare name SyntaxClassName
;;   #:declare name (SyntaxClassName expr ...)
;;   #:rename internal-id external-id
;;   #:with pattern expr
;;     #:with clauses are let*-scoped
;;   #:when expr

;; A HeadDirective is one of
;;   #:min nat/#f
;;   #:max nat/#f
;;   #:opt
;;   #:mand
;;   -- For optional heads only:
;;     #:occurs id
;;       'id' is bound to #t is the pattern occurs, #f otherwise
;;     #:default form
;;       Preceding head must have a single pvar
;;       If the head is not present, the pvar is bound to 'form' instead

(define-syntax (define-syntax-class stx)
  (syntax-case stx ()
    [(define-syntax-class (name arg ...) . rhss)
     #`(begin (define-syntax name
                (let ([the-rhs
                       (parameterize ((current-syntax-context
                                       (quote-syntax #,stx)))
                         (parse-rhs (quote-syntax rhss) #t (quote-syntax #,stx)))])
                  (make sc 'name
                        '(arg ...)
                        (rhs-attrs the-rhs)
                        ((syntax-local-certifier) #'parser)
                        #'description)))
              (define-values (parser description)
                (rhs->parser+description name rhss (arg ...) #,stx)))]
    [(define-syntax-class name . rhss)
     (syntax/loc stx
       (define-syntax-class (name) . rhss))]))

(define-syntax (rhs->parser+description stx)
  (syntax-case stx ()
    [(rhs->parser+description name rhss (arg ...) ctx)
     (with-disappeared-uses
      (parameterize ((current-syntax-context #'ctx))
        (let ([rhs (parse-rhs #'rhss #f #'ctx)]
              [sc (syntax-local-value #'name)])
          #`(values #,(parse:rhs rhs
                                 (sc-attrs sc)
                                 (syntax->list #'(arg ...)))
                    (lambda (arg ...)
                      #,(or (rhs-description rhs)
                            #'(symbol->string 'name)))))))]))

(define-syntax (parse-sc stx)
  (syntax-case stx ()
    [(parse s x arg ...)
     (parameterize ((current-syntax-context stx))
       (let* ([arg-count (length (syntax->list #'(arg ...)))]
              [stxclass (get-stxclass/check-arg-count #'s arg-count)]
              [attrs (flatten-sattrs (sc-attrs stxclass))])
         (with-syntax ([parser (sc-parser-name stxclass)]
                       [(name ...) (map attr-name attrs)]
                       [(depth ...) (map attr-depth attrs)])
           #'(let ([raw (parser x arg ...)])
               (if (ok? raw)
                   (map vector '(name ...) '(depth ...) (cdr raw))
                   raw)))))]))

(define-syntax (attrs-of stx)
  (syntax-case stx ()
    [(attrs-of s)
     (parameterize ((current-syntax-context stx))
       (let ([attrs (flatten-sattrs (sc-attrs (get-stxclass #'s)))])
         (with-syntax ([(a ...) (map attr-name attrs)]
                       [(depth ...) (map attr-depth attrs)])
           #'(quote ((a depth) ...)))))]))

(define-syntax (debug-rhs stx)
  (syntax-case stx ()
    [(debug-rhs rhs)
     (let ([rhs (parse-rhs #'rhs #f stx)])
       #`(quote #,rhs))]))

(define-syntax-rule (syntax-parse stx-expr . clauses)
  (let ([x stx-expr])
    (syntax-parse* syntax-parse x . clauses)))

(define-syntax-rule (syntax-parser . clauses)
  (lambda (x) (syntax-parse* syntax-parser x . clauses)))

(define-syntax (syntax-parse* stx)
  (syntax-case stx ()
    [(syntax-parse report-as expr . clauses)
     (with-disappeared-uses
      (parameterize ((current-syntax-context
                      (syntax-property stx
                                       'report-errors-as
                                       (syntax-e #'report-as))))
        #`(let ([x expr])
            (let ([fail (syntax-patterns-fail x)])
              (parameterize ((current-expression (or (current-expression) x)))
                #,(parse:clauses #'clauses #'x #'fail))))))]))

(define-syntax with-patterns
  (syntax-rules ()
    [(with-patterns () . b)
     (let () . b)]
    [(with-patterns ([p x] . more) . b)
     (syntax-parse x [p (with-patterns more . b)])]))

(define ((syntax-patterns-fail stx0) x expected frontier frontier-stx)
  (define (err msg stx)
    (raise (make-exn:fail:syntax 
            (if msg
                (string->immutable-string (string-append "bad syntax: " msg))
                (string->immutable-string "bad syntax"))
            (current-continuation-marks)
            (list stx))))
  (define n (last frontier))
  (cond [(expectation-of-null? expected)
         ;; FIXME: "extra term(s) after <pattern>"
         (syntax-case x ()
           [(one)
            (err "unexpected term" #'one)]
           [(first . more)
            (err "unexpected terms starting here" #'first)]
           [_
            (err "unexpected term" x)])]
        [(and expected (expectation->string expected))
         =>
         (lambda (msg)
           (err (format "expected ~a~a"
                        msg
                        (cond [(zero? n) ""]
                              [(= n +inf.0) " after matching main pattern"]
                              [else (format " after ~s ~a"
                                            n
                                            (if (= 1 n) "form" "forms"))]))
                frontier-stx))]
        [else
         (err #f stx0)]))



#|
(begin-for-syntax
 (define (check-attrlist stx)
   (syntax-case stx ()
     [(form ...)
      (let ([names (for/list ([s (syntax->list #'(form ...))])
                     (check-attr s)
                     (stx-car s))])
        (check-duplicate-identifier names)
        stx)]
     [_
      (raise-syntax-error 'define-syntax-class
                          "expected attribute table" stx)]))
 (define stxclass-table
   `((#:description check-string)
     (#:attributes check-attrlist)))
 (define (split-rhss rhss stx)
   (define-values (chunks rest)
     (chunk-kw-seq/no-dups rhss stxclass-table  #:context stx))
   (define (assq* x alist default)
     (cond [(assq x alist) => cdr]
           [else default]))
   (values (cond [(assq '#:attributes chunks) => caddr]
                 [else null])
           (cond [(assq '#:description chunks) => caddr]
                 [else #f])
           rest)))

(define-syntax (define-syntax-class stx)
  (syntax-case stx ()
    [(define-syntax-class (name arg ...) . rhss)
     (let-values ([(attrs description rhss) (split-rhss #'rhss stx)])
       #`(begin (define-syntax name
                  (make sc
                    'name
                    '(arg ...)
                    '#,attrs
                    ((syntax-local-value) #'parser)
                    '#,description))
                (define parser
                  (rhs->parser name #,rhss (arg ...) #,stx))))]
    [(define-syntax-class name . rhss)
     (syntax/loc stx
       (define-syntax-class (name) . rhss))]))
|#

