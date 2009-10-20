#lang scheme/base
(require scheme/contract/base
         scheme/stxparam
         scheme/list
         (for-syntax scheme/base
                     syntax/stx
                     scheme/private/sc
                     "rep-data.ss"
                     "rep-attrs.ss"
                     "../util.ss"))

(provide pattern
         ~var
         ~literal
         ~and
         ~or
         ~not
         ~seq
         ~bounds
         ~once
         ~optional
         ~rest
         ~struct
         ~describe
         ~!
         ~bind
         ~fail
         ~parse

         current-expression
         current-macro-name

         this-syntax

         compare-dfcs

         expect?
         expectation?
         (struct-out expect:thing)
         (struct-out expect:atom)
         (struct-out expect:literal)
         (struct-out expect:message)
         (struct-out expect:pair)
         (struct-out expect:disj)
         merge-expectations
         expect->alternatives
         ineffable?

         expectation-of-null?

         enclosing-fail
         enclosing-cut-fail
         with-enclosing-fail
         with-enclosing-cut-fail
         with-enclosing-fail*
         without-fails

         ok?
         (struct-out failure)
         (struct-out join-failures)

         try

         stx-list-take

         let-attributes
         attribute
         let/unpack
         attribute-binding
         check-list^depth)

;; == Keywords

(define-syntax-rule (define-keyword name)
  (define-syntax name
    (lambda (stx)
      (raise-syntax-error #f "keyword used out of context" stx))))

(define-keyword pattern)
(define-keyword ~var)
(define-keyword ~literal)
(define-keyword ~and)
(define-keyword ~or)
(define-keyword ~not)
(define-keyword ~seq)
(define-keyword ~bounds)
(define-keyword ~once)
(define-keyword ~optional)
(define-keyword ~rest)
(define-keyword ~struct)
(define-keyword ~describe)
(define-keyword ~!)
(define-keyword ~bind)
(define-keyword ~fail)
(define-keyword ~parse)

;; == Parameters & Syntax Parameters

;; this-syntax
;; Bound to syntax being matched inside of syntax class
(define-syntax-parameter this-syntax
  (lambda (stx)
    (wrong-syntax stx "used out of context: not within a syntax class")))

(define current-expression (make-parameter #f))

(define (current-macro-name)
  (let ([expr (current-expression)])
    (and expr
         (syntax-case expr (set!)
           [(set! kw . _)
            #'kw]
           [(kw . _)
            (identifier? #'kw)
            #'kw]
           [kw
            (identifier? #'kw)
            #'kw]
           [_ #f]))))


;; == Dynamic Frontier Contexts (DFCs)

;; A DFC is a list of numbers.

;; compare-dfcs : DFC DFC -> (one-of '< '= '>)
;; Note A>B means A is "further along" than B.
(define (compare-dfcs a b)
  (cond [(and (null? a) (null? b))
         '=]
        [(and (pair? a) (null? b))
         '>]
        [(and (null? a) (pair? b))
         '<]
        [(and (pair? a) (pair? b))
         (cond [(> (car a) (car b)) '>]
               [(< (car a) (car b)) '<]
               [else (compare-dfcs (cdr a) (cdr b))])]))

;; == Codegen internal syntax parameters

(define-for-syntax not-allowed/not-parsing
  (lambda (stx)
    (wrong-syntax stx "used out of context: not parsing pattern")))

(define-syntax-parameter pattern-source not-allowed/not-parsing)

;; Two levels of fail continuation:
;;  - enclosing-fail : ordinary fail
;;  - enclosing-cut-fail : last cut "prompt"

(define-syntax-parameter enclosing-fail not-allowed/not-parsing)
(define-syntax-parameter enclosing-cut-fail not-allowed/not-parsing)

(define-syntax-rule (with-enclosing-fail failvar expr)
  (syntax-parameterize ((enclosing-fail (make-rename-transformer (quote-syntax failvar))))
    expr))

(define-syntax-rule (with-enclosing-cut-fail failvar expr)
  (syntax-parameterize ((enclosing-cut-fail (make-rename-transformer (quote-syntax failvar))))
    expr))

(define-syntax-rule (with-enclosing-fail* failvar expr)
  (syntax-parameterize ((enclosing-fail (make-rename-transformer (quote-syntax failvar)))
                        (enclosing-cut-fail (make-rename-transformer (quote-syntax failvar))))
    expr))

(define-syntax-rule (without-fails body)
  (syntax-parameterize ((enclosing-fail not-allowed/not-parsing)
                        (enclosing-cut-fail not-allowed/not-parsing))
    body))


;; == Success and Failure

;; A Failure is one of
;;   (make-failure stx DFC stx expectation/c)
;;   (make-join-failures Failure Failure)

(define ok? list?)

(define-struct failure (stx frontier frontier-stx expectation) #:prefab)
(define-struct join-failures (f1 f2) #:prefab)

;; (try expr ...)
(define-syntax (try stx)
  (syntax-case stx ()
    [(try expr ...)
     (when (stx-null? #'(expr ...))
       (raise-syntax-error #f "must have at least one attempt" stx))
     #'(try* (list (lambda (fail)
                     (with-enclosing-fail fail expr))
                   ...)
             enclosing-fail)]))

;; FailFunction = (Failure -> Result)

;; try* : (nonempty-listof (-> FailFunction Result)) FailFunction -> Result
(define (try* attempts fail)
  (let ([first-attempt (car attempts)]
        [rest-attempts (cdr attempts)])
    (if (null? rest-attempts)
        (first-attempt fail)
        (let ([next-fail
               (lambda (f1)
                 (let ([combining-fail
                        (lambda (f2)
                          (fail (make-join-failures f1 f2)))])
                   (try* rest-attempts combining-fail)))])
          (first-attempt next-fail)))))


;; == Expectations

#|
An Expectation is one of
  'ineffable
  (make-expect:thing string boolean Failure/#f)
  (make-expect:atom atom)
  (make-expect:literal identifier)
  (make-expect:message string)
  (make-expect:pair)
  (make-expect:disj Expectation Expectation)
|#
(define-struct expect:thing (description transparent? chained) #:prefab)
(define-struct expect:atom (atom) #:prefab)
(define-struct expect:literal (literal) #:prefab)
(define-struct expect:message (message) #:prefab)
(define-struct expect:pair () #:prefab)
(define-struct expect:disj (a b) #:prefab)

(define (expect? x)
  (or (expect:thing? x)
      (expect:atom? x)
      (expect:literal? x)
      (expect:message? x)
      (expect:pair? x)
      (expect:disj? x)))

(define expectation?
  (or/c expect? (symbols 'ineffable)))

(define (merge-expectations a b)
  (make-expect:disj a b))

;; expect->alternatives : Expectation -> (listof Expectation)/#f
;; #f indicates 'ineffable somewhere in expectation
(define (expect->alternatives e)
  (define (loop-onto e rest)
    (cond [(expect:disj? e)
           (loop-onto (expect:disj-a e)
                      (loop-onto (expect:disj-b e) rest))]
          [else (cons e rest)]))
  (let ([alts (remove-duplicates (loop-onto e null))])
    (if (for/or ([alt alts]) (eq? alt 'ineffable))
        #f
        alts)))

(define (expectation-of-null? e)
  (or (equal? e '#s(expect:atom ()))
      (and (expect:disj? e)
           (expectation-of-null? (expect:disj-a e))
           (expectation-of-null? (expect:disj-b e)))))

(define (ineffable? e)
  (or (eq? e 'ineffable)
      (and (expect:disj? e)
           (or (ineffable? (expect:disj-a e))
               (ineffable? (expect:disj-b e))))))


;; -----

(require syntax/stx)
(define (stx-list-take stx n)
  (datum->syntax stx
                 (let loop ([stx stx] [n n])
                   (if (zero? n)
                       null
                       (cons (stx-car stx)
                             (loop (stx-cdr stx) (sub1 n)))))
                 stx))

;; == Attributes

(begin-for-syntax
 (define-struct attribute-mapping (var name depth syntax?)
   #:omit-define-syntaxes
   #:property prop:procedure
   (lambda (self stx)
     (if (attribute-mapping-syntax? self)
         #`(#%expression #,(attribute-mapping-var self))
         #`(let ([value #,(attribute-mapping-var self)])
             (if (check-syntax '#,(attribute-mapping-depth self) value)
                 value
                 (raise-syntax-error #f
                                     "attribute is bound to non-syntax value"
                                     (quote-syntax
                                      #,(datum->syntax
                                         stx
                                         (attribute-mapping-name self)
                                         stx)))))))))

;; check-syntax : nat any -> boolean
;; Returns #t if value is a (listof^depth syntax)
(define (check-syntax depth value)
  (if (zero? depth)
      (syntax? value)
      (and (list? value)
           (for/and ([part value])
             (check-syntax (sub1 depth) part)))))

(define-syntax (let-attributes stx)
  (define (parse-attr x)
    (syntax-case x ()
      [#s(attr name depth syntax?) #'(name depth syntax?)]))
  (syntax-case stx ()
    [(let-attributes ([a value] ...) . body)
     (with-syntax ([((name depth syntax?) ...)
                    (map parse-attr (syntax->list #'(a ...)))])
       (with-syntax ([(vtmp ...) (generate-temporaries #'(name ...))]
                     [(stmp ...) (generate-temporaries #'(name ...))])
         #'(letrec-syntaxes+values
               ([(stmp) (make-attribute-mapping (quote-syntax vtmp) 'name 'depth 'syntax?)] ...)
               ([(vtmp) value] ...)
             (letrec-syntaxes+values
                 ([(name) (make-syntax-mapping 'depth (quote-syntax stmp))] ...)
                 ()
               . body))))]))

(define-syntax (attribute stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      [(attribute name)
       (identifier? #'name)
       (let ([mapping (syntax-local-value #'name (lambda () #f))])
         (unless (syntax-pattern-variable? mapping)
           (wrong-syntax #'name "not bound as a pattern variable"))
         (let ([var (syntax-mapping-valvar mapping)])
           (let ([attr (syntax-local-value var (lambda () #f))])
             (unless (attribute-mapping? attr)
               (wrong-syntax #'name "not bound as an attribute"))
             (syntax-property (attribute-mapping-var attr)
                              'disappeared-use
                              #'name))))])))

;; (let/unpack (([id num] ...) expr) expr) : expr
;; Special case: empty attrs need not match packed length
(define-syntax (let/unpack stx)
  (syntax-case stx ()
    [(let/unpack (() packed) body)
     #'body]
    [(let/unpack ((a ...) packed) body)
     (with-syntax ([(tmp ...) (generate-temporaries #'(a ...))])
       #'(let-values ([(tmp ...) (apply values packed)])
           (let-attributes ([a tmp] ...) body)))]))

;; (attribute-binding id)
;; mostly for debugging/testing
(define-syntax (attribute-binding stx)
  (syntax-case stx ()
    [(attribute-bound? name)
     (identifier? #'name)
     (let ([value (syntax-local-value #'name (lambda () #f))])
       (if (syntax-pattern-variable? value)
           (let ([value (syntax-local-value (syntax-mapping-valvar value) (lambda () #f))])
             (if (attribute-mapping? value)
                 #`(quote #,(make-attr (attribute-mapping-name value)
                                       (attribute-mapping-depth value)
                                       (attribute-mapping-syntax? value)))
                 #'(quote #f)))
           #'(quote #f)))]))

;; (check-list^depth attr expr)
(define-syntax (check-list^depth stx)
  (syntax-case stx ()
    [(_ a expr)
     (with-syntax ([#s(attr name depth syntax?) #'a])
       (quasisyntax/loc #'expr
         (check-list^depth* 'name 'depth expr)))]))

(define (check-list^depth* aname n0 v0)
  (define (loop n v)
    (when (positive? n)
      (unless (list? v)
        (raise-type-error aname (format "lists nested ~s deep" n0) v))
      (for ([x v]) (loop (sub1 n) x))))
  (loop n0 v0)
  v0)
