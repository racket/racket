#lang scheme/base
(require scheme/contract
         scheme/match
         scheme/stxparam
         (for-syntax scheme/base)
         (for-syntax syntax/stx)
         (for-syntax scheme/private/sc)
         (for-syntax "rep-data.ss")
         (for-syntax "../util/error.ss"))
(provide pattern
         ~and
         ~or
         ...*

         with-enclosing-fail
         enclosing-fail

         ok?
         (struct-out failed)

         current-expression
         current-macro-name

         this-syntax

         (for-syntax expectation-of-stxclass
                     expectation-of-constants
                     expectation-of/message)

         try
         expectation/c
         expectation-of-null?
         expectation->string

         let-attributes
         attribute)

;; Keywords

(define-syntax-rule (define-keyword name)
  (define-syntax name
    (lambda (stx)
      (raise-syntax-error #f "keyword used out of context" stx))))

(define-keyword pattern)
(define-keyword ~and)
(define-keyword ~or)
(define-keyword ...*)

;; Parameters & Syntax Parameters

(define-syntax-parameter enclosing-fail
  (lambda (stx)
    (wrong-syntax stx "used out of context: not parsing pattern")))

(define-syntax-rule (with-enclosing-fail failvar expr)
  (syntax-parameterize ((enclosing-fail
                         (make-rename-transformer (quote-syntax failvar))))
    expr))

(define-syntax-parameter pattern-source
  (lambda (stx)
    (wrong-syntax stx "used out of context: not parsing pattern")))

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

;; Runtime: syntax-class parser results

;; A PatternParseResult is one of
;;   - (listof value)
;;   - (make-failed stx expectation/c frontier/#f stx)

(define (ok? x) (or (pair? x) (null? x)))
(define-struct failed (stx expectation frontier frontier-stx)
  #:transparent)

;; Runtime: Dynamic Frontier Contexts (DFCs)

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

;; Runtime: parsing failures/expectations

;; An Expectation is
;;   (make-expc (listof scdyn) (listof string/#t) (listof atom) (listof id))
(define-struct expc (stxclasses compound data literals)
  #:transparent)

(define-struct scdyn (name desc failure)
  #:transparent)

(define expectation/c (or/c expc?))

(define (make-stxclass-expc scdyn)
  (make-expc (list scdyn) null null null))

(begin-for-syntax
  (define certify (syntax-local-certifier))
  (define (expectation-of-stxclass stxclass args result-var)
    (unless (sc? stxclass)
      (raise-type-error 'expectation-of-stxclass "stxclass" stxclass))
    (with-syntax ([name (sc-name stxclass)]
                  [desc-var (sc-description stxclass)]
                  [(arg ...) args])
      (certify #`(begin
                   (make-stxclass-expc
                    (make-scdyn 'name (desc-var arg ...)
                                (if (failed? #,result-var) #,result-var #f)))))))

  (define (expectation-of-constants pairs? data literals description)
    (with-syntax ([(datum ...) data]
                  [(literal ...) literals]
                  [pairs? pairs?]
                  [description
                   (if pairs?
                       (list (or description #t))
                       null)])
      (certify
       #'(make-expc null 'description (list 'datum ...)
                    (list (quote-syntax literal) ...)))))

  (define (expectation-of/message msg)
    (with-syntax ([msg msg])
      (certify
       #'(make-expc '() '() '((msg)) '())))))

(define-syntax (try stx)
  (syntax-case stx ()
    [(try failvar (expr ...) previous-fail)
     (when (stx-null? #'(expr ...))
       (raise-syntax-error #f "must have at least one attempt" stx))
     #'(try* (list (lambda (failvar) expr) ...) previous-fail)]))

;; try* : (nonempty-listof (-> FailFunction Result)) FailFunction -> Result
;; FailFunction = (stx expectation/c ?? DynamicFrontier) -> Result
(define (try* attempts fail)
  (let ([first-attempt (car attempts)]
        [rest-attempts (cdr attempts)])
    (if (null? rest-attempts)
        (first-attempt fail)
        (let ([next-fail
               (lambda (x1 p1 f1 fs1)
                 (let ([combining-fail
                        (lambda (x2 p2 f2 fs2)
                          (choose-error fail x1 x2 p1 p2 f1 f2 fs1 fs2))])
                   (try* rest-attempts combining-fail)))])
          (first-attempt next-fail)))))

(define (choose-error k x1 x2 p1 p2 frontier1 frontier2 fs1 fs2)
  (case (compare-dfcs frontier1 frontier2)
    [(>) (k x1 p1 frontier1 fs1)]
    [(<) (k x2 p2 frontier2 fs2)]
    [(=) (k x1 (merge-expectations p1 p2) frontier1 fs1)]))

(define (merge-expectations e1 e2)
  (make-expc (union (expc-stxclasses e1) (expc-stxclasses e2))
             (union (expc-compound e1) (expc-compound e2))
             (union (expc-data e1) (expc-data e2))
             (union (expc-literals e1) (expc-literals e2))))

(define (union a b)
  (append a (for/list ([x b] #:when (not (member x a))) x)))

(define (expectation-of-null? e)
  (match e
    [(struct expc (scs compound data literals))
     (and (null? scs)
          (null? compound)
          (null? literals)
          (and (pair? data) (null? (cdr data)))
          (equal? (car data) '()))]
    [#f #f]))

(define (expectation->string e)
  (match e
    [(struct expc (stxclasses compound data literals))
     (cond [(null? compound)
            (let ([s1 (and (pair? stxclasses) (string-of-stxclasses stxclasses))]
                  [s2 (and (pair? data) (string-of-data data))]
                  [s3 (and (pair? literals) (string-of-literals literals))])
              (join-sep (filter string? (list s1 s2 s3))
                        ";"
                        "or"))]
           [(andmap string? compound)
            (join-sep compound ";" "or")]
           [else
            #f])]))

(define (string-of-stxclasses scdyns)
  (comma-list (map string-of-stxclass scdyns)))

(define (string-of-stxclass scdyn)
  (define expected (or (scdyn-desc scdyn) (scdyn-name scdyn)))
  (if (scdyn-failure scdyn)
      (let ([inner (expectation->string (failed-expectation (scdyn-failure scdyn)))])
        (or inner (format "~a" expected)))
      (format "~a" expected)))

(define (string-of-literals literals0)
  (define literals
    (sort (map syntax-e literals0) 
          string<? 
          #:key symbol->string
          #:cache-keys? #t))
  (case (length literals)
    [(1) (format "the literal identifier ~s" (car literals))]
    [else (format "one of the following literal identifiers: ~a"
                  (comma-list (map ->string literals)))]))

(define (string-of-data data)
  (case (length data)
    [(1) (format "the literal ~s" (car data))]
    [else (format "one of the following literals: ~a"
                  (comma-list (map ->string data)))]))

(define (->string x) (format "~s" x))

(define string-of-pairs?
  "structured syntax")

(define (comma-list items)
  (join-sep items "," "or"))

(define (join-sep items sep0 ult0)
  (define sep (string-append sep0 " "))
  (define ult (string-append ult0 " "))
  (define (loop items)
    (cond [(null? items)
           null]
          [(null? (cdr items))
           (list sep ult (car items))]
          [else
           (list* sep (car items) (loop (cdr items)))]))
  (case (length items)
    [(2) (format "~a ~a~a" (car items) ult (cadr items))]
    [else (let ([strings (list* (car items) (loop (cdr items)))])
            (apply string-append strings))]))


;; Attributes

(begin-for-syntax
 (define-struct attribute-mapping (var)
   #:omit-define-syntaxes
   #:property prop:procedure
   (lambda (self stx)
     #`(#%expression #,(attribute-mapping-var self)))))

(define-syntax (let-attributes stx)
  (syntax-case stx ()
    [(let-attributes ([attr depth value] ...) . body)
     (with-syntax ([(vtmp ...) (generate-temporaries #'(attr ...))]
                   [(stmp ...) (generate-temporaries #'(attr ...))])
       #'(letrec-syntaxes+values
             ([(stmp) (make-attribute-mapping (quote-syntax vtmp))]
              ...)
             ([(vtmp) value] ...)
           (letrec-syntaxes+values
               ([(attr) (make-syntax-mapping 'depth (quote-syntax stmp))] ...)
               ()
             . body)))]))

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
