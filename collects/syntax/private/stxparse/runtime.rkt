#lang racket/base
(require racket/contract/base
         racket/stxparam
         racket/list
         unstable/struct
         "minimatch.ss"
         (for-syntax racket/base
                     syntax/stx
                     racket/private/sc
                     "rep-data.ss"
                     "rep-attrs.ss"
                     "../util.ss"))

(provide pattern
         ~var
         ~datum
         ~literal
         ~and
         ~or
         ~not
         ~seq
         ~between
         ~once
         ~optional
         ~rest
         ~describe
         ~!
         ~bind
         ~fail
         ~early-fail
         ~parse
         ...+

         current-expression
         current-macro-name

         this-syntax

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
(define-keyword ~datum)
(define-keyword ~literal)
(define-keyword ~and)
(define-keyword ~or)
(define-keyword ~not)
(define-keyword ~seq)
(define-keyword ~between)
(define-keyword ~once)
(define-keyword ~optional)
(define-keyword ~rest)
(define-keyword ~describe)
(define-keyword ~!)
(define-keyword ~bind)
(define-keyword ~fail)
(define-keyword ~early-fail)
(define-keyword ~parse)
(define-keyword ...+)

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

(provide (struct-out dfc:empty)
         (struct-out dfc:car)
         (struct-out dfc:cdr)
         (struct-out dfc:pre)
         (struct-out dfc:post)
         dfc-empty
         dfc-add-car
         dfc-add-cdr
         dfc-add-pre
         dfc-add-post
         dfc-add-unbox
         dfc-add-unvector
         dfc-add-unpstruct

         dfc->index
         dfc->stx
         dfc-difference
         dfc-append

         invert-dfc
         compare-idfcs
         idfc>?
         idfc=?)

#|
A Dynamic Frontier Context (DFC) is one of
  - (make-dfc:empty stx)
  - (make-dfc:car DFC stx)
  - (make-dfc:cdr DFC positive-integer)
  - (make-dfc:pre DFC stx)
  - (make-dfc:post DFC stx)
|#

(define-struct dfc:empty (stx) #:prefab)
(define-struct dfc:car (parent stx) #:prefab)
(define-struct dfc:cdr (parent n) #:prefab)
(define-struct dfc:pre (parent stx) #:prefab)
(define-struct dfc:post (parent stx) #:prefab)

(define (dfc-empty x) (make dfc:empty x))
(define (dfc-add-car parent stx)
  (make dfc:car parent stx))
(define (dfc-add-cdr parent _)
  (match parent
    [(make dfc:cdr uberparent n)
     (make dfc:cdr uberparent (add1 n))]
    [_ (make dfc:cdr parent 1)]))
(define (dfc-add-pre parent stx)
  (make dfc:pre parent stx))
(define (dfc-add-post parent stx)
  (make dfc:post parent stx))

(define (dfc-add-unbox parent stx)
  (dfc-add-car parent stx))
(define (dfc-add-unvector parent stx)
  (dfc-add-car parent stx))
(define (dfc-add-unpstruct parent stx)
  (dfc-add-car parent stx))

(define (dfc->index dfc)
  (match dfc
    [(make dfc:cdr parent n) n]
    [_ 0]))

(define (dfc->stx dfc)
  (match dfc
    [(make dfc:empty stx) stx]
    [(make dfc:car parent stx) stx]
    [(make dfc:cdr parent n) (dfc->stx parent)]
    [(make dfc:pre parent stx) stx]
    [(make dfc:post parent stx) stx]))

;; dfc-difference : DFC DFC -> nat
;; Returns N s.t. B = (dfc-add-cdr^N A)
(define (dfc-difference a b)
  (define (whoops)
    (error 'dfc-difference "~e is not an extension of ~e"
           (frontier->sexpr b) (frontier->sexpr a)))
  (match (list a b)
    [(list (make dfc:cdr pa na) (make dfc:cdr pb nb))
     (unless (equal? pa pb) (whoops))
     (- nb na)]
    [(list pa (make dfc:cdr pb nb))
     (unless (equal? pa pb) (whoops))
     nb]
    [_
     (unless (equal? a b) (whoops))
     0]))

;; dfc-append : DFC DFC -> DFC
;; puts A at the base, B on top
(define (dfc-append a b)
  (match b
    [(make dfc:empty stx) a]
    [(make dfc:car pb stx) (make dfc:car (dfc-append a pb) stx)]
    [(make dfc:cdr (make dfc:empty _) nb)
     ;; Special case to merge "consecutive" cdr frames
     (match a
       [(make dfc:cdr pa na) (make dfc:cdr pa (+ na nb))]
       [_ (make dfc:cdr a nb)])]
    [(make dfc:cdr pb nb) (make dfc:cdr (dfc-append a pb) nb)]
    [(make dfc:pre pb stx) (make dfc:pre (dfc-append a pb) stx)]
    [(make dfc:post pb stx) (make dfc:post (dfc-append a pb) stx)]))


;; An Inverted DFC (IDFC) is a DFC inverted for easy comparison.

(define (invert-dfc dfc)
  (define (invert dfc acc)
    (match dfc
      [(make dfc:empty _) acc]
      [(make dfc:car parent stx)
       (invert parent (make dfc:car acc stx))]
      [(make dfc:cdr parent n)
       (invert parent (make dfc:cdr acc n))]
      [(make dfc:pre parent stx)
       (invert parent (make dfc:pre acc stx))]
      [(make dfc:post parent stx)
       (invert parent (make dfc:post acc stx))]))
  (invert dfc (dfc-empty 'dummy)))

;; compare-idfcs : IDFC IDFC -> (one-of '< '= '>)
;; Note A>B means A is "further along" than B.
;; Lexicographic generalization of PRE < CAR < CDR < POST
(define (compare-idfcs a b)
  (match (list a b)
    ;; Same constructors
    [(list (make dfc:empty _) (make dfc:empty _)) '=]
    [(list (make dfc:car pa _) (make dfc:car pb _))
     (compare-idfcs pa pb)]
    [(list (make dfc:cdr pa na) (make dfc:cdr pb nb))
     (cond [(< na nb) (compare-idfcs pa (make dfc:cdr pb (- nb na)))]
           [(> na nb) (compare-idfcs (make-dfc:cdr pa (- na nb)) pb)]
           [(= na nb) (compare-idfcs pa pb)])]
    [(list (make dfc:pre pa _) (make dfc:pre pb _))
     ;; FIXME: possibly just '= here, treat all sides as equiv
     (compare-idfcs pa pb)]
    [(list (make dfc:post pa _) (make dfc:post pb _))
     ;; FIXME: possibly just '= here, treat all sides as equiv
     (compare-idfcs pa pb)]
    ;; Different constructors
    [(list (make dfc:empty _) _) '<]
    [(list _ (make dfc:empty _)) '>]
    [(list (make dfc:pre _ _) _) '<]
    [(list _ (make dfc:pre _ _)) '>]
    [(list (make dfc:car _ _) _) '<]
    [(list _ (make dfc:car _ _)) '>]
    [(list (make dfc:cdr _ _) _) '<]
    [(list _ (make dfc:cdr _ _)) '>]))

(define (idfc>? a b)
  (eq? (compare-idfcs a b) '>))

(define (idfc=? a b)
  (eq? (compare-idfcs a b) '=))

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
;;   (make-failure stx DFC expectation/c)
;;   (make-join-failures Failure Failure)

(define ok? list?)

(define-struct failure (stx frontier expectation) #:prefab)
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
                          (fail (make join-failures f1 f2)))])
                   (try* rest-attempts combining-fail)))])
          (first-attempt next-fail)))))


;; == Expectations

;; FIXME: add phase to expect:literal

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
  (make expect:disj a b))

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


;; ----

;; debugging

(provide failure->sexpr
         one-failure->sexpr
         frontier->sexpr
         expectation->sexpr)

(define (failure->sexpr f)
  (define fs
    (let loop ([f f])
      (match f
        [(make join-failures f1 f2)
         (append (loop f1) (loop f2))]
        [_ (list f)])))
  (case (length fs)
    ((1) (one-failure->sexpr f))
    (else `(union ,@(map one-failure->sexpr fs)))))

(define (one-failure->sexpr f)
  (match f
    [(make failure x frontier expectation)
     `(failure ,(frontier->sexpr frontier)
               #:term ,(syntax->datum x)
               #:expected ,(expectation->sexpr expectation))]))

(define (frontier->sexpr dfc)
  (match (invert-dfc dfc)
    [(make dfc:empty _) '()]
    [(make dfc:car p _) (cons 'car (frontier->sexpr p))]
    [(make dfc:cdr p n) (cons n (frontier->sexpr p))]
    [(make dfc:pre p _) (cons 'pre (frontier->sexpr p))]
    [(make dfc:post p _) (cons 'post (frontier->sexpr p))]))

(define (expectation->sexpr expectation)
  (match expectation
    [(make expect:thing thing '#t chained)
     (make expect:thing thing #t (failure->sexpr chained))]
    [_ expectation]))


;;

(provide (struct-out parser))

(define-struct parser (proc errors)
  #:property prop:procedure (struct-field-index proc))

;;

(provide curried-stxclass-procedures)

(define-syntax (curried-stxclass-procedures stx)
  (syntax-case stx ()
    [(cp class (arg ...))
     (let* ([args (syntax->list #'(arg ...))]
            [sc (get-stxclass/check-arg-count #'class (length args))])
       (with-syntax ([parser (stxclass-parser-name sc)]
                     [get-description (stxclass-description sc)]
                     [(extra ...)
                      (if (stxclass-commit? sc)
                          #'()
                          #'(k))])
         #'(values (lambda (x extra ...) (parser x extra ... arg ...))
                   (lambda () (get-description arg ...)))))]))

;; 

(provide phase+
         check-literal
         free-identifier=?/phases)

(define (phase+ a b)
  (and (number? a) (number? b) (+ a b)))

;; check-literal : id phase-level stx -> void
;; FIXME: change to normal 'error', if src gets stripped away
(define (check-literal id phase ctx)
  (unless (identifier-binding id phase)
    (raise-syntax-error #f "literal identifier has no binding" ctx id)))

;; free-identifier=?/phases : id phase-level id phase-level -> boolean
;; Determines whether x has the same binding at phase-level phase-x
;; that y has at phase-level y.
;; At least one of the identifiers MUST have a binding (module or lexical)
(define (free-identifier=?/phases x phase-x y phase-y)
  (let ([base-phase (syntax-local-phase-level)])
    (let ([bx (identifier-binding x (phase+ base-phase phase-x))]
          [by (identifier-binding y (phase+ base-phase phase-y))])
      (cond [(and (list? bx) (list? by))
             (let ([modx (module-path-index-resolve (first bx))]
                   [namex (second bx)]
                   [phasex (fifth bx)]
                   [mody (module-path-index-resolve (first by))]
                   [namey (second by)]
                   [phasey (fifth by)])
               (and (eq? modx mody) ;; resolved-module-paths are interned
                    (eq? namex namey)
                    (equal? phasex phasey)))]
            [else
             ;; One must be lexical (can't be #f, since one must be bound)
             ;; lexically-bound names bound in only one phase; just compare
             (free-identifier=? x y)]))))

;; ----

(provide begin-for-syntax/once)

;; (begin-for-syntax/once expr/phase1 ...)
;; evaluates in pass 2 of module/intdefs expansion
(define-syntax (begin-for-syntax/once stx)
  (syntax-case stx ()
    [(bfs/o e ...)
     (cond [(list? (syntax-local-context))
            #`(define-values ()
                (begin (begin-for-syntax/once e ...)
                       (values)))]
           [else
            #'(let-syntax ([m (lambda _ (begin e ...) #'(void))])
                (m))])]))
