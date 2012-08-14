#lang racket/base
(require (for-syntax racket/base
                     racket/set
                     racket/syntax
                     syntax/parse/private/minimatch
                     racket/private/sc)
         syntax/parse/private/residual
         "private/substitute.rkt")
(provide template
         define-template-metafunction
         ??
         ?@)

#|
To do:
- improve error messages
|#

#|
A Template (T) is one of:
  - pvar
  - const (including () and non-pvar identifiers)
  - (metafunction . T)
  - (H . T)
  - (H ... . T), (H ... ... . T), etc
  - (?? T T)
  - #(T*)
  - #s(prefab-struct-key T*)

A HeadTemplate (H) is one of:
  - T
  - (?? H)
  - (?? H H)
  - (?@ . T)
|#

(define-syntax (template stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      [(template t)
       #'(template t #:properties (paren-shape))]
      [(template t #:properties (prop ...))
       (andmap identifier? (syntax->list #'(prop ...)))
       (let-values ([(guide deps props-guide)
                     (parameterize ((retain-props (syntax->datum #'(prop ...))))
                       (parse-template #'t))])
         (let ([vars
                (for/list ([dep (in-vector deps)])
                  (cond [(pvar? dep)
                         (let* ([sm (pvar-sm dep)]
                                [valvar (syntax-mapping-valvar sm)]
                                [attr (syntax-local-value valvar (lambda () #f))])
                           (cond [(attribute-mapping? attr)
                                  (attribute-mapping-var attr)]
                                 [else valvar]))]
                        [(template-metafunction? dep)
                         (template-metafunction-var dep)]
                        [else
                         (error 'template
                                "internal error: bad environment entry: ~e"
                                dep)]))])
           (syntax-arm
            (cond [(equal? guide '1) ;; was (template pvar)
                   (with-syntax ([var (car vars)])
                     #'(if (syntax? var)
                           var
                           (error/not-stx (quote-syntax t) var)))]
                  [(equal? guide '_) ;; constant
                   (cond [(equal? props-guide '_) ;; no props
                          #`(quote-syntax t)]
                         [else
                          (with-syntax ([props-guide props-guide])
                            #`(substitute (quote-syntax t) 'props-guide '_ #f))])]
                  [else
                   (with-syntax ([guide guide]
                                 [props-guide props-guide]
                                 [vars-vector
                                  (if (pair? vars)
                                      #`(vector . #,vars)
                                      #''#())])
                     #'(substitute (quote-syntax t)
                                   'props-guide
                                   'guide
                                   vars-vector))]))))])))

;; substitute-table : hash[stx => translated-template]
;; Cache for closure-compiled templates. Key is just syntax of
;; template, since eq? templates must have equal? guides.
(define substitute-table (make-weak-hasheq))

;; props-syntax-table : hash[stx => stx]
(define props-syntax-table (make-weak-hasheq))

(define (substitute stx props-guide g main-env)
  (let* ([stx (if (eq? props-guide '_)
                  stx
                  (or (hash-ref props-syntax-table stx #f)
                      (let* ([pf (translate stx props-guide 0)]
                             [pstx (pf '#() #f)])
                        (hash-set! props-syntax-table stx pstx)
                        pstx)))]
         [f (or (hash-ref substitute-table stx #f)
                (let ([f (translate stx g (vector-length main-env))])
                  (hash-set! substitute-table stx f)
                  f))])
    (f main-env #f)))

;; ----

(define-syntaxes (?? ?@)
  (let ([tx (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx))])
    (values tx tx)))

;; ============================================================

#|
See private/substitute for definition of Guide (G) and HeadGuide (HG).

A env-entry is one of
  - (pvar syntax-mapping depth-delta)
  - template-metafunction

The depth-delta associated with a depth>0 pattern variable is the difference
between the pattern variable's depth and the depth at which it is used. (For
depth 0 pvars, it's #f.) For example, in

  (with-syntax ([x #'0]
                [(y ...) #'(1 2)]
                [((z ...) ...) #'((a b) (c d))])
    (template (((x y) ...) ...)))

the depth-delta for x is #f, the depth-delta for y is 1, and the depth-delta for
z is 0. Coincidentally, the depth-delta is the same as the depth of the ellipsis
form at which the variable should be moved to the loop-env. That is, the
template above should be interpreted as roughly similar to

  (let ([x (pvar-value-of x)]
        [y (pvar-value-of y)]
        [z (pvar-value-of z)])
    (for ([Lz (in-list z)]) ;; depth 0
      (for ([Ly (in-list y)] ;; depth 1
            [Lz (in-list Lz)])
        (___ x Ly Lz ___))))

A Pre-Guide is like a Guide but with env-entry and (setof env-entry)
instead of integers and integer vectors.
|#

(begin-for-syntax
 (struct pvar (sm dd) #:prefab))

;; ============================================================

(define-syntax (define-template-metafunction stx)
  (syntax-case stx ()
    [(dsm (id arg ...) . body)
     #'(dsm id (lambda (arg ...) . body))]
    [(dsm id expr)
     (identifier? #'id)
     (with-syntax ([(internal-id) (generate-temporaries #'(id))])
       #'(begin (define internal-id expr)
                (define-syntax id
                  (template-metafunction (quote-syntax internal-id)))))]))

(begin-for-syntax
 (struct template-metafunction (var)))

;; ============================================================

(begin-for-syntax

 ;; parse-template : stx -> (values guide (vectorof env-entry) guide)
 (define (parse-template t)
   (let-values ([(drivers pre-guide props-guide) (parse-t t 0 #f)])
     (define main-env (set->env drivers (hash)))
     (define guide (guide-resolve-env pre-guide main-env))
     (values guide
             (index-hash->vector main-env)
             props-guide)))

 ;; set->env : (setof env-entry) -> hash[env-entry => nat]
 (define (set->env drivers init-env)
   (for/fold ([env init-env])
       ([pvar (in-set drivers)]
        [n (in-naturals (+ 1 (hash-count init-env)))])
     (hash-set env pvar n)))

 ;; guide-resolve-env : pre-guide hash[env-entry => nat] -> guide
 (define (guide-resolve-env g0 main-env)
   (define (loop g loop-env)
     (define (get-index x)
       (let ([loop-index (hash-ref loop-env x #f)])
         (if loop-index
             (- loop-index)
             (hash-ref main-env x))))
     (match g
       ['_ '_]
       [(cons g1 g2) (cons (loop g1 loop-env) (loop g2 loop-env))]
       [(? pvar? pvar) (get-index pvar)]
       [(vector 'dots head new-hdrivers/level nesting '#f tail)
        (let-values ([(sub-loop-env r-uptos)
                      (for/fold ([env (hash)] [r-uptos null])
                          ([new-hdrivers (in-list new-hdrivers/level)])
                        (let ([new-env (set->env new-hdrivers env)])
                          (values new-env (cons (hash-count new-env) r-uptos))))])
          (let ([sub-loop-vector (index-hash->vector sub-loop-env get-index)])
            (vector 'dots
                    (loop head sub-loop-env)
                    sub-loop-vector
                    nesting
                    (reverse r-uptos)
                    (loop tail loop-env))))]
       [(vector 'app head tail)
        (vector 'app (loop head loop-env) (loop tail loop-env))]
       [(vector 'escaped g1)
        (vector 'escaped (loop g1 loop-env))]
       [(vector 'orelse g1 drivers1 g2)
        (vector 'orelse
                (loop g1 loop-env)
                (for/vector ([ee (in-set drivers1)])
                  (get-index ee))
                (loop g2 loop-env))]
       [(vector 'orelse-h g1 drivers1 g2)
        (vector 'orelse-h
                (loop g1 loop-env)
                (for/vector ([ee (in-set drivers1)])
                  (get-index ee))
                (loop g2 loop-env))]
       [(vector 'metafun mf g1)
        (vector 'metafun
                (get-index mf)
                (loop g1 loop-env))]
       [(vector 'vector g1)
        (vector 'vector (loop g1 loop-env))]
       [(vector 'struct g1)
        (vector 'struct (loop g1 loop-env))]
       [(vector 'box g1)
        (vector 'box (loop (unbox g) loop-env))]
       [(vector 'copy-props g1 keys)
        (vector 'copy-props (loop g1 loop-env) keys)]
       [(vector 'set-props g1 props-alist)
        (vector 'set-props (loop g1 loop-env) props-alist)]
       [(vector 'app-opt g1 drivers1)
        (vector 'app-opt
                (loop g1 loop-env)
                (for/vector ([ee (in-set drivers1)])
                  (get-index ee)))]
       [(vector 'splice g1)
        (vector 'splice (loop g1 loop-env))]
       [else (error 'template "internal error: bad pre-guide: ~e" g)]))
   (loop g0 '#hash()))

 ;; ----------------------------------------

 (define retain-props (make-parameter '(paren-shape)))

 (define (wrap-props stx env-set pre-guide props-guide)
   (let ([prop-entries
          (if (syntax? stx)
              (for/fold ([entries null]) ([prop (in-list (retain-props))])
                (let ([v (syntax-property stx prop)])
                  (if (and v (quotable? v))
                      (cons (cons prop v) entries)
                      entries)))
              null)])
     (if (pair? prop-entries)
         (values env-set
                 (cond [(eq? pre-guide '_)
                        ;; No need to copy props; already on constant.
                        '_]
                       [else (vector 'copy-props pre-guide (map car prop-entries))])
                 (vector 'set-props props-guide prop-entries))
         (values env-set pre-guide props-guide))))

 (define (quotable? v)
   (or (null? v)
       (string? v)
       (bytes? v)
       (number? v)
       (boolean? v)
       (char? v)
       (keyword? v)
       (regexp? v)
       (and (box? v) (quotable? (unbox v)))
       (and (symbol? v) (symbol-interned? v))
       (and (pair? v) (quotable? (car v)) (quotable? (cdr v)))
       (and (vector? v) (andmap quotable? (vector->list v)))
       (and (prefab-struct-key v) (andmap quotable? (struct->vector v)))))

 (define (cons-guide g1 g2)
   (if (and (eq? g1 '_) (eq? g2 '_)) '_ (cons g1 g2)))

 (define (list-guide . gs)
   (foldr cons-guide '_ gs))

 ;; parse-t : stx nat boolean -> (values (setof env-entry) pre-guide)
 (define (parse-t t depth esc?)
   (syntax-case t (?? ?@)
     [id
      (identifier? #'id)
      (cond [(and (not esc?)
                  (or (free-identifier=? #'id (quote-syntax ...))
                      (free-identifier=? #'id (quote-syntax ??))
                      (free-identifier=? #'id (quote-syntax ?@))))
             (wrong-syntax #'id "illegal use")]
            [else
             (let ([pvar (lookup #'id depth)])
               (cond [(pvar? pvar)
                      (values (set pvar) pvar '_)]
                     [(template-metafunction? pvar)
                      (wrong-syntax t "illegal use of syntax metafunction")]
                     [else
                      (wrap-props #'id (set) '_ '_)]))])]
     [(mf . template)
      (and (not esc?)
           (identifier? #'mf)
           (template-metafunction? (lookup #'mf #f)))
      (let-values ([(mf) (lookup #'mf #f)]
                   [(drivers guide props-guide) (parse-t #'template depth esc?)])
        (values (set-union (set mf) drivers)
                (vector 'metafun mf guide)
                (cons-guide '_ props-guide)))]
     [(DOTS template)
      (and (not esc?)
           (identifier? #'DOTS) (free-identifier=? #'DOTS (quote-syntax ...)))
      (let-values ([(drivers guide props-guide) (parse-t #'template depth #t)])
        (values drivers (vector 'escaped guide)
                (list-guide '_ props-guide)))]
     [(?? t1 t2)
      (not esc?)
      (let-values ([(drivers1 guide1 props-guide1) (parse-t #'t1 depth esc?)]
                   [(drivers2 guide2 props-guide2) (parse-t #'t2 depth esc?)])
        (values (set-union drivers1 drivers2)
                (vector 'orelse guide1 (set-filter drivers1 pvar?) guide2)
                (list-guide '_ props-guide1 props-guide2)))]
     [(head DOTS . tail)
      (and (not esc?)
           (identifier? #'DOTS) (free-identifier=? #'DOTS (quote-syntax ...)))
      (let-values ([(nesting tail)
                    (let loop ([nesting 1] [tail #'tail])
                      (syntax-case tail ()
                        [(DOTS . tail)
                         (and (identifier? #'DOTS) (free-identifier=? #'DOTS (quote-syntax ...)))
                         (loop (add1 nesting) #'tail)]
                        [else (values nesting tail)]))])
        (let-values ([(hdrivers _hsplice? hguide hprops-guide)
                      (parse-h #'head (+ depth nesting) esc?)]
                     [(tdrivers tguide tprops-guide)
                      (parse-t tail depth esc?)])
          (unless (positive? (set-count hdrivers))
            (wrong-syntax #'head "no pattern variables in term before ellipsis"))
          (wrap-props t
                      (set-union hdrivers tdrivers)
                      ;; pre-guide hdrivers is (listof (setof pvar))
                      ;; set of pvars new to each level
                      (let* ([hdrivers/level
                              (for/list ([i (in-range nesting)])
                                (set-filter hdrivers (pvar/dd<=? (+ depth i))))]
                             [new-hdrivers/level
                              (let loop ([raw hdrivers/level] [last (set)])
                                (cond [(null? raw) null]
                                      [else
                                       (cons (set-subtract (car raw) last)
                                             (loop (cdr raw) (car raw)))]))])
                        (vector 'dots hguide new-hdrivers/level nesting #f tguide))
                      (cons-guide hprops-guide (cons-guide '_ tprops-guide)))))]
     [(head . tail)
      (let-values ([(hdrivers hsplice? hguide hprops-guide)
                    (parse-h #'head depth esc?)]
                   [(tdrivers tguide tprops-guide)
                    (parse-t #'tail depth esc?)])
        (wrap-props t
                    (set-union hdrivers tdrivers)
                    (cond [(and (eq? hguide '_) (eq? tguide '_)) '_]
                          [hsplice? (vector 'app hguide tguide)]
                          [else (cons hguide tguide)])
                    (cons-guide hprops-guide tprops-guide)))]
     [vec
      (vector? (syntax-e #'vec))
      (let-values ([(drivers guide props-guide)
                    (parse-t (vector->list (syntax-e #'vec)) depth esc?)])
        (wrap-props t drivers
                    (if (eq? guide '_) '_ (vector 'vector guide))
                    (if (eq? props-guide '_) '_ (vector 'vector props-guide))))]
     [pstruct
      (prefab-struct-key (syntax-e #'pstruct))
      (let-values ([(drivers guide props-guide)
                    (parse-t (cdr (vector->list (struct->vector (syntax-e #'pstruct)))) depth esc?)])
        (wrap-props t drivers
                    (if (eq? guide '_) '_ (vector 'struct guide))
                    (if (eq? props-guide '_) '_ (vector 'struct props-guide))))]
     [#&template
      (let-values ([(drivers guide props-guide)
                    (parse-t #'template depth esc?)])
        (wrap-props t drivers
                    (if (eq? guide '_) '_ (vector 'box guide))
                    (if (eq? props-guide '_) '_ (vector 'box props-guide))))]
     [const
      (wrap-props t (set) '_ '_)]))

 ;; parse-h : stx nat boolean -> (values (setof env-entry) boolean pre-head-guide)
 (define (parse-h h depth esc?)
   (syntax-case h (?? ?@)
     [(?? t)
      (not esc?)
      (let-values ([(drivers splice? guide props-guide)
                    (parse-h #'t depth esc?)])
        (values drivers #t
                (vector 'app-opt guide (set-filter drivers pvar?))
                (list-guide '_ props-guide)))]
     [(?? t1 t2)
      (not esc?)
      (let-values ([(drivers1 splice?1 guide1 props-guide1) (parse-h #'t1 depth esc?)]
                   [(drivers2 splice?2 guide2 props-guide2) (parse-h #'t2 depth esc?)])
        (values (set-union drivers1 drivers2)
                (or splice?1 splice?2)
                (vector (if (or splice?1 splice?2) 'orelse-h 'orelse)
                        guide1 (set-filter drivers1 pvar?) guide2)
                (list-guide '_ props-guide1 props-guide2)))]
     [(?@ . t)
      (not esc?)
      (let-values ([(drivers guide props-guide) (parse-t #'t depth esc?)])
        (values drivers #t (vector 'splice guide) (cons-guide '_ props-guide)))]
     [t
      (let-values ([(drivers guide props-guide) (parse-t #'t depth esc?)])
        (values drivers #f guide props-guide))]))

 ;; Note: always creates equal?-based set.
 (define (set-filter s pred?)
   (for/set ([el (in-set s)] #:when (pred? el)) el))

 (define (lookup id depth)
   (let ([v (syntax-local-value id (lambda () #f))])
     (cond [(syntax-pattern-variable? v)
            (let ([pvar-depth (syntax-mapping-depth v)])
              (cond [(not depth) ;; not looking for pvars, only for metafuns
                     #f]
                    [(zero? pvar-depth)
                     (pvar v #f)]
                    [(>= depth pvar-depth)
                     (pvar v (- depth pvar-depth))]
                    [else
                     (wrong-syntax id
                                   (string-append "pattern variable used at wrong ellipsis depth "
                                                  "(expected at least ~s, used at ~s)")
                                   pvar-depth depth)]))]
           [(template-metafunction? v)
            v]
           [else
            ;; id is a literal; check that for all x s.t. id = x.y, x is not a pattern variable
            (for ([pfx (in-list (dotted-prefixes id))])
              (when (syntax-pattern-variable? (syntax-local-value pfx (lambda () #f)))
                (wrong-syntax id "undefined nested attribute of attribute `~a'" (syntax-e pfx))))
            #f])))

 (define (dotted-prefixes id)
   (let* ([id-string (symbol->string (syntax-e id))]
          [dot-locations (map car (regexp-match-positions* #rx"\\.[^.]" id-string))])
     (for/list ([loc (in-list dot-locations)])
       (datum->syntax id (string->symbol (substring id-string 0 loc))))))

 (define (index-hash->vector hash [f values])
   (let ([vec (make-vector (hash-count hash))])
     (for ([(value index) (in-hash hash)])
       (vector-set! vec (sub1 index) (f value)))
     vec))

 (define ((pvar/dd<=? expected-dd) x)
   (match x
     [(pvar sm dd) (and dd (<= dd expected-dd))]
     [_ #f]))
 )
