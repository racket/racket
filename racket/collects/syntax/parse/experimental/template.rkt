#lang racket/base
(require (for-syntax racket/base
                     "dset.rkt"
                     racket/syntax
                     syntax/parse/private/minimatch
                     racket/private/stx ;; syntax/stx
                     racket/private/sc)
         syntax/parse/private/residual
         "private/substitute.rkt")
(provide template
         template/loc
         quasitemplate
         quasitemplate/loc
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
  * (unquote expr)

A HeadTemplate (H) is one of:
  - T
  - (?? H)
  - (?? H H)
  - (?@ . T)
  * (unquote-splicing expr)
|#

(begin-for-syntax
 (define (do-template ctx tstx quasi? loc-id)
   (parameterize ((current-syntax-context ctx)
                  (quasi (and quasi? (box null))))
     (let*-values ([(guide deps props-guide) (parse-template tstx loc-id)]
                   [(vars)
                    (for/list ([dep (in-vector deps)])
                      (cond [(pvar? dep) (pvar-var dep)]
                            [(template-metafunction? dep)
                             (template-metafunction-var dep)]
                            [else
                             (error 'template
                                    "internal error: bad environment entry: ~e"
                                    dep)]))])
       (with-syntax ([t tstx])
         (syntax-arm
          (cond [(equal? guide '1)
                 ;; was (template pvar), implies props-guide = '_
                 (car vars)]
                [(and (equal? guide '_) (equal? props-guide '_))
                 #'(quote-syntax t)]
                [else
                 (with-syntax ([guide guide]
                               [props-guide props-guide]
                               [vars-vector
                                (if (pair? vars)
                                    #`(vector . #,vars)
                                    #''#())]
                               [((un-var . un-form) ...)
                                (if quasi? (reverse (unbox (quasi))) null)])
                   #'(let ([un-var (handle-unsyntax un-form)] ...)
                       (substitute (quote-syntax t)
                                   'props-guide
                                   'guide
                                   vars-vector)))])))))))

(define-syntax (template stx)
  (syntax-case stx ()
    [(template t)
     (do-template stx #'t #f #f)]
    [(template t #:properties (prop ...))
     (andmap identifier? (syntax->list #'(prop ...)))
     (parameterize ((props-to-serialize (syntax->datum #'(prop ...)))
                    (props-to-transfer (syntax->datum #'(prop ...))))
       (do-template stx #'t #f #f))]))

(define-syntax (quasitemplate stx)
  (syntax-case stx ()
    [(quasitemplate t)
     (do-template stx #'t #t #f)]))

(define-syntaxes (template/loc quasitemplate/loc)
  ;; FIXME: better to replace unsyntax form, shrink template syntax constant
  (let ([make-tx
         (lambda (quasi?)
           (lambda (stx)
             (syntax-case stx ()
               [(?/loc loc-expr t)
                (syntax-arm
                 (with-syntax ([main-expr (do-template stx #'t quasi? #'loc-stx)])
                   #'(let ([loc-stx (handle-loc '?/loc loc-expr)])
                       main-expr)))])))])
    (values (make-tx #f) (make-tx #t))))

(define (handle-loc who x)
  (if (syntax? x)
      x
      (raise-argument-error who "syntax?" x)))

;; FIXME: what lexical context should result of expr get if not syntax?
(define-syntax handle-unsyntax
  (syntax-rules (unsyntax unsyntax-splicing)
    [(handle-syntax (unsyntax expr)) expr]
    [(handle-syntax (unsyntax-splicing expr)) expr]))

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
  - (pvar syntax-mapping attribute-mapping/#f depth-delta)
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
 (struct pvar (sm attr dd) #:prefab))

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

 ;; props-to-serialize determines what properties are saved even when
 ;; code is compiled.  (Unwritable values are dropped.)
 ;; props-to-transfer determines what properties are transferred from
 ;; template to stx constructed.
 ;; If a property is in props-to-transfer but not props-to-serialize,
 ;; compiling the module may have caused the property to disappear.
 ;; If a property is in props-to-serialize but not props-to-transfer,
 ;; it will show up only in constant subtrees.
 ;; The behavior of 'syntax' is serialize '(), transfer '(paren-shape).

 ;; props-to-serialize : (parameterof (listof symbol))
 (define props-to-serialize (make-parameter '()))

 ;; props-to-transfer : (parameterof (listof symbol))
 (define props-to-transfer (make-parameter '(paren-shape)))

 ;; quasi : (parameterof (or/c #f (list^n (boxof QuasiPairs))))
 ;; each list wrapper represents nested quasi wrapping
 ;; QuasiPairs = (listof (cons/c identifier syntax))
 (define quasi (make-parameter #f))

 ;; parse-template : stx id/#f -> (values guide (vectorof env-entry) guide)
 (define (parse-template t loc-id)
   (let*-values ([(drivers pre-guide props-guide) (parse-t t 0 #f)]
                 [(drivers pre-guide)
                  (if loc-id
                      (let* ([loc-sm (make-syntax-mapping 0 loc-id)]
                             [loc-pvar (pvar loc-sm #f #f)])
                        (values (dset-add drivers loc-pvar)
                                (relocate-guide pre-guide loc-pvar)))
                      (values drivers pre-guide))])
     (let* ([main-env (dset->env drivers (hash))]
            [guide (guide-resolve-env pre-guide main-env)])
       (values guide
               (index-hash->vector main-env)
               props-guide))))

 ;; dset->env : (dsetof env-entry) -> hash[env-entry => nat]
 (define (dset->env drivers init-env)
   (for/fold ([env init-env])
       ([pvar (in-list (dset->list drivers))]
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
       [(cons g1 g2)
        (cons (loop g1 loop-env) (loop g2 loop-env))]
       [(? pvar? pvar)
        (if (pvar-check? pvar)
            (vector 'check (get-index pvar))
            (get-index pvar))]
       [(vector 'dots head new-hdrivers/level nesting '#f tail)
        (let-values ([(sub-loop-env r-uptos)
                      (for/fold ([env (hash)] [r-uptos null])
                          ([new-hdrivers (in-list new-hdrivers/level)])
                        (let ([new-env (dset->env new-hdrivers env)])
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
       [(vector 'orelse g1 g2)
        (vector 'orelse (loop g1 loop-env) (loop g2 loop-env))]
       [(vector 'orelse-h g1 g2)
        (vector 'orelse-h (loop g1 loop-env) (loop g2 loop-env))]
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
       [(vector 'app-opt g1)
        (vector 'app-opt (loop g1 loop-env))]
       [(vector 'splice g1)
        (vector 'splice (loop g1 loop-env))]
       [(vector 'unsyntax var)
        (vector 'unsyntax (get-index var))]
       [(vector 'unsyntax-splicing var)
        (vector 'unsyntax-splicing (get-index var))]
       [(vector 'relocate g1 var)
        (vector 'relocate (loop g1 loop-env) (get-index var))]
       [else (error 'template "internal error: bad pre-guide: ~e" g)]))
   (loop g0 '#hash()))

 ;; ----------------------------------------

 ;; relocate-gude : stx guide -> guide
 (define (relocate-guide g0 loc-pvar)
   (define (relocate g)
     (vector 'relocate g loc-pvar))
   (define (error/no-relocate)
     (wrong-syntax #f "cannot apply syntax location to template"))
   (define (loop g)
     (match g
       ['_
        (relocate g)]
       [(cons g1 g2)
        (relocate g)]
       [(? pvar? g)
        g]
       [(vector 'dots head new-hdrivers/level nesting '#f tail)
        ;; Ideally, should error. For perfect backwards compatability,
        ;; should relocate. But if there are zero iterations, that
        ;; means we'd relocate tail (which might be bad). Making
        ;; relocation depend on number of iterations would be
        ;; complicated. So just ignore.
        g]
       [(vector 'escaped g1)
        (vector 'escaped (loop g1))]
       [(vector 'vector g1)
        (relocate g)]
       [(vector 'struct g1)
        (relocate g)]
       [(vector 'box g1)
        (relocate g)]
       [(vector 'copy-props g1 keys)
        (vector 'copy-props (loop g1) keys)]
       [(vector 'unsyntax var)
        g]
       ;; ----
       [(vector 'app ghead gtail)
        (match ghead
          [(vector 'unsyntax-splicing _) g]
          [_ (error/no-relocate)])]
       ;; ----
       [(vector 'orelse g1 g2)
        (error/no-relocate)]
       [(vector 'orelse-h g1 g2)
        (error/no-relocate)]
       [(vector 'metafun mf g1)
        (error/no-relocate)]
       [(vector 'app-opt g1)
        (error/no-relocate)]
       [(vector 'splice g1)
        (error/no-relocate)]
       [(vector 'unsyntax-splicing var)
        g]
       [else (error 'template "internal error: bad guide for relocation: ~e" g0)]))
   (loop g0))

 ;; ----------------------------------------

 (define (wrap-props stx env-set pre-guide props-guide)
   (let ([saved-prop-values
          (if (syntax? stx)
              (for/fold ([entries null]) ([prop (in-list (props-to-serialize))])
                (let ([v (syntax-property stx prop)])
                  (if (and v (quotable? v))
                      (cons (cons prop v) entries)
                      entries)))
              null)]
         [copy-props
          (if (syntax? stx)
              (for/list ([prop (in-list (props-to-transfer))]
                         #:when (syntax-property stx prop))
                prop)
              null)])
     (values env-set
             (cond [(eq? pre-guide '_)
                    ;; No need to copy props; already on constant
                    '_]
                   [(pair? copy-props)
                    (vector 'copy-props pre-guide copy-props)]
                   [else pre-guide])
             (if (pair? saved-prop-values)
                 (vector 'set-props props-guide saved-prop-values)
                 props-guide))))

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

 ;; parse-t : stx nat boolean -> (values (dsetof env-entry) pre-guide props-guide)
 (define (parse-t t depth esc?)
   (syntax-case t (?? ?@ unsyntax quasitemplate)
     [id
      (identifier? #'id)
      (cond [(or (and (not esc?)
                      (or (free-identifier=? #'id (quote-syntax ...))
                          (free-identifier=? #'id (quote-syntax ??))
                          (free-identifier=? #'id (quote-syntax ?@))))
                 (and (quasi)
                      (or (free-identifier=? #'id (quote-syntax unsyntax))
                          (free-identifier=? #'id (quote-syntax unsyntax-splicing)))))
             (wrong-syntax #'id "illegal use")]
            [else
             (let ([pvar (lookup #'id depth)])
               (cond [(pvar? pvar)
                      (values (dset pvar) pvar '_)]
                     [(template-metafunction? pvar)
                      (wrong-syntax t "illegal use of syntax metafunction")]
                     [else
                      (wrap-props #'id (dset) '_ '_)]))])]
     [(mf . template)
      (and (not esc?)
           (identifier? #'mf)
           (template-metafunction? (lookup #'mf #f)))
      (let-values ([(mf) (lookup #'mf #f)]
                   [(drivers guide props-guide) (parse-t #'template depth esc?)])
        (values (dset-add drivers mf)
                (vector 'metafun mf guide)
                (cons-guide '_ props-guide)))]
     [(unsyntax t1)
      (quasi)
      (let ([qval (quasi)])
        (cond [(box? qval)
               (with-syntax ([(tmp) (generate-temporaries #'(unsyntax-expr))])
                 (set-box! qval (cons (cons #'tmp t) (unbox qval)))
                 (let* ([fake-sm (make-syntax-mapping 0 #'tmp)]
                        [fake-pvar (pvar fake-sm #f #f)])
                   (values (dset fake-pvar) (vector 'unsyntax fake-pvar) '_)))]
              [else
               (parameterize ((quasi (car qval)))
                 (let-values ([(drivers guide props-guide) (parse-t #'t1 depth esc?)])
                   (wrap-props t
                               drivers
                               (list-guide '_ guide)
                               (list-guide '_ props-guide))))]))]
     [(quasitemplate t1)
      ;; quasitemplate escapes inner unsyntaxes
      (quasi)
      (parameterize ((quasi (list (quasi))))
        (let-values ([(drivers guide props-guide) (parse-t #'t1 depth esc?)])
          (wrap-props t
                      drivers
                      (list-guide '_ guide)
                      (list-guide '_ props-guide))))]
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
        (values (dset-union drivers1 drivers2)
                (vector 'orelse guide1 guide2)
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
          (when (dset-empty? hdrivers)
            (wrong-syntax #'head "no pattern variables before ellipsis in template"))
          (when (dset-empty? (dset-filter hdrivers (pvar/dd<=? depth)))
            ;; FIXME: improve error message?
            (let ([bad-dots
                   ;; select the nestingth (last) ellipsis as the bad one
                   (stx-car (stx-drop nesting t))])
              (wrong-syntax bad-dots "too many ellipses in template")))
          (wrap-props t
                      (dset-union hdrivers tdrivers)
                      ;; pre-guide hdrivers is (listof (setof pvar))
                      ;; set of pvars new to each level
                      (let* ([hdrivers/level
                              (for/list ([i (in-range nesting)])
                                (dset-filter hdrivers (pvar/dd<=? (+ depth i))))]
                             [new-hdrivers/level
                              (let loop ([raw hdrivers/level] [last (dset)])
                                (cond [(null? raw) null]
                                      [else
                                       (cons (dset-subtract (car raw) last)
                                             (loop (cdr raw) (car raw)))]))])
                        (vector 'dots hguide new-hdrivers/level nesting #f tguide))
                      (cons-guide hprops-guide (cons-guide '_ tprops-guide)))))]
     [(head . tail)
      (let-values ([(hdrivers hsplice? hguide hprops-guide)
                    (parse-h #'head depth esc?)]
                   [(tdrivers tguide tprops-guide)
                    (parse-t #'tail depth esc?)])
        (wrap-props t
                    (dset-union hdrivers tdrivers)
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
      (wrap-props t (dset) '_ '_)]))

 ;; parse-h : stx nat boolean -> (values (dsetof env-entry) boolean pre-head-guide props-guide)
 (define (parse-h h depth esc?)
   (syntax-case h (?? ?@ unsyntax-splicing)
     [(?? t)
      (not esc?)
      (let-values ([(drivers splice? guide props-guide)
                    (parse-h #'t depth esc?)])
        (values drivers #t
                (vector 'app-opt guide)
                (list-guide '_ props-guide)))]
     [(?? t1 t2)
      (not esc?)
      (let-values ([(drivers1 splice?1 guide1 props-guide1) (parse-h #'t1 depth esc?)]
                   [(drivers2 splice?2 guide2 props-guide2) (parse-h #'t2 depth esc?)])
        (values (dset-union drivers1 drivers2)
                (or splice?1 splice?2)
                (vector (if (or splice?1 splice?2) 'orelse-h 'orelse)
                        guide1 guide2)
                (list-guide '_ props-guide1 props-guide2)))]
     [(?@ . t)
      (not esc?)
      (let-values ([(drivers guide props-guide) (parse-t #'t depth esc?)])
        (values drivers #t (vector 'splice guide) (cons-guide '_ props-guide)))]
     [(unsyntax-splicing t1)
      (quasi)
      (let ([qval (quasi)])
        (cond [(box? qval)
               (with-syntax ([(tmp) (generate-temporaries #'(unsyntax-splicing-expr))])
                 (set-box! qval (cons (cons #'tmp h) (unbox qval)))
                 (let* ([fake-sm (make-syntax-mapping 0 #'tmp)]
                        [fake-pvar (pvar fake-sm #f #f)])
                   (values (dset fake-pvar) #t (vector 'unsyntax-splicing fake-pvar) '_)))]
              [else
               (parameterize ((quasi (car qval)))
                 (let*-values ([(drivers guide props-guide) (parse-t #'t1 depth esc?)]
                               [(drivers guide props-guide)
                                (wrap-props h
                                            drivers
                                            (list-guide '_ guide)
                                            (list-guide '_ props-guide))])
                   (values drivers #f guide props-guide)))]))]
     [t
      (let-values ([(drivers guide props-guide) (parse-t #'t depth esc?)])
        (values drivers #f guide props-guide))]))

 (define (lookup id depth)
   (let ([v (syntax-local-value id (lambda () #f))])
     (cond [(syntax-pattern-variable? v)
            (let* ([pvar-depth (syntax-mapping-depth v)]
                   [attr (syntax-local-value (syntax-mapping-valvar v) (lambda () #f))]
                   [attr (and (attribute-mapping? attr) attr)])
              (cond [(not depth) ;; not looking for pvars, only for metafuns
                     #f]
                    [(zero? pvar-depth)
                     (pvar v attr #f)]
                    [(>= depth pvar-depth)
                     (pvar v attr (- depth pvar-depth))]
                    [else
                     (wrong-syntax id "missing ellipses with pattern variable in template")]))]
           [(template-metafunction? v)
            v]
           [else
            ;; id is a literal; check that for all x s.t. id = x.y, x is not an attribute
            (for ([pfx (in-list (dotted-prefixes id))])
              (let ([pfx-v (syntax-local-value pfx (lambda () #f))])
                (when (and (syntax-pattern-variable? pfx-v)
                           (let ([valvar (syntax-mapping-valvar pfx-v)])
                             (attribute-mapping? (syntax-local-value valvar (lambda () #f)))))
                  (wrong-syntax id "undefined nested attribute of attribute `~a'" (syntax-e pfx)))))
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
     [(pvar sm attr dd) (and dd (<= dd expected-dd))]
     [_ #f]))

 (define (pvar-var x)
   (match x
     [(pvar sm '#f dd) (syntax-mapping-valvar sm)]
     [(pvar sm attr dd) (attribute-mapping-var attr)]))

 (define (pvar-check? x)
   (match x
     [(pvar sm '#f dd) #f]
     [(pvar sm attr dd) (not (attribute-mapping-syntax? attr))]))

 (define (stx-drop n x)
   (cond [(zero? n) x]
         [else (stx-drop (sub1 n) (stx-cdr x))]))
 )
