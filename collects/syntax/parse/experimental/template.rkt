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
- support flexible depths, eg
  (with-syntax ([(a ...) #'(1 2 3)]
                [((b ...) ...) #'((1 2 3) (4 5 6) (7 8 9))])
    #'(((a b) ...) ...))  ;; a has depth 1, used at depth 2
- support #hash templates, etc (check for other atomic & compound forms)
|#

#|
A Template (T) is one of:
  - pvar
  - atom (including (), not pvar)
  - (metafunction . T)
  - (H . T)
  - (H ... . T), (H ... ... . T), etc
  - (?? T T)
  - ... other standard compound forms

A HeadTemplate (H) is one of:
  - T
  - (?? T)
  - (?@ . T)
|#

(define-syntax (template stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      [(template t)
       (let-values ([(guide deps) (parse-template #'t)])
         ;; (eprintf "guide = ~s\n" guide)
         (let ([vars
                (for/list ([dep (in-vector deps)])
                  (cond [(syntax-pattern-variable? dep)
                         (let* ([valvar (syntax-mapping-valvar dep)]
                                [attr (syntax-local-value valvar (lambda () #f))])
                           (cond [(attribute-mapping? attr)
                                  (attribute-mapping-var attr)]
                                 [else valvar]))]
                        [(template-metafunction? dep)
                         (template-metafunction-var dep)]))])
           (syntax-arm
            (cond [(equal? guide '1) ;; was (template pvar)
                   (with-syntax ([var (car vars)])
                     #'(if (syntax? var)
                           var
                           (error/not-stx (quote-syntax t) var)))]
                  [(equal? guide '_) ;; constant
                   #`(quote-syntax t)]
                  [else
                   (with-syntax ([guide guide]
                                 [(var ...) vars])
                     #'(substitute (quote-syntax t)
                                   'guide
                                   (vector var ...)))]))))])))

;; substitute-table : hash[stx => translated-template]
;; Cache for closure-compiled templates. Key is just syntax of
;; template, since eq? templates must have equal? guides.
(define substitute-table (make-weak-hasheq))

(define (substitute stx g main-env)
  (let ([f (or (hash-ref substitute-table stx #f)
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

An env-entry is one of
  - syntax-mapping (for pattern variables)
  - template-metafunction

A Pre-Guide is like a Guide but with env-entry and (setof env-entry)
instead of integers and integer vectors.
|#

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

 ;; parse-template : stx -> (values guide (vectorof env-entry))
 (define (parse-template t)
   (let-values ([(drivers pre-guide) (parse-t t 0 #f)])
     (define main-env (set->env drivers))
     (define guide (guide-resolve-env pre-guide main-env))
     (values guide
             (index-hash->vector main-env))))

 ;; set->env : (setof env-entry) -> hash[env-entry => nat]
 (define (set->env drivers)
   (for/hash ([pvar (in-set drivers)]
              [n (in-naturals 1)])
     (values pvar n)))

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
       [(? syntax-pattern-variable? pvar) (get-index pvar)]
       [(vector 'dots head hdrivers nesting tail)
        (let* ([sub-loop-env (set->env hdrivers)]
               [sub-loop-vector (index-hash->vector sub-loop-env get-index)])
          (vector 'dots
                  (loop head sub-loop-env)
                  sub-loop-vector
                  nesting
                  (loop tail loop-env)))]
       [(vector 'app head tail)
        (vector 'app (loop head loop-env) (loop tail loop-env))]
       [(vector 'escaped g1)
        (vector 'escaped (loop g1 loop-env))]
       [(vector 'orelse g1 drivers1 g2)
        (vector 'orelse
                (loop g1 loop-env)
                (for/vector ([pvar (in-set drivers1)])
                  (get-index pvar))
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
       [(vector 'app-opt g1 drivers1)
        (vector 'app-opt
                (loop g1 loop-env)
                (for/vector ([pvar (in-set drivers1)])
                  (get-index pvar)))]
       [(vector 'splice g1)
        (vector 'splice (loop g1 loop-env))]
       [else (error 'template "internal error: bad pre-guide: ~e" g)]))
   (loop g0 '#hash()))

 ;; ----------------------------------------

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
               (cond [(syntax-pattern-variable? pvar)
                      (values (set pvar) pvar)]
                     [(template-metafunction? pvar)
                      (wrong-syntax t "illegal use of syntax metafunction")]
                     [else (values (set) '_)]))])]
     [atom
      (atom? (syntax-e #'atom))
      (values (set) '_)]
     [(mf . template)
      (and (not esc?)
           (identifier? #'mf)
           (template-metafunction? (lookup #'mf #f)))
      (let-values ([(mf) (lookup #'mf #f)]
                   [(drivers guide) (parse-t #'template depth esc?)])
        (values (set-union (set mf) drivers)
                (vector 'metafun mf guide)))]
     [(DOTS template)
      (and (not esc?)
           (identifier? #'DOTS) (free-identifier=? #'DOTS (quote-syntax ...)))
      (let-values ([(drivers guide) (parse-t #'template depth #t)])
        (values drivers (vector 'escaped guide)))]
     [(?? t1 t2)
      (not esc?)
      (let-values ([(drivers1 guide1) (parse-t #'t1 depth esc?)]
                   [(drivers2 guide2) (parse-t #'t2 depth esc?)])
        (values (set-union drivers1 drivers2)
                (vector 'orelse guide1 (set-filter drivers1 syntax-pattern-variable?) guide2)))]
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
        (let-values ([(hdrivers _hsplice? hguide) (parse-h #'head (+ depth nesting) esc?)]
                     [(tdrivers tguide) (parse-t tail depth esc?)])
          (unless (positive? (set-count hdrivers))
            (wrong-syntax #'head "no pattern variables in term before ellipsis"))
          (values (set-union hdrivers tdrivers)
                  (vector 'dots hguide (set-filter hdrivers pvar/depth>0?) nesting tguide))))]
     [(head . tail)
      (let-values ([(hdrivers hsplice? hguide) (parse-h #'head depth esc?)]
                   [(tdrivers tguide) (parse-t #'tail depth esc?)])
        (values (set-union hdrivers tdrivers)
                (cond [(and (eq? hguide '_) (eq? tguide '_)) '_]
                      [hsplice? (vector 'app hguide tguide)]
                      [else (cons hguide tguide)])))]
     [vec
      (vector? (syntax-e #'vec))
      (let-values ([(drivers guide) (parse-t (vector->list (syntax-e #'vec)) depth esc?)])
        (values drivers (if (eq? guide '_) '_ (vector 'vector guide))))]
     [pstruct
      (prefab-struct-key (syntax-e #'pstruct))
      (let-values ([(drivers guide)
                    (parse-t (cdr (vector->list (struct->vector (syntax-e #'pstruct)))) depth esc?)])
        (values drivers (if (eq? guide '_) '_ (vector 'struct guide))))]
     [#&template
      (let-values ([(drivers guide) (parse-t #'template depth esc?)])
        (values drivers (if (eq? guide '_) '_ (vector 'box guide))))]
     [_ (wrong-syntax t "bad template")]))

 ;; parse-h : stx nat boolean -> (values (setof env-entry) boolean pre-head-guide)
 (define (parse-h h depth esc?)
   (syntax-case h (?? ?@)
     [(?? t)
      (not esc?)
      (let-values ([(drivers guide) (parse-t #'t depth esc?)])
        (values drivers #t (vector 'app-opt guide (set-filter drivers syntax-pattern-variable?))))]
     [(?@ . t)
      (not esc?)
      (let-values ([(drivers guide) (parse-t #'t depth esc?)])
        (values drivers #t (vector 'splice guide)))]
     [t
      (let-values ([(drivers guide) (parse-t #'t depth esc?)])
        (values drivers #f guide))]))

 (define (atom? x)
   (or (null? x)
       (number? x)
       (boolean? x)
       (string? x)
       (bytes? x)
       (keyword? x)
       (regexp? x)
       (char? x)))

 ;; Note: always creates equal?-based set.
 (define (set-filter s pred?)
   (for/set ([el (in-set s)] #:when (pred? el)) el))

 (define (lookup id depth)
   (let ([v (syntax-local-value id (lambda () #f))])
     (cond [(syntax-pattern-variable? v)
            (unless (or (not depth)
                        (= (syntax-mapping-depth v) depth)
                        (= (syntax-mapping-depth v) 0))
              (wrong-syntax id
                            "pattern variable used at wrong ellipsis depth (expected ~s, used at ~s)"
                            (syntax-mapping-depth v)
                            depth))
            v]
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

 (define (pvar/depth>0? x)
   (and (syntax-pattern-variable? x)
        (positive? (syntax-mapping-depth x))))
 )
