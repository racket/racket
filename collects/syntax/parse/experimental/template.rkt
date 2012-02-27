#lang racket/base
(require (for-syntax racket/base
                     racket/set
                     racket/syntax
                     racket/match
                     racket/private/sc
                     unstable/struct)
         racket/match
         racket/vector
         syntax/stx
         syntax/parse/private/residual
         unstable/struct)
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

(define-syntaxes (?? ?@)
  (let ([tx (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx))])
    (values tx tx)))

;; ============================================================

#|
A Guide (G) is one of:
  - _
  - (G . G)
  - integer
  - #s(stxvector G)
  - #s(stxstruct G)
  - #&G
  - #s(dots HG (vector-of integer) nat G)
  - #s(app HG G)
  - #s(escaped G)
  - #s(orelse G (vector-of integer) G)
  - #s(metafun integer G)
  "optimized" forms:
  - #s(sdots G integer G) ; simple head template, one driver, nesting=1

A HeadGuide (HG) is one of:
  - G
  - #s(app-opt G (vector-of integer))
  - #s(splice G)

A Pre-Guide is like a Guide but with pvars and pvar sets instead of
integers and integer vectors.
|#

(define-syntax-rule (begin-both-phases form ...)
  (begin (begin-for-syntax form ...)
         (begin form ...)))

(begin-both-phases
 (struct stxvector (g) #:prefab)
 (struct stxstruct (g) #:prefab)
 (struct dots (head hdrivers nesting tail) #:prefab)
 (struct app (head tail) #:prefab)
 (struct escaped (body) #:prefab)
 (struct orelse (g1 drivers1 g2) #:prefab)
 (struct metafun (index g) #:prefab)
 (struct app-opt (g drivers) #:prefab)
 (struct splice (g) #:prefab)

 (struct sdots (head driver tail) #:prefab)

 (define (head-guide? x)
   (or (app-opt? x) (splice? x)))
 )

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

 (define (parse-template t)
   (let-values ([(_const? drivers pre-guide) (parse-t t 0 #f)])
     (define (set->env drivers)
       (for/hash ([pvar (in-set drivers)]
                  [n (in-naturals 1)])
         (values pvar n)))
     (define main-env (set->env drivers))
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
         [(dots head hdrivers nesting tail)
          (cond [(and (= nesting 1)
                      (= (set-count hdrivers) 1)
                      (not (head-guide? head)))
                 (let* ([pvar (for/first ([pvar (in-set hdrivers)]) pvar)]
                        [sub-loop-env (hash pvar 0)])
                   (sdots (loop head sub-loop-env)
                          (get-index pvar)
                          (loop tail loop-env)))]
                [else
                 (let* ([sub-loop-env (set->env hdrivers)]
                        [sub-loop-vector (index-hash->vector sub-loop-env get-index)])
                   (dots (loop head sub-loop-env)
                         sub-loop-vector
                         nesting
                         (loop tail loop-env)))])]
         [(app head tail)
          (app (loop head loop-env) (loop tail loop-env))]
         [(escaped g1)
          (escaped (loop g1 loop-env))]
         [(orelse g1 drivers1 g2)
          (orelse (loop g1 loop-env)
                  (for/vector ([pvar (in-set drivers1)])
                    (get-index pvar))
                  (loop g2 loop-env))]
         [(metafun mf g1)
          (metafun (get-index mf)
                   (loop g1 loop-env))]
         [(stxvector g1)
          (stxvector (loop g1 loop-env))]
         [(stxstruct g1)
          (stxstruct (loop g1 loop-env))]
         [(? box?)
          (box (loop (unbox g) loop-env))]
         [(app-opt g1 drivers1)
          (app-opt (loop g1 loop-env)
                   (for/vector ([pvar (in-set drivers1)])
                     (get-index pvar)))]
         [(splice g1)
          (splice (loop g1 loop-env))]
         [else (error 'template "internal error: bad pre-guide: ~e" g)]))
     (define guide (loop pre-guide #hash()))
     (values guide
             (index-hash->vector main-env))))

 ;; ----------------------------------------

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
                      (values #f (set pvar) pvar)]
                     [(template-metafunction? pvar)
                      (wrong-syntax t "illegal use of syntax metafunction")]
                     [else (values #t (set) '_)]))])]
     [atom
      (atom? (syntax-e #'atom))
      (values #t (set) '_)]
     [(mf . template)
      (and (not esc?)
           (identifier? #'mf)
           (template-metafunction? (lookup #'mf #f)))
      (let-values ([(mf) (lookup #'mf #f)]
                   [(const? drivers guide) (parse-t #'template depth esc?)])
        (values #f
                (set-union (set mf) drivers)
                (metafun mf guide)))]
     [(DOTS template)
      (and (not esc?)
           (identifier? #'DOTS) (free-identifier=? #'DOTS (quote-syntax ...)))
      (let-values ([(const? drivers guide) (parse-t #'template depth #t)])
        (values #f drivers (escaped guide)))]
     [(?? t1 t2)
      (not esc?)
      (let-values ([(const1? drivers1 guide1) (parse-t #'t1 depth esc?)]
                   [(const2? drivers2 guide2) (parse-t #'t2 depth esc?)])
        (values #f
                (set-union drivers1 drivers2)
                (orelse guide1 (set-filter drivers1 syntax-pattern-variable?) guide2)))]
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
        (let-values ([(hconst? hdrivers _hsplice? hguide) (parse-h #'head (+ depth nesting) esc?)]
                     [(tconst? tdrivers tguide) (parse-t tail depth esc?)])
          (unless (positive? (set-count hdrivers))
            (wrong-syntax #'head "no pattern variables in term before ellipsis"))
          (values #f
                  (set-union hdrivers tdrivers)
                  (dots hguide (set-filter hdrivers pvar/depth>0?) nesting tguide))))]
     [(head . tail)
      (let-values ([(hconst? hdrivers hsplice? hguide) (parse-h #'head depth esc?)]
                   [(tconst? tdrivers tguide) (parse-t #'tail depth esc?)])
        (let ([const? (and hconst? tconst?)])
          (values const?
                  (set-union hdrivers tdrivers)
                  (cond [const? '_]
                        [hsplice? (app hguide tguide)]
                        [else (cons hguide tguide)]))))]
     [vec
      (vector? (syntax-e #'vec))
      (let-values ([(const? drivers guide) (parse-t (vector->list (syntax-e #'vec)) depth esc?)])
        (values const? drivers (if const? '_ (stxvector guide))))]
     [pstruct
      (prefab-struct-key (syntax-e #'pstruct))
      (let-values ([(const? drivers guide) (parse-t (struct->list (syntax-e #'pstruct)) depth esc?)])
        (values const? drivers (if const? '_ (stxstruct guide))))]
     [#&template
      (let-values ([(const? drivers guide) (parse-t #'template depth esc?)])
        (values const? drivers (if const? '_ (box guide))))]
     [_ (wrong-syntax t "bad pattern")]))

 (define (parse-h h depth esc?)
   (syntax-case h (?? ?@)
     [(?? t)
      (not esc?)
      (let-values ([(const? drivers guide) (parse-t #'t depth esc?)])
        (values #f drivers #t (app-opt guide (set-filter drivers syntax-pattern-variable?))))]
     [(?@ . t)
      (not esc?)
      (let-values ([(const? drivers guide) (parse-t #'t depth esc?)])
        (values #f drivers #t (splice guide)))]
     [t
      (let-values ([(const? drivers guide) (parse-t #'t depth esc?)])
        (values const? drivers #f guide))]))

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

;; ============================================================

(define (substitute stx g main-env)
  (define (get index lenv)
    (cond [(positive? index)
           (vector-ref main-env (sub1 index))]
          [(negative? index)
           (vector-ref lenv (- -1 index))]
          [(zero? index) lenv]))
  (define (loop stx g lenv)
    (match g
      ['_ stx]
      [(cons g1 g2)
       (restx stx (cons (loop (stx-car stx) g1 lenv) (loop (stx-cdr stx) g2 lenv)))]
      [(? exact-integer? index)
       (let ([v (get index lenv)])
         (if (syntax? v)
             v
             (error/not-stx stx v)))]
      [(sdots ghead loop-var gtail)
       (let ([lenv* (get loop-var lenv)])
         (unless lenv* (error 'template "pattern variable used in ellipsis pattern is not defined"))
         (restx stx
                (if (equal? ghead '0) ;; pattern was just (pvar ... . T)
                    (append lenv* (loop (stx-cddr stx) gtail lenv))
                    (let ([head-stx (stx-car stx)])
                      (let dotsloop ([lenv* lenv*])
                        (if (null? lenv*)
                            (loop (stx-cddr stx) gtail lenv)
                            (cons (loop head-stx ghead (car lenv*))
                                  (dotsloop (cdr lenv*)))))))))]
      [(dots ghead henv nesting gtail)
       (define head-stx (stx-car stx))
       (define (nestloop lenv* nesting)
         (cond [(zero? nesting)
                (loop-h head-stx ghead lenv*)]
               [else
                (for ([v (in-vector lenv*)])
                  (unless v (error 'template "pattern variable used in ellipsis pattern is not defined")))
                (let ([len0 (length (vector-ref lenv* 0))])
                  (for ([v (in-vector lenv*)])
                    (unless (= len0 (length v))
                      (raise-syntax-error 'template
                                          "incomplatible ellipsis match counts for template"
                                          stx)))
                  (let dotsloop ([len0 len0] [lenv* lenv*])
                    (if (zero? len0)
                        null
                        (let ([lenv** (vector-map car lenv*)])
                          (cons (nestloop lenv** (sub1 nesting))
                                (dotsloop (sub1 len0) (vector-map! cdr lenv*)))))))]))
       (let ([head-results ;; (listof^nesting (listof stx)) -- extra listof for loop-h
              (nestloop (vector-map (lambda (index) (get index lenv)) henv) nesting)]
             [tail-result (loop (stx-drop nesting (stx-cdr stx)) gtail lenv)])
         (restx stx (deep-append head-results nesting tail-result)))]
      [(app ghead gtail)
       (restx stx (append (loop-h (stx-car stx) ghead lenv)
                          (loop (stx-cdr stx) gtail lenv)))]
      [(escaped g1)
       (loop (stx-cadr stx) g1 lenv)]
      [(orelse g1 drivers1 g2)
       (if (for/and ([index (in-vector drivers1)]) (get index lenv))
           (loop (stx-cadr stx) g1 lenv)
           (loop (stx-caddr stx) g2 lenv))]
      [(metafun index g1)
       (let ([v (restx stx (cons (stx-car stx) (loop (stx-cdr stx) g1 lenv)))]
             [mark (make-syntax-introducer)]
             [old-mark (current-template-metafunction-introducer)]
             [mf (get index lenv)])
         (parameterize ((current-template-metafunction-introducer mark))
           (let ([r (mf (mark (old-mark v)))])
             (unless (syntax? r)
               (raise-syntax-error 'template "result of metafunction was not syntax" stx))
             (restx stx (old-mark (mark r))))))]
      [(stxvector g1)
       (restx stx (list->vector (loop (vector->list (syntax-e stx)) g1 lenv)))]
      [(stxstruct g1)
       (let ([s (syntax-e stx)])
         (restx stx (apply make-prefab-struct
                           (prefab-struct-key s)
                           (loop (struct->list s) g1 lenv))))]
      [(box g1)
       (restx stx (box (loop (unbox (syntax-e stx)) g1 lenv)))]))
  (define (loop-h stx hg lenv)
    (match hg
      [(app-opt g1 drivers1)
       (if (for/and ([index (in-vector drivers1)]) (get index lenv))
           (list (loop (stx-cadr stx) g1 lenv))
           null)]
      [(splice g1)
       (let* ([v (loop (stx-cdr stx) g1 lenv)]
              [v* (stx->list v)])
         (unless v*
           (raise-syntax-error 'template
                               "splicing template did not produce a syntax list"
                               stx))
         v*)]
      [else (list (loop stx hg lenv))]))
  (loop stx g #f))

(define current-template-metafunction-introducer
  (make-parameter
   (lambda (stx)
     (if (syntax-transforming?)
         (syntax-local-introduce stx)
         stx))))

(define (stx-cadr x) (stx-car (stx-cdr x)))
(define (stx-cddr x) (stx-cdr (stx-cdr x)))
(define (stx-caddr x) (stx-car (stx-cdr (stx-cdr x))))

(define (stx-drop n x)
  (cond [(zero? n) x]
        [else (stx-drop (sub1 n) (stx-cdr x))]))

(define (restx basis val)
  (if (syntax? basis)
      (datum->syntax basis val basis basis)
      val))

;; deep-append : (listof^(nesting+1) A) nat (listof A) -> (listof A)
;; (Actually, in practice onto is stx, so this is an improper append.)
(define (deep-append lst nesting onto)
  (cond [(null? lst) onto]
        [(zero? nesting) (append lst onto)]
        [else (deep-append (car lst) (sub1 nesting)
                           (deep-append (cdr lst) nesting onto))]))

(define (error/not-stx stx val)
  (raise-syntax-error 'template "pattern variable is not syntax-valued" stx))
