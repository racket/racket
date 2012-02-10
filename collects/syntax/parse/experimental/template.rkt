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
       (let-values ([(guide pvars) (parse-template #'t)])
         ;; (eprintf "guide = ~s\n" guide)
         (with-syntax ([guide
                        guide]
                       [(var ...)
                        (for/list ([pvar (in-vector pvars)])
                          (let* ([valvar (syntax-mapping-valvar pvar)]
                                 [attr (syntax-local-value valvar (lambda () #f))])
                            (cond [(attribute-mapping? attr)
                                   (attribute-mapping-var attr)]
                                  [else valvar])))])
           (syntax-arm #'(substitute (quote-syntax t) 'guide (vector var ...)))))])))

(define-syntaxes (?? ?@)
  (let ([tx (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx))])
    (values tx tx)))

;; ============================================================

#|
A Guide (G) is one of:
  - _
  - (G . G)
  - positive-integer
  - negative-integer
  - #s(stxvector G)
  - #s(stxstruct G)
  - #&G
  - #s(dots HG (vector-of integer) nat G)
  - #s(app HG G)
  - #s(escaped G)
  - #s(orelse G (vector-of integer) G)

A HeadGuide (HG) is one of:
  - G
  - #s(app-opt G (vector-of integer))
  - #s(splice G)
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
 (struct app-opt (g drivers) #:prefab)
 (struct splice (g) #:prefab))

;; ============================================================

(begin-for-syntax

 (define (parse-template t)
   (let-values ([(_const? drivers pre-guide) (parse-t t 0 #f)])
     ;; (eprintf "pre-guide = ~s\n" pre-guide)
     (define (pvar-set->env drivers)
       (for/hash ([pvar (in-set drivers)]
                  [n (in-naturals 1)])
         (values pvar n)))
     (define main-env (pvar-set->env drivers))
     (define (loop g loop-env)
       (define (pvar->index pvar)
         (let ([loop-index (hash-ref loop-env pvar #f)])
           (if loop-index
               (- loop-index)
               (hash-ref main-env pvar))))
       (match g
         ['_ '_]
         [(cons g1 g2) (cons (loop g1 loop-env) (loop g2 loop-env))]
         [(? syntax-pattern-variable? pvar) (pvar->index pvar)]
         [(dots head hdrivers nesting tail)
          (let* ([sub-loop-env (pvar-set->env hdrivers)]
                 [sub-loop-vector (index-hash->vector sub-loop-env pvar->index)])
            (dots (loop head sub-loop-env)
                  sub-loop-vector
                  nesting
                  (loop tail loop-env)))]
         [(app head tail)
          (app (loop head loop-env) (loop tail loop-env))]
         [(escaped g1)
          (escaped (loop g1 loop-env))]
         [(orelse g1 drivers1 g2)
          (orelse (loop g1 loop-env)
                  (for/vector ([pvar (in-set drivers1)])
                    (pvar->index pvar))
                  (loop g2 loop-env))]
         [(stxvector g1)
          (stxvector (loop g1 loop-env))]
         [(stxstruct g1)
          (stxstruct (loop g1 loop-env))]
         [(? box?)
          (box (loop (unbox g) loop-env))]
         [(app-opt g1 drivers1)
          (app-opt (loop g1 loop-env)
                   (for/vector ([pvar (in-set drivers1)])
                     (pvar->index pvar)))]
         [(splice g1)
          (splice (loop g1 loop-env))]
         [else (error 'parse:convert "bad pre-guide: ~e" g)]))
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
             (let ([pvar (lookup-pvar #'id depth)])
               (cond [pvar (values #f (set pvar) pvar)]
                     [else (values #t (set) '_)]))])]
     [atom
      (atom? (syntax-e #'atom))
      (values #t (set) '_)]
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
          (values #f
                  (set-union hdrivers tdrivers)
                  (dots hguide hdrivers nesting tguide))))]
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
                (orelse guide1 drivers1 guide2)))]
     [(?? . _)
      (not esc?)
      (wrong-syntax t "bad pattern")]
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
        (values #f drivers #t (app-opt guide drivers)))]
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

 (define (lookup-pvar id depth)
   (let ([v (syntax-local-value id (lambda () #f))])
     (cond [(syntax-pattern-variable? v)
            (unless (or (= (syntax-mapping-depth v) depth)
                        (= (syntax-mapping-depth v) 0))
              (wrong-syntax id
                            "pattern variable used at wrong ellipsis depth (expected ~s, used at ~s)"
                            (syntax-mapping-depth v)
                            depth))
            v]
           [else #f])))

 (define (index-hash->vector hash [f values])
   (let ([vec (make-vector (hash-count hash))])
     (for ([(value index) (in-hash hash)])
       (vector-set! vec (sub1 index) (f value)))
     vec))
 )

;; ============================================================

(define (substitute stx g main-env)
  ;; (eprintf "main-env = ~s\n" main-env)
  (define (get index lenv)
    (cond [(positive? index)
           (vector-ref main-env (sub1 index))]
          [else
           (vector-ref lenv (- -1 index))]))
  (define (loop stx g lenv)
    (match g
      ['_ stx]
      [(cons g1 g2)
       (restx stx (cons (loop (stx-car stx) g1 lenv) (loop (stx-cdr stx) g2 lenv)))]
      [(? exact-integer? index)
       (let ([v (get index lenv)])
         (unless (syntax? v)
           (error 'template "syntax pattern variable is not syntax-valued"))
         v)]
      [(dots ghead henv nesting gtail)
       (define head-stx (stx-car stx))
       (define (nestloop lenv* nesting)
         (cond [(zero? nesting)
                (loop-h head-stx ghead lenv*)]
               [else
                (for ([v (in-vector lenv*)])
                  (unless v (error 'template "loop variable is not defined")))
                (let ([len0 (length (vector-ref lenv* 0))])
                  (for ([v (in-vector lenv*)])
                    (unless (= len0 (length v))
                      (error 'template "loop variable count mismatch")))
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
           (error 'template "not a syntax list: ~e" v))
         v*)]
      [else (list (loop stx hg lenv))]))
  (loop stx g #f))

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
