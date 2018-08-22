#lang racket/base
(require racket/syntax
         racket/pretty
         syntax/parse/private/residual-ct ;; keep abs. path
         "minimatch.rkt"
         "rep-patterns.rkt"
         "kws.rkt")
(provide (struct-out pk1)
         (rename-out [optimize-matrix0 optimize-matrix]))

;; controls debugging output for optimization successes and failures
(define DEBUG-OPT-SUCCEED #f)
(define DEBUG-OPT-FAIL #f)

;; ----

;; A Matrix is a (listof PK) where each PK has same number of columns
;; A PK is one of
;;  - (pk1 (listof pattern) expr) -- a simple row in a parsing matrix
;;  - (pk/same pattern Matrix)    -- a submatrix with a common first column factored out
;;  - (pk/pair Matrix)            -- a submatrix with pair patterns in the first column unfolded
;;  - (pk/and Matrix)             -- a submatrix with and patterns in the first column unfolded
(struct pk1 (patterns k) #:prefab)
(struct pk/same (pattern inner) #:prefab)
(struct pk/pair (inner) #:prefab)
(struct pk/and (inner) #:prefab)

(define (pk-columns pk)
  (match pk
    [(pk1 patterns k) (length patterns)]
    [(pk/same p inner) (add1 (pk-columns inner))]
    [(pk/pair inner) (sub1 (pk-columns inner))]
    [(pk/and inner) (sub1 (pk-columns inner))]))

;; Can factor pattern P given clauses like
;;   [ P P1 ... | e1]     [  | [P1 ... | e1] ]
;;   [ P  ⋮     |  ⋮]  => [P | [ ⋮     |  ⋮] ]
 ;   [ P PN ... | eN]     [  | [PN ... | eN] ]
;; if P cannot cut and P succeeds at most once (otherwise may reorder backtracking)

;; Can unfold pair patterns as follows:
;;   [ (P11 . P12) P1 ... | e1 ]                [ P11 P12 P1 ... | e1 ]
;;   [      ⋮      ⋮      |  ⋮ ] => check pair, [      ⋮         |  ⋮ ]
;;   [ (PN1 . PN2) PN ... | eN ]                [ PN1 PN2 PN ... | eN ]

;; Can unfold ~and patterns similarly; ~and patterns can hide
;; factoring opportunities.

;; ----

(define (optimize-matrix0 rows)
  (define now (current-inexact-milliseconds))
  (when (and DEBUG-OPT-SUCCEED (> (length rows) 1))
    (eprintf "\n%% optimizing (~s):\n" (length rows))
    (pretty-write (matrix->sexpr rows) (current-error-port)))
  (define result (optimize-matrix rows))
  (define then (current-inexact-milliseconds))
  (when (and DEBUG-OPT-SUCCEED (> (length rows) 1))
    (cond [(= (length result) (length rows))
           (eprintf "%% !! FAILED !! (~s ms)\n\n" (floor (- then now)))]
          [else
           (eprintf "==> (~s ms)\n" (floor (- then now)))
           (pretty-write (matrix->sexpr result) (current-error-port))
           (eprintf "\n")]))
  result)

;; optimize-matrix : (listof pk1) -> Matrix
(define (optimize-matrix rows)
  (cond [(null? rows) null]
        [(null? (cdr rows)) rows] ;; no opportunities for 1 row
        [(null? (pk1-patterns (car rows))) rows]
        [else
         ;; first unfold and-patterns
         (let-values ([(col1 col2)
                       (for/lists (col1 col2) ([row (in-list rows)])
                         (unfold-and (car (pk1-patterns row)) null))])
           (cond [(ormap pair? col2)
                  (list
                   (pk/and
                    (optimize-matrix*
                     (for/list ([row (in-list rows)]
                                [col1 (in-list col1)]
                                [col2 (in-list col2)])
                       (pk1 (list* col1
                                   (make-and-pattern col2)
                                   (cdr (pk1-patterns row)))
                            (pk1-k row))))))]
                 [else (optimize-matrix* rows)]))]))

;; optimize-matrix* : (listof pk1) -> Matrix
;; The matrix is nonempty, and first column has no unfoldable pat:and.
;; Split into submatrixes (sequences of rows) starting with similar patterns,
;; handle according to similarity, then recursively optimize submatrixes.
(define (optimize-matrix* rows)
  (define row1 (car rows))
  (define pat1 (car (pk1-patterns row1)))
  (define k1 (pk1-k row1))
  ;; Now accumulate rows starting with patterns like pat1
  (define-values (like? combine) (pattern->partitioner pat1))
  (let loop ([rows (cdr rows)] [rrows (list row1)])
    (cond [(null? rows)
           (cons (combine (reverse rrows)) null)]
          [else
           (define row1 (car rows))
           (define pat1 (car (pk1-patterns row1)))
           (cond [(like? pat1)
                  (loop (cdr rows) (cons row1 rrows))]
                 [else
                  (cons (combine (reverse rrows))
                        (optimize-matrix* rows))])])))

;; pattern->partitioner : pattern -> (values (pattern -> boolean) ((listof pk1) -> PK))
(define (pattern->partitioner pat1)
  (match pat1
    [(pat:pair head tail)
     (values (lambda (p) (pat:pair? p))
             (lambda (rows)
               (when DEBUG-OPT-SUCCEED
                 (eprintf "-- accumulated ~s rows like ~e\n" (length rows) (pattern->sexpr pat1)))
               (cond [(> (length rows) 1)
                      (pk/pair (optimize-matrix
                                (for/list ([row (in-list rows)])
                                  (let* ([patterns (pk1-patterns row)]
                                         [pat1 (car patterns)])
                                    (pk1 (list* (pat:pair-head pat1)
                                                (pat:pair-tail pat1)
                                                (cdr patterns))
                                         (pk1-k row))))))]
                     [else (car rows)])))]
    [(? pattern-factorable?)
     (values (lambda (pat2) (pattern-equal? pat1 pat2))
             (lambda (rows)
               (when DEBUG-OPT-SUCCEED
                 (eprintf "-- accumulated ~s rows like ~e\n" (length rows) (pattern->sexpr pat1)))
               (cond [(> (length rows) 1)
                      (pk/same pat1
                               (optimize-matrix
                                (for/list ([row (in-list rows)])
                                  (pk1 (cdr (pk1-patterns row)) (pk1-k row)))))]
                     [else (car rows)])))]
    [_
     (values (lambda (pat2)
               (when DEBUG-OPT-FAIL
                 (when (pattern-equal? pat1 pat2)
                   (eprintf "** cannot factor: ~e\n" (syntax->datum #`#,pat2))))
               #f)
             (lambda (rows)
               ;; (length rows) = 1
               (car rows)))]))

;; unfold-and : pattern (listof pattern) -> (values pattern (listof pattern))
(define (unfold-and p onto)
  (match p
    [(pat:and subpatterns)
     ;; pat:and is worth unfolding if first subpattern is not pat:action
     ;; if first subpattern is also pat:and, keep unfolding
     (let* ([first-sub (car subpatterns)]
            [rest-subs (cdr subpatterns)])
       (cond [(not (pat:action? first-sub))
              (when #f ;; DEBUG-OPT-SUCCEED
                (eprintf ">> unfolding: ~e\n" p))
              (unfold-and first-sub (*append rest-subs onto))]
             [else (values p onto)]))]
    [_ (values p onto)]))

(define (pattern-factorable? p)
  ;; Can factor out p if p can succeed at most once, does not cut
  ;;  - if p can succeed multiple times, then factoring changes success order
  ;;  - if p can cut, then factoring changes which choice points are discarded (too few)
  (match p
    [(pat:any) #t]
    [(pat:svar _n) #t]
    [(pat:var/p _ _ _ _ _ (scopts _ commit? _ _))
     ;; commit? implies delimit-cut
     commit?]
    [(? pat:integrated?) #t]
    [(pat:literal _lit _ip _lp) #t]
    [(pat:datum _datum) #t]
    [(pat:action _act _pat) #f]
    [(pat:head head tail)
     (and (pattern-factorable? head)
          (pattern-factorable? tail))]
    [(pat:dots heads tail)
     ;; Conservative approximation for common case: one head pattern
     ;; In general, check if heads don't overlap, don't overlap with tail.
     (and (= (length heads) 1)
          (let ([head (car heads)])
            (and (pattern-factorable? head)))
          (equal? tail (pat:datum '())))]
    [(pat:and patterns)
     (andmap pattern-factorable? patterns)]
    [(pat:or _ patterns _) #f]
    [(pat:not pattern) #f] ;; FIXME: ?
    [(pat:pair head tail)
     (and (pattern-factorable? head)
          (pattern-factorable? tail))]
    [(pat:vector pattern)
     (pattern-factorable? pattern)]
    [(pat:box pattern)
     (pattern-factorable? pattern)]
    [(pat:pstruct key pattern)
     (pattern-factorable? pattern)]
    [(pat:describe pattern _desc _trans _role)
     (pattern-factorable? pattern)]
    [(pat:delimit pattern)
     (pattern-factorable? pattern)]
    [(pat:commit pattern) #t]
    [(? pat:reflect?) #f]
    [(pat:ord pattern _ _)
     (pattern-factorable? pattern)]
    [(pat:post pattern)
     (pattern-factorable? pattern)]
    ;; ----
    [(hpat:var/p _ _ _ _ _ (scopts _ commit? _ _))
     commit?]
    [(hpat:seq inner)
     (pattern-factorable? inner)]
    [(hpat:commit inner) #t]
    ;; ----
    [(ehpat _ head repc _)
     (and (equal? repc #f)
          (pattern-factorable? head))]
    ;; ----
    [else #f]))

(define (subpatterns-equal? as bs)
  (and (= (length as) (length bs))
       (for/and ([a (in-list as)]
                 [b (in-list bs)])
         (pattern-equal? a b))))

(define (pattern-equal? a b)
  (define result
    (cond [(and (pat:any? a) (pat:any? b)) #t]
          [(and (pat:svar? a) (pat:svar? b))
           (bound-identifier=? (pat:svar-name a) (pat:svar-name b))]
          [(and (pat:var/p? a) (pat:var/p? b))
           (and (free-id/f-equal? (pat:var/p-parser a) (pat:var/p-parser b))
                (bound-id/f-equal? (pat:var/p-name a) (pat:var/p-name b))
                (equal-iattrs? (pat:var/p-nested-attrs a) (pat:var/p-nested-attrs b))
                (equal-argu? (pat:var/p-argu a) (pat:var/p-argu b))
                (expr-equal? (pat:var/p-role a) (pat:var/p-role b)))]
          [(and (pat:integrated? a) (pat:integrated? b))
           (and (bound-id/f-equal? (pat:integrated-name a) (pat:integrated-name b))
                (free-identifier=? (pat:integrated-predicate a)
                                   (pat:integrated-predicate b))
                (expr-equal? (pat:integrated-role a) (pat:integrated-role b)))]
          [(and (pat:literal? a) (pat:literal? b))
           ;; literals are hard to compare, so compare gensyms attached to
           ;; literal ids (see rep.rkt) instead
           (let ([ka (syntax-property (pat:literal-id a) 'literal)]
                 [kb (syntax-property (pat:literal-id b) 'literal)])
             (and ka kb (eq? ka kb)))]
          [(and (pat:datum? a) (pat:datum? b))
           (equal? (pat:datum-datum a)
                   (pat:datum-datum b))]
          [(and (pat:head? a) (pat:head? b))
           (and (pattern-equal? (pat:head-head a) (pat:head-head b))
                (pattern-equal? (pat:head-tail a) (pat:head-tail b)))]
          [(and (pat:dots? a) (pat:dots? b))
           (and (subpatterns-equal? (pat:dots-heads a) (pat:dots-heads b))
                (pattern-equal? (pat:dots-tail a) (pat:dots-tail b)))]
          [(and (pat:and? a) (pat:and? b))
           (subpatterns-equal? (pat:and-patterns a) (pat:and-patterns b))]
          [(and (pat:or? a) (pat:or? b))
           (subpatterns-equal? (pat:or-patterns a) (pat:or-patterns b))]
          [(and (pat:not? a) (pat:not? b))
           (pattern-equal? (pat:not-pattern a) (pat:not-pattern b))]
          [(and (pat:pair? a) (pat:pair? b))
           (and (pattern-equal? (pat:pair-head a) (pat:pair-head b))
                (pattern-equal? (pat:pair-tail a) (pat:pair-tail b)))]
          [(and (pat:vector? a) (pat:vector? b))
           (pattern-equal? (pat:vector-pattern a) (pat:vector-pattern b))]
          [(and (pat:box? a) (pat:box? b))
           (pattern-equal? (pat:box-pattern a) (pat:box-pattern b))]
          [(and (pat:pstruct? a) (pat:pstruct? b))
           (and (equal? (pat:pstruct-key a)
                        (pat:pstruct-key b))
                (pattern-equal? (pat:pstruct-pattern a)
                                (pat:pstruct-pattern b)))]
          [(and (pat:describe? a) (pat:describe? b)) #f] ;; can't compare desc exprs
          [(and (pat:delimit? a) (pat:delimit? b))
           (pattern-equal? (pat:delimit-pattern a) (pat:delimit-pattern b))]
          [(and (pat:commit? a) (pat:commit? b))
           (pattern-equal? (pat:commit-pattern a) (pat:commit-pattern b))]
          [(and (pat:reflect? a) (pat:reflect? b)) #f] ;; FIXME: ?
          [(and (pat:ord? a) (pat:ord? b))
           (and (pattern-equal? (pat:ord-pattern a) (pat:ord-pattern b))
                (equal? (pat:ord-group a) (pat:ord-group b))
                (equal? (pat:ord-index a) (pat:ord-index b)))]
          [(and (pat:post? a) (pat:post? b))
           (pattern-equal? (pat:post-pattern a) (pat:post-pattern b))]
          ;; ---
          [(and (hpat:var/p? a) (hpat:var/p? b))
           (and (free-id/f-equal? (hpat:var/p-parser a) (hpat:var/p-parser b))
                (bound-id/f-equal? (hpat:var/p-name a) (hpat:var/p-name b))
                (equal-iattrs? (hpat:var/p-nested-attrs a) (hpat:var/p-nested-attrs b))
                (equal-argu? (hpat:var/p-argu a) (hpat:var/p-argu b))
                (expr-equal? (hpat:var/p-role a) (hpat:var/p-role b)))]
          [(and (hpat:seq? a) (hpat:seq? b))
           (pattern-equal? (hpat:seq-inner a) (hpat:seq-inner b))]
          ;; ---
          [(and (ehpat? a) (ehpat? b))
           (and (equal? (ehpat-repc a) #f)
                (equal? (ehpat-repc b) #f)
                (pattern-equal? (ehpat-head a) (ehpat-head b)))]
          ;; FIXME: more?
          [else #f]))
  (when DEBUG-OPT-FAIL
    (when (and (eq? result #f)
               (equal? (syntax->datum #`#,a) (syntax->datum #`#,b)))
      (eprintf "** pattern-equal? failed on ~e\n" a)))
  result)

(define (equal-iattrs? as bs)
  (and (= (length as) (length bs))
       ;; assumes attrs in same order
       (for/and ([aa (in-list as)]
                 [ba (in-list bs)])
         (and (bound-identifier=? (attr-name aa) (attr-name ba))
              (equal? (attr-depth aa) (attr-depth ba))
              (equal? (attr-syntax? aa) (attr-syntax? ba))))))

(define (expr-equal? a b)
  ;; Expression equality is undecidable in general. Especially difficult for unexpanded
  ;; code, but it would be very difficult to set up correct env for local-expand because of
  ;; attr binding rules. So, do *very* conservative approx: simple variables and literals.
  ;; FIXME: any other common cases?
  (cond [(not (and (syntax? a) (syntax? b)))
         (equal? a b)]
        [(and (identifier? a) (identifier? b))
         ;; note: "vars" might be identifier macros (unsafe to consider equal),
         ;; so check var has no compile-time binding
         (and (free-identifier=? a b)
              (let/ec k (syntax-local-value a (lambda () (k #t))) #f))]
        [(syntax-case (list a b) (quote)
           [((quote ad) (quote bd))
            (cons (syntax->datum #'ad) (syntax->datum #'bd))]
           [_ #f])
         => (lambda (ad+bd)
              (equal? (car ad+bd) (cdr ad+bd)))]
        [else
         ;; approx: equal? only if both simple data (bool, string, etc), no inner stx
         (let ([ad (syntax-e a)]
               [bd (syntax-e b)])
           (and (equal? ad bd)
                (free-identifier=? (datum->syntax a '#%datum) #'#%datum)
                (free-identifier=? (datum->syntax b '#%datum) #'#%datum)))]))

(define (equal-argu? a b)
  (define (unwrap-arguments x)
    (match x
      [(arguments pargs kws kwargs)
       (values pargs kws kwargs)]))
  (define (list-equal? as bs inner-equal?)
    (and (= (length as) (length bs))
         (andmap inner-equal? as bs)))
  (let-values ([(apargs akws akwargs) (unwrap-arguments a)]
               [(bpargs bkws bkwargs) (unwrap-arguments b)])
    (and (list-equal? apargs bpargs expr-equal?)
         (equal? akws bkws)
         (list-equal? akwargs bkwargs expr-equal?))))

(define (free-id/f-equal? a b)
  (or (and (eq? a #f)
           (eq? b #f))
      (and (identifier? a)
           (identifier? b)
           (free-identifier=? a b))))

(define (bound-id/f-equal? a b)
  (or (and (eq? a #f)
           (eq? b #f))
      (and (identifier? a)
           (identifier? b)
           (bound-identifier=? a b))))

(define (make-and-pattern subs)
  (cond [(null? subs) (pat:any)] ;; shouldn't happen
        [(null? (cdr subs)) (car subs)]
        [else (pat:and subs)]))

(define (*append a b) (if (null? b) a (append a b)))

(define (stx-e x) (if (syntax? x) (syntax-e x) x))

;; ----

(define (matrix->sexpr rows)
  (cond [(null? rows) ;; shouldn't happen
         '(FAIL)]
        [(null? (cdr rows))
         (pk->sexpr (car rows))]
        [else
         (cons 'TRY (map pk->sexpr rows))]))
(define (pk->sexpr pk)
  (match pk
    [(pk1 pats k)
     (cons 'MATCH (map pattern->sexpr pats))]
    [(pk/same pat inner)
     (list 'SAME (pattern->sexpr pat) (matrix->sexpr inner))]
    [(pk/pair inner)
     (list 'PAIR (matrix->sexpr inner))]
    [(pk/and inner)
     (list 'AND (matrix->sexpr inner))]))
(define (pattern->sexpr p)
  (match p
    [(pat:any) '_]
    [(pat:integrated name pred desc _)
     (format-symbol "~a:~a" (or name '_) desc)]
    [(pat:svar name)
     (syntax-e name)]
    [(pat:var/p name parser _ _ _ _)
     (cond [(and parser (regexp-match #rx"^parse-(.*)$" (symbol->string (syntax-e parser))))
            => (lambda (m)
                 (format-symbol "~a:~a" (or name '_) (cadr m)))]
           [else
            (if name (syntax-e name) '_)])]
    [(? pat:literal?) `(quote ,(syntax->datum (pat:literal-id p)))]
    [(pat:datum datum) datum]
    [(? pat:action?) 'ACTION]
    [(pat:pair head tail)
     (cons (pattern->sexpr head) (pattern->sexpr tail))]
    [(pat:head head tail)
     (cons (pattern->sexpr head) (pattern->sexpr tail))]
    [(pat:dots (list eh) tail)
     (list* (pattern->sexpr eh) '... (pattern->sexpr tail))]
    [(ehpat _as hpat '#f _cn)
     (pattern->sexpr hpat)]
    [_ 'PATTERN]))
