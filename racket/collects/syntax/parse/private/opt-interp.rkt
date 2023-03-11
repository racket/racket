#lang racket/base
(require racket/syntax
         racket/list
         racket/pretty
         (submod "residual.rkt" ct)
         "minimatch.rkt"
         "rep-patterns.rkt"
         "const-expr.rkt"
         "kws.rkt")
(provide (all-defined-out))

;; ============================================================
;; Optimizer

;; The goal of the optimizer is to factor out common patterns at the beginning
;; of multiple disjuncts in a list of clauses or syntax class patterns.

;; Can factor pattern P given clauses like
;;   [ P P1 ...]            [P1 ... ]
;;   [ P  :    ]  => same P [ :     ]
;;   [ P PN ...]            [PN ... ]
;; if P cannot cut and P succeeds at most once (otherwise may reorder backtracking)

;; Can unfold pair patterns as follows:
;;   [ (P11 . P12) P1 ... ]         [P11 P12 P1 ... ]
;;   [      :       :     ] => pair [      :        ]
;;   [ (PN1 . PN2) PN ... ]         [PN1 PN2 PN ... ]

;; Can unfold ~and patterns similarly; ~and patterns can hide
;; factoring opportunities.

;; A Matrix1 is a (NEListof row1).

;; A Matrix is a (NEListof Row) where each Row has same number of columns
;; A Row is one of
;;  - (row1 AEnv (Listof Pattern) K)  -- a simple row in a parsing matrix
;;  - (row/same Pattern Matrix)       -- common first column factored out
;;  - (row/pair (Listof FirstDesc) Matrix)  -- pair patterns in the first column unfolded
;;  - (row/and Matrix)                -- and patterns in the first column unfolded
;; where K = (REnv -> REnv)
(struct row1 (aenv patterns k) #:prefab)
(struct row/same (pattern inner) #:prefab)
(struct row/pair (first-descs inner) #:prefab)
(struct row/and (inner) #:prefab)

;; Since interpreting a matrix is more expensive than interpreting a pattern,
;; only keep the optimized version if it appears worth it. The matrix-opt-score
;; function attempts to measure "worth it".

;; matrix-has-opt? : Matrix -> Boolean
(define (matrix-has-opt? rows)
  (define (row-has-opt? row)
    (match row
      [(? row1?) #f]
      [(? row/same?) #t]
      [(row/pair _ inner) (matrix-has-opt? inner)]
      [(row/and inner) (matrix-has-opt? inner)]))
  (ormap row-has-opt? rows))

;; matrix-keep? : Matrix -> Boolean
(define (matrix-keep? rows [threshold 0])
  (>= (matrix-opt-score rows) threshold))

;; matrix-opt-score : Matrix -> Real
;; Measure of benefit of optimized matrix minus penalties for increased
;; complexity.
(define (matrix-opt-score rows)
  (define (row-opt-score row)
    (match row
      [(? row1?) 0]
      [(row/same p inner)
       (+ (pattern-opt-score p (sub1 (length inner)))
          (matrix-opt-score inner))]
      [(row/pair _ inner)
       (+ -2 (matrix-opt-score inner))]
      [(row/and inner)
       (+ -2 (matrix-opt-score inner))]))
  (+ 0 (for/sum ([row (in-list rows)]) (row-opt-score row))))

;; pattern-opt-score : *Pattern Nat -> Real
;; Measure of benefit of saving n copies (ie, n+1 down to 1) of p.
(define (pattern-opt-score p n)
  (define (loop p [n n]) (pattern-opt-score p n))
  (match p
    [(? pat:var/p?) (* n 8)]
    [(? pat:integrated?) (* n 2)]
    [(? pat:literal?) (* n 3)]
    [(? pat:datum?) (* n 2)]
    [(pat:dots ehs tailp)
     (+ (for/sum ([eh (in-list ehs)]) (loop eh (* 2 n)))
        (loop tailp n))]
    [(pat:pair headp tailp) (+ 1 (loop headp) (loop tailp))]
    [(pat:describe p _ _ _) (+ 1 (loop p))]
    [(pat:commit p) (+ 1 (loop p))]
    [(pat:delimit p) (+ 1 (loop p))]
    [(pat:ord p _ _) (+ 1 (loop p))]
    [(pat:post p) (+ 1 (loop p))]
    [(? hpat:var/p?) (* n 10)]
    [(hpat:single sp) (loop sp)]
    [(ehpat _ hp _ _) (loop hp)]
    [_ (* n 1)]))

;; ----------------------------------------

;; make-optimizer : (Pattern -> String/#f)
;;               -> (Matrix1 -> Matrix/#f)
(define (make-optimizer first-desc-s)

  ;; optimize-matrix0 : Syntax/#f Matrix1 -> (U Matrix #f)
  (define (optimize-matrix0 ctx rows)
    (define MIN-SCORE 2)
    (cond [(> (length rows) 1)
           (define now (current-inexact-monotonic-milliseconds))
           (log-syntax-parse-info "OPT matrix (~s rows)~a\n~a" (length rows)
                                  (if (syntax? ctx)
                                      (let* ([src (syntax-source ctx)]
                                             [src (if (path? src) (path->string src) src)])
                                        (format " loc=~s:~s" src (syntax-line ctx)))
                                      "")
                                  (pretty-format (matrix->sexpr rows) #:mode 'print))
           (define result (optimize-matrix rows))
           (define then (current-inexact-monotonic-milliseconds))
           (cond [result
                  (define score (matrix-opt-score result))
                  (log-syntax-parse-info "OPT ==> score:~s (~s ms)\n~a" score (floor (- then now))
                                         (pretty-format (matrix->sexpr result) #:mode 'print))
                  (and (>= score MIN-SCORE) result)]
                 [else
                  (log-syntax-parse-info "OPT FAILED (~s ms)" (floor (- then now)))
                  #f])]
          [else #f]))

  ;; optimize-matrix : Matrix1 -> Matrix/#f
  ;; Note: this repeated checking of matrix-better? is quadratic, but only
  ;; in the depth of the optimizations performed.
  (define (optimize-matrix rows)
    (define result (optimize-matrix/1 rows))
    (and (matrix-keep? result) result))

  ;; optimize-matrix/1 : Matrix1 -> Matrix
  ;; The matrix is nonempty. Unfold pat:and patterns in the first column,
  ;; if any, then continue to optimize-matrix/2.
  (define (optimize-matrix/1 rows)
    (unless (pair? rows) (error 'optimize-matrix/1 "empty matrix"))
    (cond [(null? (cdr rows)) rows] ;; no opportunities for 1 row
          [(null? (row1-patterns (car rows))) rows]
          [else
           ;; first unfold and-patterns
           (define-values (col1 col2)
             (for/lists (col1 col2) ([row (in-list rows)])
               (unfold-and (car (row1-patterns row)))))
           (cond [(ormap pair? col2)
                  (list
                   (row/and
                    (optimize-matrix/2
                     (for/list ([row (in-list rows)]
                                [col1 (in-list col1)]
                                [col2 (in-list col2)])
                       (match row
                         [(row1 aenv (cons _ ps) k)
                          (row1 aenv (list* col1 (make-and-pattern col2) ps) k)])))))]
                 [else (optimize-matrix/2 rows)])]))

  ;; optimize-matrix/2 : Matrix1 -> Matrix
  ;; The matrix is nonempty, and first column has no unfoldable pat:and.
  ;; Split into submatrixes (sequences of rows) starting with similar patterns,
  ;; handle according to similarity, then recursively optimize submatrixes.
  (define (optimize-matrix/2 rows)
    (define row1 (car rows))
    (define pat1 (car (row1-patterns row1)))
    ;; Now accumulate rows starting with patterns like pat1
    (define-values (like? combine) (pattern->partitioner pat1))
    (let loop ([rows (cdr rows)] [rrows (list row1)])
      (cond [(null? rows)
             (combine (reverse rrows))]
            [else
             (define row (car rows))
             (define pat (car (row1-patterns row)))
             (cond [(like? pat)
                    (loop (cdr rows) (cons row rrows))]
                   [else
                    (append (combine (reverse rrows))
                            (optimize-matrix/2 rows))])])))

  ;; pattern->partitioner : Pattern -> (values (Pattern -> Boolean) (Matrix1 -> Matrix))
  (define (pattern->partitioner pat1)
    (match pat1
      [(pat:pair head tail)
       (values (lambda (p) (pat:pair? p))
               (lambda (rows)
                 (log-syntax-parse-debug "-- got ~s pair rows like ~e"
                                         (length rows) (pattern->sexpr pat1))
                 (cond [(> (length rows) 1)
                        (define inner-rows
                          (for/list ([row (in-list rows)])
                            (match row
                              [(row1 aenv (cons (pat:pair headp tailp) ps) k)
                               (row1 aenv (list* headp tailp ps) k)])))
                        (list (row/pair
                               (remove-duplicates
                                (for/list ([row (in-list inner-rows)])
                                  (let ([p (car (row1-patterns row))])
                                    (first-desc-s p))))
                               (or (optimize-matrix inner-rows) inner-rows)))]
                       [else rows])))]
      [(? pattern-factorable?)
       (values (lambda (pat2) (pattern-equal? pat1 pat2))
               (lambda (rows)
                 (log-syntax-parse-debug "-- got ~s factorable like ~e"
                                         (length rows) (pattern->sexpr pat1))
                 (cond [(> (length rows) 1)
                        (define inner-rows
                          (for/list ([row (in-list rows)])
                            (match row
                              [(row1 aenv (cons p ps) k)
                               (let ([aenv (append (reverse (pattern-attrs p #t)) aenv)])
                                 (row1 aenv ps k))])))
                        (list (row/same pat1 (or (optimize-matrix inner-rows) inner-rows)))]
                       [else rows])))]
      [_
       (values (lambda (pat2) #f)
               (lambda (rows) rows))]))

  optimize-matrix0)

;; unfold-and : Pattern (Listof Pattern) -> (values Pattern (Listof Pattern))
(define (unfold-and p [onto null])
  (match p
    [(pat:and (cons p ps))
     (unfold-and p (*append ps onto))]
    [_ (values p onto)]))

;; pattern-factorable? : *Pattern -> Boolean
(define (pattern-factorable? p)
  ;; Cannot factor out p if
  ;; - p can succeed multiple times (factoring changes success order)
  ;; - p can cut (factoring changes which choice points are discarded (too few))
  ;; Note: presence of sub-expressions handled by pattern-equal?.
  (let loop ([p p])
    (match p
      [(? pat:any?) #t]
      [(? pat:svar?) #t]
      [(pat:var/p _ _ _ _ _ (scopts _ commit? _ _)) commit?]
      [(? pat:literal?) #t]
      [(? pat:datum?) #t]
      [(? pat:action?) #f]
      [(pat:head hp sp) (and (loop hp) (loop sp))]
      [(pat:dots heads tail)
       ;; Conservative approximation: (hp ... . ()) or (hp ...+ . ())
       (and (= (length heads) 1)
            (equal? tail (pat:datum '()))
            (andmap loop heads))]
      [(pat:and ps) (andmap loop ps)]
      [(? pat:or?) #f]
      [(? pat:not?) #t]
      [(pat:vector sp) (loop sp)]
      [(pat:box sp) (loop sp)]
      [(pat:pstruct key sp) (loop sp)]
      [(pat:describe sp _ _ _) (loop sp)]
      [(pat:delimit sp) (loop sp)]
      [(? pat:commit?) #t]
      [(? pat:reflect?) #f]
      [(pat:post sp) (loop sp)]
      [(? pat:integrated?) #t]
      [(? pat:seq-end?) #t]
      ;; ----
      [(hpat:single sp) (loop sp)]
      [(hpat:var/p _ _ _ _ _ (scopts _ commit? _ _)) commit?]
      [(hpat:seq lp) (loop lp)]
      [(? hpat:action?) #f]
      [(hpat:and hp sp) (and (loop hp) (loop sp))]
      [(? hpat:or?) #f]
      [(hpat:describe hp _ _ _) (loop hp)]
      [(hpat:delimit hp) (loop hp)]
      [(hpat:commit hp) (loop hp)]
      [(? hpat:reflect?) #f]
      [(hpat:post hp) (loop hp)]
      [(hpat:peek hp) #f]
      [(hpat:peek-not hp) #f]
      ;; ----
      [(ehpat _ hp repc _)
       (and (loop hp) (or (equal? repc #f) (equal? repc repc:plus)))]
      ;; ----
      [_ #f])))

(define-syntax-rule (match* [e ...] [[pat ...] rhs ...] ...)
  (match (list* e ...) [(list* pat ...) rhs ...] ...))

;; pattern-equal? : *Pattern *Pattern -> Boolean
(define (pattern-equal? a b)
  (define-syntax-rule (ignore e) #t)
  (define result
    (match* [a b]
      [[(? pat:any?) (? pat:any?)] #t]
      [[(pat:svar aname) (pat:svar bname)] #t]
      [[(pat:var/p aname aparser aargu anested arole aopts)
        (pat:var/p bname bparser bargu bnested brole bopts)]
       (and (free-id/f-equal? aparser bparser)
            (equal-presence? aname bname)
            (= (length anested) (length bnested))
            (equal-argu? aargu bargu)
            (expr-equal? arole brole))]
      [[(pat:integrated aname apred adesc arole)
        (pat:integrated bname bpred bdesc brole)]
       (and (equal-presence? aname bname)
            (free-identifier=? apred bpred)
            (expr-equal? arole brole))]
      [[(pat:literal aid _ _) (pat:literal bid _ _)]
       ;; literals are hard to compare, so compare gensyms attached to
       ;; literal ids (see rep.rkt) instead
       (let ([ka (syntax-property aid 'literal)]
             [kb (syntax-property bid 'literal)])
         (and ka kb (eq? ka kb)))]
      [[(pat:datum av) (pat:datum bv)]
       (equal? av bv)]
      [[(pat:head ahead atail) (pat:head bhead btail)]
       (and (pattern-equal? ahead bhead)
            (pattern-equal? atail btail))]
      [[(pat:dots aheads atail) (pat:dots bheads btail)]
       (and (subpatterns-equal? aheads bheads)
            (pattern-equal? atail btail))]
      [[(pat:and aps) (pat:and bps)]
       (subpatterns-equal? aps bps)]
      [[(pat:or _ aps _) (pat:or _ bps _)]
       (subpatterns-equal? aps bps)]
      [[(pat:not ap) (pat:not bp)]
       (pattern-equal? ap bp)]
      [[(pat:pair ahead atail) (pat:pair bhead btail)]
       (and (pattern-equal? ahead bhead)
            (pattern-equal? atail btail))]
      [[(pat:vector ap) (pat:vector bp)]
       (pattern-equal? ap bp)]
      [[(pat:box ap) (pat:box bp)]
       (pattern-equal? ap bp)]
      [[(pat:pstruct akey ap) (pat:pstruct bkey bp)]
       (and (equal? akey bkey)
            (pattern-equal? ap bp))]
      [[(pat:describe ap adesc atransp? arole)
        (pat:describe bp bdesc btransp? brole)]
       (and (pattern-equal? ap bp)
            (expr-equal? adesc bdesc)
            (equal? atransp? btransp?)
            (expr-equal? arole brole))]
      [[(pat:delimit ap) (pat:delimit bp)]
       (pattern-equal? ap bp)]
      [[(pat:commit ap) (pat:commit bp)]
       (pattern-equal? ap bp)]
      [[(pat:ord ap agroup aindex) (pat:ord bp bgroup bindex)]
       (and (pattern-equal? ap bp)
            (equal? agroup bgroup)
            (equal? aindex bindex))]
      [[(pat:post ap) (pat:post bp)]
       (pattern-equal? ap bp)]
      [[(pat:seq-end) (pat:seq-end)]
       #t]
      ;; ----
      [[(hpat:single ap) (hpat:single bp)]
       (pattern-equal? ap bp)]
      [[(hpat:var/p aname aparser aargu anested arole aopts)
        (hpat:var/p bname bparser bargu bnested brole bopts)]
       (and (free-id/f-equal? aparser bparser)
            (equal-presence? aname bname)
            (= (length anested) (length bnested))
            (equal-argu? aargu bargu)
            (expr-equal? arole brole))]
      [[(hpat:seq ap) (hpat:seq bp)]
       (pattern-equal? ap bp)]
      ;; FIXME: more variants?
      ;; ----
      [[(ehpat aattrs ahead arepc _) (ehpat battrs bhead brepc _)]
       (and (eq? arepc brepc)
            (pattern-equal? ahead bhead))]
      ;; ----
      [[_ _] #f]))
  (when (and (eq? result #f)
             (log-level? syntax-parse-logger 'debug)
             (equal? (syntax->datum #`#,a) (syntax->datum #`#,b)))
    (log-syntax-parse-debug "** pattern-equal? failed on ~e" a))
  result)

(define (subpatterns-equal? as bs)
  (and (= (length as) (length bs))
       (for/and ([a (in-list as)]
                 [b (in-list bs)])
         (pattern-equal? a b))))

(define (equal-presence? a b)
  (or (and a b) (and (not a) (not b))))

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
  (cond [(eq? a b) #t]
        [(and (datum-expr? a) (datum-expr? b))
         (equal? (datum-expr-value a) (datum-expr-value b))]
        [(and (identifier? a) (identifier? b))
         ;; note: "vars" might be identifier macros (unsafe to consider equal),
         ;; so check var has no compile-time binding
         (and (free-identifier=? a b)
              (let/ec k (syntax-local-value a (lambda () (k #t))) #f))]
        [else #f]))

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

(define (make-and-pattern subs)
  (cond [(null? subs) (pat:any)]
        [(null? (cdr subs)) (car subs)]
        [else (pat:and subs)]))

(define (*append a b) (if (null? b) a (append a b)))

(define (stx-e x) (if (syntax? x) (syntax-e x) x))

;; ----

(define (matrix->sexpr rows)
  (cond [(null? rows) ;; shouldn't happen
         '(FAIL)]
        [(null? (cdr rows))
         (row->sexpr (car rows))]
        [else
         (cons 'TRY (map row->sexpr rows))]))
(define (row->sexpr row)
  (match row
    [(row1 aenv pats k)
     (cons 'MATCH (map pattern->sexpr pats))]
    [(row/same pat inner)
     (list 'SAME (pattern->sexpr pat) (matrix->sexpr inner))]
    [(row/pair _ inner)
     (list 'PAIR (matrix->sexpr inner))]
    [(row/and inner)
     (list 'AND (matrix->sexpr inner))]))
(define (pattern->sexpr p)
  (match p
    [(pat:any) '_]
    [(pat:integrated name pred desc _)
     (format-symbol "~a:~a" (or name '_) desc)]
    [(pat:svar name)
     (syntax-e name)]
    [(pat:var/p name parser _ _ _ _)
     (cond [(and parser (regexp-match #rx"^parse-(.*?)[0-9]*$" (symbol->string (syntax-e parser))))
            => (lambda (m)
                 (format-symbol "~a:~a" (or name '_) (cadr m)))]
           [else
            (if name (syntax-e name) '_)])]
    [(? pat:literal?) `(syntax ,(syntax->datum (pat:literal-id p)))]
    [(pat:datum datum)
     (cond [(or (symbol? datum) (pair? datum))
            `(quote ,datum)]
           [else datum])]
    [(pat:action action (pat:any)) (pattern->sexpr action)]
    [(pat:action action inner) (list '~AAND (pattern->sexpr action) (pattern->sexpr inner))]
    [(pat:and patterns) (cons '~and (map pattern->sexpr patterns))]
    [(pat:or _ patterns _) (cons '~or (map pattern->sexpr patterns))]
    [(pat:not pattern) (list '~not (pattern->sexpr pattern))]
    [(pat:pair head tail)
     (cons (pattern->sexpr head) (pattern->sexpr tail))]
    [(pat:head head tail)
     (cons (pattern->sexpr head) (pattern->sexpr tail))]
    [(pat:dots (list eh) tail)
     (list* (pattern->sexpr eh) '... (pattern->sexpr tail))]
    [(pat:dots ehs tail)
     (list* (cons '~alt (map pattern->sexpr ehs)) '... (pattern->sexpr tail))]
    [(pat:describe sp _ _ _) (list '~describe (pattern->sexpr sp))]
    [(pat:delimit sp) (list '~delimit-cut (pattern->sexpr sp))]
    [(pat:commit sp) (list '~commit (pattern->sexpr sp))]
    [(pat:ord pattern _ _) (list '~ord (pattern->sexpr pattern))]
    [(pat:post sp) (list '~post (pattern->sexpr sp))]
    [(pat:seq-end) '()]
    [(action:cut) '~!]
    [(action:fail cnd msg) (list '~fail)]
    [(action:bind attr expr) (list '~bind)]
    [(action:and as) (cons '~and (map pattern->sexpr as))]
    [(action:parse sp expr) (list '~parse (pattern->sexpr sp))]
    [(action:do stmts) (list '~do)]
    [(action:undo stmts) (list '~undo)]
    [(action:ord ap _ _) (list '~ord (pattern->sexpr ap))]
    [(action:post ap) (list '~post (pattern->sexpr ap))]
    [(hpat:single sp) (pattern->sexpr sp)]
    [(hpat:var/p name parser _ _ _ _)
     (cond [(and parser (regexp-match #rx"^parse-(.*?)[0-9]*$" (symbol->string (syntax-e parser))))
            => (lambda (m) (format-symbol "~a:~a" (or name '_) (cadr m)))]
           [else (if name (syntax-e name) '_)])]
    [(hpat:seq lp) (cons '~seq (pattern->sexpr lp))]
    [(hpat:action ap hp) (list '~AAND (pattern->sexpr ap) (pattern->sexpr hp))]
    [(hpat:and hp sp) (list '~and (pattern->sexpr hp) (pattern->sexpr sp))]
    [(hpat:or _ hps _) (cons '~or (map pattern->sexpr hps))]
    [(hpat:describe hp _ _ _) (list '~describe (pattern->sexpr hp))]
    [(hpat:delimit hp) (list '~delimit-cut (pattern->sexpr hp))]
    [(hpat:commit hp) (list '~commit (pattern->sexpr hp))]
    [(hpat:ord hp _ _) (list '~ord (pattern->sexpr hp))]
    [(hpat:post hp) (list '~post (pattern->sexpr hp))]
    [(hpat:peek hp) (list '~peek (pattern->sexpr hp))]
    [(hpat:peek-not hp) (list '~peek-not (pattern->sexpr hp))]
    [(ehpat _as hpat repc _cn)
     (if (eq? repc #f) (pattern->sexpr hpat) (list '~REPC (pattern->sexpr hpat)))]
    [_ '<Pattern>]))
