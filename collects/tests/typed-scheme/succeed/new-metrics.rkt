#lang typed-scheme
(provide results run-all-tests)

(require (except-in scheme/list count) scheme/math scheme/path mzlib/match
         (prefix-in srfi13: srfi/13) scheme/file
         (for-syntax scheme/base))


(require/typed (prefix-in srfi48: srfi/48)
               [srfi48:format ( Port String String Any * -> Any)] )

(define-type-alias NumF (U Number #f))

(define-type-alias (Unit C) ((C ->  (Listof NumF)) -> (Path -> (Listof (U #f (Listof NumF))))))

;; ============================================================
;; CONFIG
(define COLLECTS-PATH (make-parameter (build-path "/home/samth/Desktop/collects-tmp/")))
(define PLANET-CODE-PATH (make-parameter (build-path "/home/samth/Desktop/most-recent-archives/")))

; collects-path : a path to the collects directory to compare
; planet-code-path : a path to the "other" code to compare (i.e. unpacked, most recent versions
;   of all planet packages)

;; ============================================================
;; STATS

(: t-test  ((Listof Number) (Listof Number) -> Number))
;; computes t value for the given sequences. t-tests measure
;; the extent to which difference in mean between two sets of
;; _interval-valued_ samples (e.g. distances, times, weights, counts ...)
;; can be explained by chance. Generally speaking, higher absolute
;; values of t correspond to higher confidence that an observed difference
;; in mean cannot be explained by chance.
(define (t-test seqA seqB)
  (manual-t-test
   (avg seqA) (avg seqB)
   (variance seqA) (variance seqB)
   (length seqA) (length seqB)))

(: manual-t-test (Number Number Number Number Number Number -> Number))
(define (manual-t-test avga avgb vara varb cta ctb)
  (/ (- avga avgb)
     (assert (sqrt (+ (/ vara cta) (/ varb ctb))) number?)))

(: chi-square  ((Listof Number) (Listof Number) -> Number))
;; chi-square is a simple measure of the extent to which the
;; difference in the frequency of 0's and 1's in the first
;; sequence and their frequency in the second sequence can
;; be explained by chance. higher numbers means higher confidence
;; that they cannot.
(define (chi-square seqA seqB)
  (with-handlers ([exn:fail? (λ (e) +nan.0)])
    (let* ([ct-a (length seqA)]
           [ct-b (length seqB)]
           [total-subjects (+ ct-a ct-b)]
           [a-hits (apply + seqA)]
           [b-hits (apply + seqB)] ;; these assume that the data is coded as 1 = present, 0 = not present
           [a-misses (- ct-a a-hits)]
           [b-misses (- ct-b b-hits)]
           [table
            `((,a-hits ,b-hits)
              (,a-misses ,b-misses))]
           [expected (λ: ([i : Integer] [j : Integer])
                         (/ (* (row-total i table) (col-total j table)) total-subjects))])
      (exact->inexact
       (table-sum
        (λ (i j) (/ (sqr (- (expected i j) (table-ref i j table))) (expected i j)))
        table)))))

;; ============================================================
;; UNITS OF MEASUREMENT IMPLEMENTATIONS

(: per-module (All (X) (((Listof Any) ->  X) -> (Path -> (List (U #f X))))))
(define (per-module f)
  (λ (path)
    (with-handlers ([exn:fail:read? (λ (e) (list #f))])
      (let ([initial-sexp (with-input-from-file path read)])
        (match initial-sexp
          [`(module ,_ ,_  ,bodies ...)
           (list (f bodies))]
          [_ (list #f)])))))

(: per-module-top-level-expression ((Any -> (Listof NumF)) -> MetricFn))
(define (per-module-top-level-expression f)
  (let ([calc (per-module (λ: ([exprs : (Listof Any)]) (map f exprs)))])
    (λ (p) (let ([r  (calc p)]) (if (car r) (car r) r)))))

;; ============================================================
;; BASIC CALCULATIONS
;; (for use with metric definitions below)

;; ----------------------------------------
;; depth

(: sexp-depth (Any -> Integer))
(define (sexp-depth sexp)
  (cond
    [(pair? sexp)
     (+ (max-sexp-depth sexp) 1)]
    [else 0]))

(: max-sexp-depth (Any -> Integer))
(define (max-sexp-depth losx)
  (improper-foldr (λ: ([t : Any] [r : Integer]) (max (sexp-depth t) r)) 0 losx))

(: avg-sexp-depth ((Listof Any) -> Number))
(define (avg-sexp-depth sexps)
  (cond
    [(null? sexps) 0]
    [else (avg (map sexp-depth sexps))]))

;; ----------------------------------------
;; setbang counts

(: count-setbangs/ilist (Any -> Number))
(define (count-setbangs/ilist exprs)
  (apply + (imap count-setbangs/expr exprs)))
(: count-setbangs/expr (Any -> Number))
(define (count-setbangs/expr expr)
  (match expr
    [`(,(? setbang?) ,rest ...) (+ 1 (count-setbangs/ilist rest))]
    [('quote _) 0]
    [('quasiquote _) 0] ; undercount potentially, but how many `,(set! ...)'s can there be?
    [`(,e1 . ,e2) (count-setbangs/ilist expr)]
    [_ 0]))

(: setbang?  (Any -> Any))
(define (setbang? v)
  (and (symbol? v)
       (regexp-match #rx"^set(-.*)?!" (symbol->string v))))

;; count-fns
(: count-fns-with-setbangs ((Listof Any) -> Number))
(define (count-fns-with-setbangs exprs)
  (apply + (map (λ (e) (if (= (count-setbangs/expr e) 0) 0 1)) exprs)))
(: module-has-setbangs? ((Listof Any) -> Boolean))
(define (module-has-setbangs? exprs) (ormap expr-uses-setbangs? exprs))
(: expr-uses-setbangs? (Any -> Boolean))
(define (expr-uses-setbangs? expr)
  (not (= (count-setbangs/expr expr) 0)))

(: setbangs-per-1000-atoms ((Listof Any) -> NumF))
(define (setbangs-per-1000-atoms exprs)
  (if (null? exprs)
      #f
      (let ([set!s (count-setbangs/ilist exprs)]
            [atoms (atoms exprs)])
        (* (/ set!s atoms) 1000.0))))

;; ----------------------------------------
;; contracts

(: uses-contracts ((Listof Any) -> Boolean))
(define (uses-contracts exprs)
  (ormap (λ (e)
           (ann
            (match e
              [`(provide/contract ,_ ...) #t]
              [_ #f])
            : Boolean))
         exprs))

(: contracted-provides ((Listof Any) -> Number))
(define (contracted-provides exprs)
  (foldl
   (λ: ([t : Any] [r : Number])
       (ann
        (match t
          [(provide/contract ,p ...) (+ (length p) r)]
          [_ r]) : Number))
   0
   exprs))

(: uncontracted-provides ((Listof Any) -> Number))
(define (uncontracted-provides exprs)
  (foldl
   (λ: ([t : Any] [r : Number])
       (ann
        (match t
          [`(provide ,p  ...) (+ (length p) r)]
          [_ r]) : Number))
   0
   exprs))

;; ----------------------------------------
;; macros

(: number-of-macro-definitions (Any -> Number))
(define (number-of-macro-definitions expr)
  (match expr
    [`(define-syntax ,_ ...) 1]
    [`(define-syntaxes (,s ...) ,_ ...) (length s)]
    [`(define-syntax-set (,s ...) ,_ ...) (length s)]
    [_ 0]))

(: num-of-define-syntax ((Listof Any) -> Number))
(define (num-of-define-syntax exprs)
  (foldl (λ: ([t : Any] [r : Number]) (+ (number-of-macro-definitions t) r)) 0 exprs))

;; ----------------------------------------
;; expression size

(: atoms (Any -> Integer))
(define (atoms sexp)
  (cond
    [(null? sexp) 0]
    [(not (pair? sexp)) 1]
    [else (+ (atoms (car sexp)) (atoms (cdr sexp)))]))

(: max-atoms ((Listof Any) -> NumF))
(define (max-atoms exprs)
  (let ([atom-counts (map atoms exprs)])
    (if (null? atom-counts)
        #f
        (apply max atom-counts))))

(: avg-atoms ((Listof Any) -> NumF))
(define (avg-atoms exprs)
  (let ([atom-counts (map atoms exprs)])
    (if (null? atom-counts)
        #f
        (avg (map atoms exprs)))))

(: total-atoms ((Listof Any) -> Number))
(define (total-atoms exprs)
  (apply + (map atoms exprs)))


;; ============================================================
;; METRIC DEFINITIONS
;; 'a 'b metric : (string * (listof sexp -> 'a option) * ((listof 'a) (listof 'a) -> 'b)
(define-typed-struct (b c d) metric ([analysis-unit : (Unit c)]
                                     [computation : (c -> d)]
                                     [>display : ((Listof d) (Listof d) -> b)]))

(define-type-alias Table (Listof (Listof Number)))
(define-type-alias Atom-display (cons Symbol (Listof Number)))

(: standard-display (Symbol ((Listof Number) -> Number) ((Listof Number) (Listof Number) -> Number)
                     -> ((Listof NumF) (Listof NumF) -> Atom-display)))
(define ((standard-display name summarize significance-test) seqA seqB)
    (let ([clean-seqA (nonfalses seqA)]
          [clean-seqB (nonfalses seqB)])
      (list name (summarize clean-seqA) (summarize clean-seqB) (significance-test clean-seqA clean-seqB))))

(: interval (All (c) ((Unit c) Symbol (c -> NumF) -> (metric Atom-display c NumF))))
(define (interval u name compute) (make-metric u compute (standard-display name avg t-test)))

(: count (All (c) ((Unit c) Symbol (c -> Boolean) -> (metric  Atom-display c NumF))))
(define (count u name compute) (make-metric u (λ: ([es : c]) (if (compute es) 1 0)) (standard-display name avg chi-square)))

(: combine-metrics (All (c) ((Listof (metric Atom-display c NumF)) -> (metric (Listof Atom-display) c (Listof NumF)))))
(define (combine-metrics ms)
  (let ([u (metric-analysis-unit (car ms))])
    ;; This test now redundant b/c of typechecking
    (unless (andmap (λ: ([m : (metric Atom-display c NumF) ]) (eq? u (metric-analysis-unit m))) ms)
      (error 'combine-metrics "all combined metrics must operate on the same unit of analysis"))

    (make-metric
     u
     (λ: ([exprs : c]) (map (λ: ([m : (metric Atom-display c NumF)]) ((metric-computation m) exprs)) ms))
     (λ: ([seqA : (Listof (Listof NumF))] [seqB : (Listof (Listof NumF))])
              (map (λ: ([m : (metric Atom-display c NumF)]
                             [sA : (Listof NumF)]
                             [sB : (Listof NumF)])
                            ((metric->display m) sA sB)) ms (pivot seqA) (pivot seqB))))))

;; FIXME - (filter (lambda (x) x) l)
(: nonfalses (All (X) ((Listof (U #f X)) -> (Listof X))))
(define (nonfalses l)
  (let loop ([lst l])
    (if (null? lst)
        '()
        (let ([x (car lst)])
          (if x
              (cons x (loop (cdr lst)))
              (loop (cdr lst)))))))

(: avg ((Listof Number) -> Number))
(define (avg l) (/ (exact->inexact (apply + l)) (length l)))
(: avg* ((Listof Number) -> Number))
(define (avg* l) (avg (nonfalses l)))

(define-syntax define-metrics
  (syntax-rules ()
    [(define-metrics all-metrics-id unit-of-analysis  (name kind fn) ...)
     (begin
       (define u unit-of-analysis)
       (define name (kind u 'name fn )) ...
       (define all-metrics-id (combine-metrics (list name ...))))]))

(define-metrics module-metrics #{per-module @ (Listof NumF)}
  (maximum-sexp-depth        interval  max-sexp-depth)
  (average-sexp-depth        interval  avg-sexp-depth)
  (number-of-setbangs/mod    interval  count-setbangs/ilist)
  (number-of-exprs           interval  #{length @ Any})
  (uses-setbang?/mod         count     module-has-setbangs?)
  (uses-contracts?           count     uses-contracts)
  (number-of-contracts       interval  contracted-provides)
  (num-uncontracted-provides interval  uncontracted-provides)
  (number-of-macro-defs      interval  num-of-define-syntax)
  (maximum-num-atoms         interval  max-atoms)
  (average-num-atoms         interval  avg-atoms)
  (total-num-atoms/mod       interval  total-atoms)
  (set!s-per-1000-atoms      interval  setbangs-per-1000-atoms))

(define-metrics tl-expr-metrics per-module-top-level-expression
  (uses-setbang?/fn          count     expr-uses-setbangs?)
  (number-of-setbangs/fn     interval  count-setbangs/expr)
  (total-num-atoms/fn        interval  atoms))

(: all-metrics (List (metric (Listof Atom-display) (Listof Any) (Listof NumF))
                     (metric (Listof Atom-display) Any (Listof NumF)) ))
(define all-metrics (list module-metrics tl-expr-metrics))

;; ============================================================
;; EXPERIMENT RUNNING

(define-syntax (define-excluder stx)

  (define (path->clause c)
    (syntax-case c ()
      [(item ...)
       #`[`(#,@(reverse (syntax-e #'(item ...))) ,_ (... ...)) #t]]
      [item
       #`[`(item) #t]]))

  (syntax-case stx ()
    [(_ name path ...)
     (with-syntax ([(match-clause ...) (map path->clause (syntax-e #'(path ...)))])
       #`(define (name p )
           (let* ([dirnames (map path->string (filter path? (explode-path p)))])
             (match (reverse dirnames) ; goofy backwards matching because ... matches greedily
               match-clause ...
               [_ #f]))))]))

(: default-excluder (Path -> Boolean))
(define-excluder default-excluder
  "compiled" ".svn" #;("collects" "drscheme") #;("collects" "framework"))

(define exclude-directory? (make-parameter default-excluder))

;; ----------------------------------------
;; apply-to-scheme-files: (path[file] -> X) path[directory] -> (listof X)
;; applies the given function to each .rkt or .ss or .scm file in the given
;; directory hierarchy; returns all results in a list
(: apply-to-scheme-files (All (X) ((Path -> X) Path -> (Listof X))))
(define  (apply-to-scheme-files f root)
  (fold-files
   (λ: ([path : Path] [kind : (U 'file 'dir 'link)] [acc : (Listof X)])
     (case kind
       [(file)
        (let ([extension (filename-extension path)])
          (cond
            [(not extension) (values acc #t)]
            [(regexp-match #rx"(ss|scm)$" extension)
             (let ([resl (f path)])
               (if resl
                   (values (cons resl acc) #t)
                   (values acc #t)))]
            [else (values acc #t)]))]
       [(dir)
        (let* ([p (normalize-path path root)])
          (if ((exclude-directory?) p)
              (values acc #f)
              (values acc #t)))]
       [(link) (values acc #t)]))
   '()
   root))
(define-typed-struct (a b c) result ([metric : (metric b c a)] [seqA : (Listof a)] [seqB : (Listof a)]))
(define-type-alias MetricFn (Path -> (Listof (U #f (Listof NumF)))))

(define-type-alias (M b c) (metric b c (Listof NumF)))
(define-type-alias (M2 b c c*) (U (M b c) (M b c*)))

;; get-sequences : (listof 'a metric) path -> (listof (listof 'a))
(: get-sequences (All (b c C) ((List (M b c) (M b C)) Path -> (Listof (Listof (Listof NumF))))))
(define (get-sequences metrics path)
  (: selector (case-lambda [(M b c) -> MetricFn] [(M b C) -> MetricFn]))
  (define (selector m) ((metric-analysis-unit m) (metric-computation m)))
  (let* ([metric-fns (map #{selector :: ((M2 b c C) -> MetricFn)} metrics)]
         [result-seqs (apply-to-scheme-files
                       (λ: ([file : Path])
                           (map (λ: ([fn : MetricFn]) (fn file)) metric-fns)) path)])
    (map
     (λ: ([l : (Listof (Listof (U #f (Listof NumF))))])
         (nonfalses (apply append l)))
     (pivot (nonfalses result-seqs)))))


(: compare*
   (All (b c c*)
        ((List (M b c) (M b c*))
         ->
         (List (result (Listof NumF) b c)
               (result (Listof NumF) b c*)))))
(define (compare* metrics)
  (let* ([seqAs (get-sequences metrics (COLLECTS-PATH))]
         [seqBs (get-sequences metrics (PLANET-CODE-PATH))])
    (list
     (make-result (car metrics) (car seqAs) (car seqBs))
     (make-result (cadr metrics) (cadr seqAs) (cadr seqBs)))))

(: show (All (a b c) ((result a b c) -> b)))
(define (show result)
  ((metric->display (result-metric result))
   (result-seqA result)
   (result-seqB result)))

(: pretty-print-result
   (case-lambda
     ((result (Listof NumF) (Listof Atom-display) (Listof Any)) -> Void)
     ((result (Listof NumF) (Listof Atom-display) Any) -> Void)))
(define (pretty-print-result result)
  (for-each
   (λ: ([l : (Listof Any)])
            (apply srfi48:format
                   (current-output-port)
                   "~26F  | ~8,2F  | ~6,2F  | ~12,2F\n"
                   (format "~a" (car l))
                   (cdr l)))
   (list* '("test name" "collects" "planet" "significance")
          '("---------" "--------" "------" "------------")
          (show result))))

;; applies only to the combined metric [or more generally to listof-answer results]
(: total (All (b c) (Integer (result (Listof Number) b c) -> (Listof Number))))
(define (total experiment-number result)
  (: total/s (Table -> Number))
  (define (total/s s) (apply + (list-ref (pivot s) experiment-number)))
  (list (total/s (result-seqA result)) (total/s (result-seqB result))))

;; ============================================================
;; UTILITY

(: imap (All (Y) ((Any -> Y) Any -> (Listof Y))))
(define (imap f il)
  (cond
    [(null? il) '()]
    [(not (pair? il)) (list (f il))]
    [else (cons (f (car il)) (imap f (cdr il)))]))

(: pivot (All (X) ((Listof (Listof X)) -> (Listof (Listof X)))))
(define (pivot l)
  (cond
    [(null? l) '()]
    [else
     (let ([n (length (car l))])
       (build-list n (λ: ([i : Integer]) (map (λ: ([j : (Listof X)]) (list-ref j i)) l))))]))

(: variance ((Listof Number) -> Number))
(define (variance xs)
  (let ([avg (/ (apply + xs) (length xs))])
    (/ (apply + (map (λ: ([x : Number]) (sqr (- x avg))) xs))
       (sub1 (length xs)))))

(: table-ref (Integer Integer Table -> Number))
(define (table-ref i j table)
  (list-ref (list-ref table i) j))
(: row-total (Integer Table -> Number))
(define (row-total i table)
  (apply + (list-ref table i)))
(: col-total (Integer Table -> Number))
(define (col-total j table)
  (apply + (map (λ: ([x : (Listof Number)]) (list-ref x j)) table)))
(: table-sum ((Integer Integer -> Number) Table -> Number))
(define (table-sum f table)
  (let ([rows (length table)]
        [cols (length (car table))])
    (let loop  ([i 0] [j 0] [#{sum : Number} 0])
      (cond
        [(>= j cols) sum]
        [(>= i rows) (loop 0 (add1 j) sum)]
        [else        (loop (add1 i) j (+ sum (f i j)))]))))

(: improper-foldr (All (Y) ((Any Y -> Y) Y Any -> Y)))
(define (improper-foldr f b l)
  (cond
    [(null? l) b]
    [(not (pair? l))
     (f l b)]
    [else
     (f (car l) (improper-foldr f b (cdr l)))]))

(: /* (All (a ...) ((Listof Number) (Listof Number) ... a -> (Listof Number))))
(define (/* arg . args)
    (apply map (λ: ([n : Number] . [ns : Number ... a]) (apply / n ns)) arg args))


;; ============================================================
;; MAIN ENTRY POINT

(: results (U #f (Listof (U (result (Listof NumF) (Listof Atom-display) (Listof Any))
                            (result (Listof NumF) (Listof Atom-display) Any)))))
(define results #f) ; just in case i want to do some more analysis on the results afterwards,
; so i don't have to waste a minute if i forget to bind the return value to something

(define (run-all-tests)
  (let ([rs (compare* all-metrics)])
        (set! results rs)
    (for-each
     (ann pretty-print-result ((U (result (Listof NumF) (Listof Atom-display) (Listof Any))
                                  (result (Listof NumF) (Listof Atom-display) Any))
                               -> Any))
     rs)
    rs))

