#lang typed-scheme
(provide results run-all-tests)

#;(require "../list.scm"
         "../etc.rkt")
(require/typed "foldo.rkt"
  (apply-to-scheme-files
                ((Path -> (Listof (Listof (U #f (Listof (U Real #f))))))
                 Path
                 -> (Listof (U #f (Listof  (Listof ( U #f (Listof (U Real #f))))))))))

(define-type-alias top Any)
(define-type-alias str String)

(require/typed mzlib/file
  [filename-extension (Path -> (U #f Bytes))]
  [normalize-path (Path Path -> Path)]
  [explode-path (Path -> (Listof Path))])
(require/typed "patch.rkt" [srfi48::format (Port String String top * -> top)])
;; FIXME - prefix
#;(require/typed srfi48:format ( Port String String top * -> top) (prefix-in srfi48: (lib "48.rkt" "srfi")))
(require (lib "match.rkt")
         ;(lib "file.rkt")
         ;(lib "list.rkt")
         ;(lib "etc.rkt")
         (prefix-in srfi13: (lib "13.rkt" "srfi"))
         ;(prefix srfi48: (lib "48.rkt" "srfi"))
         )

(define-type-alias Sexpr Any)
(define-type-alias number Real)
(define-type-alias boolean Boolean)
(define-type-alias NumF (U number #f))
(define-type-alias NumFs (Listof NumF))
(define-type-alias NumFss (Listof NumFs))
(define-type-alias NumB (U boolean number))
;;C is either Sexpr or Listof Sepr
;;X = (Listof (U number #f)) - not needed as a parameter
(define-type-alias (Unit X C) ((C ->  X) -> (Path -> (Listof (U #f X)))))

;; ============================================================
;; CONFIG
(define: COLLECTS-PATH : (Parameter Path Path ) (make-parameter (build-path "/proj/scheme/plt/collects" #;"~/svn/plt/collects/")))
(define: PLANET-CODE-PATH : (Parameter Path Path) (make-parameter (build-path "~/Desktop/most-recent-archives/" #;"~/local/src/planet/most-recent-archives/")))

; collects-path : a path to the collects directory to compare
; planet-code-path : a path to the "other" code to compare (i.e. unpacked, most recent versions
;   of all planet packages)

;; ============================================================
;; STATS

;; t-test : (listof number) (listof number) -> number
;; computes t value for the given sequences. t-tests measure
;; the extent to which difference in mean between two sets of
;; _interval-valued_ samples (e.g. distances, times, weights, counts ...)
;; can be explained by chance. Generally speaking, higher absolute
;; values of t correspond to higher confidence that an observed difference
;; in mean cannot be explained by chance.
(define: (t-test [seqA : (Listof Real)] [seqB : (Listof Real)]) : Real
  (manual-t-test
   (avg seqA) (avg seqB)
   (variance seqA) (variance seqB)
   (length seqA) (length seqB)))

(define: (manual-t-test [avga : number] [avgb : number] [vara : number]
                        [varb : number] [cta : number] [ctb : number]) : number
  (/ (- avga avgb)
     (assert (sqrt (+ (/ vara cta) (/ varb ctb))) real?)))

;; chi-square : (listof [0,1]) (listof [0,1]) -> number
;; chi-square is a simple measure of the extent to which the
;; difference in the frequency of 0's and 1's in the first
;; sequence and their frequency in the second sequence can
;; be explained by chance. higher numbers means higher confidence
;; that they cannot.
(define: (chi-square [seqA : (Listof number)] [seqB : (Listof number)]) :  number
  (with-handlers ([exn:fail? (lambda: ([e : exn]) +nan.0)])
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
           [expected (lambda: ([i : Integer] [j : Integer])
                              (/ (* (row-total i table) (col-total j table)) total-subjects))])
      (exact->inexact
       (table-sum
        (lambda: ([i : Integer] [j : Integer])
                 (/ (sqr (- (expected i j) (table-ref i j table))) (expected i j)))
        table)))))

;; ============================================================
;; UNITS OF MEASUREMENT IMPLEMENTATIONS

;; per-module : path ((listof expr) -> (number | #f)) -> (path -> (listof (number | #f)))  === Unit P
(pdefine: (X) (per-module [f : ((Listof Sexpr) ->  X )]) : (Path -> (cons (U #f X) '()))
          (lambda: ([path : Path])
                   (with-handlers ([exn:fail:read? (lambda: ([e : exn]) (list #f))])  ;; with handler
                     (let ([initial-sexp (with-input-from-file path read)])
                       (match initial-sexp
                         [`(module ,_ ,_  . , (? list? bodies)) ;; FIXME - use ... instead of .
                          (list (f bodies))]
                         [_ (list #f)])))))


;; per-module-top-level-expression : path (expr -> (number | #f)) -> (path -> (listof (number | #f)))
(define: (per-module-top-level-expression [f : (Sexpr -> (Listof NumF))] ) : ( Path -> (Listof (U #f (Listof NumF))))
  (let ([calc (per-module (lambda: ([exprs : (Listof Sexpr)]) (map f exprs)))])
    (lambda: ([p : Path]) (let* ([r  (calc p)]
                                 [carr (car r)]) ;;carr added
                            (if carr  carr
                                (list carr)))))) ;; list carr instead of r

;; ============================================================
;; BASIC CALCULATIONS
;; (for use with metric definitions below)

;; ----------------------------------------
;; depth

(define: (sexp-depth [sexp : Any]) : number
  (cond
    [(list? sexp) ;; (pair? sexp)
     (+ (max-sexp-depth sexp) 1)]
    [else 0]))

(define: (max-sexp-depth [losx : (Listof Any)]) : number
  (improper-foldr (lambda: ([t : Any] [r : number]) (max (sexp-depth t) r)) 0 losx))

(define: (avg-sexp-depth [sexps : (Listof Any)]) : number
  (cond
    [(null? sexps) 0]
    [else (avg (map sexp-depth sexps))]))

;; ----------------------------------------
;; setbang counts

(define-type-alias (IList e) (mu x (Un e '() (cons e x))))

;; count-setbangs/ilist : ((ilistof expr) -> number)
(define: (count-setbangs/ilist [exprs : (Listof Any)]) : number
  (apply + (imap count-setbangs/expr exprs)))

;; FIXME - changes having to do with match ...
(define: (count-setbangs/expr [expr : Any]) : number
  (match expr
    [`(,(? setbang?) . ,rest ) ;(,(? setbang?) ,rest ...)
     (if (list? rest)
         (+ 1 (count-setbangs/ilist rest))
         0)] ;; mostly occurs in syntax patterns
    [('quote _) 0]
    [('quasiquote _) 0] ; undercount potentially, but how many `,(set! ...)'s can there be?
    [`(,e1 . ,e2)
     (if (list? expr)
         (count-setbangs/ilist expr)
         (error " l" expr ))]   ;;FIXME - do something intelligent here
    [_ 0]))

;; setbang? : sexp -> boolean
(define: (setbang? [v : Any]) : Any
  (and (symbol? v)
       (regexp-match #rx"^set(-.*)?!" (symbol->string v))))

;; count-fns
(define: (count-fns-with-setbangs [exprs : (Listof Sexpr)]) : number
  (apply + (map (lambda: ([e : Sexpr]) (if (= (count-setbangs/expr e) 0) 0 1)) exprs)))
(define: (module-has-setbangs? [exprs : (Listof Sexpr)]) : Boolean
  (ormap expr-uses-setbangs? exprs))
(define: (expr-uses-setbangs? [expr : Sexpr]) : Boolean
  (not (= (count-setbangs/expr expr) 0)))

(define: (setbangs-per-1000-atoms [exprs : (Listof Any)]) : NumF
  (if (null? exprs)
      #f
      (let ([set!s (count-setbangs/ilist exprs)]
            [atoms (atoms exprs)])
        (* (/ set!s atoms) 1000.0))))

;; ----------------------------------------
;; contracts


(define: (uses-contracts [exprs : (Listof Sexpr)]) : Boolean
  (ormap (lambda: ([e : Sexpr])
                  (match e
                    [`(provide/contract . ,_) #t]
                    [_ #f]))
         exprs))

(define: (contracted-provides [exprs : (Listof Sexpr)]):  number
  (foldl
   (lambda: ([t : Sexpr] [r : number])
            (match t
              ;; FIXME match ...
              [`(provide/contract . ,p ) ;(provide/contract ,p ...)
               (if (list? p)
                   (+ (length p) r)
                   r)] ;; extra case added
              [_ r]))
   0
   exprs))

;; FIXME - same problem with match ...
(define: (uncontracted-provides [exprs : (Listof Sexpr)]) : number
  (foldl
   (lambda: ([t : Sexpr] [r : number])
            (match t
              [`(provide . ,p ) ;(provide ,p ...)
               (if (list? p)
                   (+ (length p) r)
                   r)]
              [_ r]))
   0
   exprs))

;; ----------------------------------------
;; macros

(define: (number-of-macro-definitions [expr : Sexpr]) : number
  (match expr
    [`(define-syntax ,_ ...) 1]
    [`(define-syntaxes (,s . ,r ). ,_ ) ;`(define-syntaxes (,s ...) ,_ ...)
     (if (and (list?  expr)(list? r))
         (length (cons s r));;s -> cadr expr
         (error "corrupted file"))]
    [`(define-syntax-set (,s . ,r) . ,_ ) ;(define-syntax-set (,s ...) ,_ ...)
     (if (and (list? expr) (list? r))
         (length (cons s r))
         (error "corrupted file"))]
    [_ 0]))

(define: (num-of-define-syntax [exprs : (Listof Sexpr)]) : number
  (foldl (lambda: ([t : Sexpr] [r : number]) (+ (number-of-macro-definitions t) r)) 0 exprs))

;; ----------------------------------------
;; expression size

(define: (atoms [sexp : Any]) : number
  (cond
    [(null? sexp) 0]
    [(not (pair? sexp)) 1]
    [else (+ (atoms (car sexp)) (atoms (cdr sexp)))]))

(define: (max-atoms [exprs : (Listof Sexpr)]) : NumF
  (let ([atom-counts (map atoms exprs)])
    (if (null? atom-counts)
        #f
        (apply max 0 atom-counts)))) ;; FIXME: expected at least 2 argument--->   0 added !!!

(define: (avg-atoms [exprs : (Listof Sexpr)]) : NumF
  (let ([atom-counts (map atoms exprs)])
    (if (null? atom-counts)
        #f
        (avg (map atoms exprs)))))

(define: (total-atoms [exprs : (Listof Sexpr)]) : number
  (apply + (map atoms exprs)))


;; ============================================================
;; METRIC DEFINITIONS
;; 'a 'b metric : (string * (listof sexp -> 'a option) * ((listof 'a) (listof 'a) -> 'b)
(define-typed-struct (b c d) metric ([analysis-unit : (Unit (Listof NumF) c)]
                                     [computation : (c -> d)]
                                     [>display : ((Listof d) (Listof d) -> b)]))
(define-type-alias Metric metric)
(define-type-alias Table (Listof (Listof Real)))
(define-type-alias Atom-display (cons Symbol (Listof Real)))

(define: (standard-display [name : Symbol]
                           [summarize : ((Listof number) -> number)]
                           [significance-test : ((Listof number)(Listof number) -> number)])
  : ((Listof NumF) (Listof NumF) -> Atom-display)
  ;; FIXME - use lambda instead of (define ((
  (lambda: ([seqA : (Listof NumF)] [seqB : (Listof NumF)])
           (let ([clean-seqA (nonfalses seqA)]
                 [clean-seqB (nonfalses seqB)])
             (list name (summarize clean-seqA) (summarize clean-seqB) (significance-test clean-seqA clean-seqB)))))

(pdefine: (c) (interval [u : (Unit (Listof NumF) c)]
                        [name : Symbol]
                        [compute : (c -> NumF)])
          : (Metric Atom-display c NumF)
          (make-metric u compute (standard-display name avg t-test)))

(pdefine: (c) (count [u : (Unit (Listof NumF) c)]
                     [name : Symbol]
                     [compute : (c -> Boolean)])
          : (Metric  Atom-display c NumF)
          (make-metric u (lambda: ([es : c]) #{(if (compute es) 1 0) :: NumF}) (standard-display name avg chi-square)))

(pdefine: (c) (combine-metrics [ms : (Listof (Metric Atom-display c NumF))])
          : (Metric (Listof Atom-display) c (Listof NumF))
          (let ([u (metric-analysis-unit (car ms))])
            ;; This test now redundant b/c of typechecking
            (unless (andmap (lambda: ([m : (Metric Atom-display c NumF) ]) (eq? u (metric-analysis-unit m))) ms)
              (error 'combine-metrics "all combined metrics must operate on the same unit of analysis"))

            (make-metric
             u
             (lambda: ([exprs : c] ) (map (lambda: ([m : (Metric Atom-display c NumF)]) ((metric-computation m) exprs)) ms))
             (lambda: ([seqA : (Listof (Listof NumF))] [seqB : (Listof (Listof NumF))])
                      (map (lambda: ([m : (Metric Atom-display c NumF)]
                                     [sA : (Listof NumF)]
                                     [sB : (Listof NumF)])
                                    ((metric->display m) sA sB)) ms (pivot seqA) (pivot seqB))))))

;; FIXME - should go in helper file
;; FIXME - (filter (lambda (x) x) l)
(pdefine: (X) (nonfalses [l : (Listof (U #f X))]) : (Listof X)
          (let: loop : (Listof X) ([lst :(Listof (U #f X)) l])
                (if (null? lst)
                    '()
                    (let ([x (car lst)])
                      (if x
                          (cons x (loop (cdr lst)))
                          (loop (cdr lst)))))))

(define: (avg [l : (Listof number)]) : number
  (/ (exact->inexact (apply + l)) (length l)))
(define: (avg* [l : (Listof number)]) : number
  (avg (nonfalses l)))

(require (for-syntax scheme/base))

(define-syntax (define-metrics stx)
  (syntax-case stx ()
    [(define-metrics all-metrics-id unit-of-analysis type (name kind fn) ...) ;;TYPE ADDED !!!!
     (with-syntax ([(kind-app ...) (for/list ([k (syntax->list #'(kind ...))]
                                              [n (syntax->list #'(name ...))]
                                              [f (syntax->list #'(fn ...))])
                                     (quasisyntax/loc k (#,k u '#,n #,f)))])
       (syntax/loc
           stx
         (begin
           (define: u : ((type -> (Listof NumF)) -> (Path -> (Listof (U #f(Listof NumF))))) unit-of-analysis )
           (define: name : (Metric Atom-display type NumF) kind-app) ...
           (define: all-metrics-id : (Metric (Listof Atom-display) type (Listof NumF)) (combine-metrics (list name ...))))))]))

(define-metrics module-metrics #{per-module @ (Listof NumF)} (Listof Sexpr)
  (maximum-sexp-depth        interval  max-sexp-depth)
  (average-sexp-depth        interval  avg-sexp-depth)
  (number-of-setbangs/mod    interval  count-setbangs/ilist)
  (number-of-exprs           #{interval @ (Listof Any)}  #{length @ Any})
  (uses-setbang?/mod         count     module-has-setbangs?)
  (uses-contracts?           count     uses-contracts)
  (number-of-contracts       interval  contracted-provides)
  (num-uncontracted-provides interval  uncontracted-provides)
  (number-of-macro-defs      interval  num-of-define-syntax)
  (maximum-num-atoms         interval  max-atoms)
  (average-num-atoms         interval  avg-atoms)
  (total-num-atoms/mod       interval  total-atoms)
  (set!s-per-1000-atoms      interval  setbangs-per-1000-atoms))

(define-metrics tl-expr-metrics per-module-top-level-expression  Sexpr
  (uses-setbang?/fn          count     expr-uses-setbangs?)
  (number-of-setbangs/fn     interval  count-setbangs/expr)
  (total-num-atoms/fn        interval  atoms))

(define: all-metrics : (Listof (U (Metric (Listof Atom-display) Sexpr (Listof NumF))
                                  (Metric (Listof Atom-display) (Listof Sexpr) (Listof NumF))))
  (list module-metrics tl-expr-metrics))

;; ============================================================
;; EXPERIMENT RUNNING

;; FIXME - everything in untyped file (foldo.rkt) b/c fold-files has terrible api
#;(define-syntax (define-excluder stx)

    (define (path->clause c)
      (syntax-case c ()
        [(item ...)
         #`[`(#,@(reverse (syntax-e #'(item ...))) ,_ (... ...)) #t]]
        [item
         #`[`(item) #t]]))

    (syntax-case stx ()
      [(_ name path ...)
       (with-syntax ([(match-clause ...) (map path->clause (syntax-e #'(path ...)))])
         #`(define: (name [p : Path]) : top
             (let* ([dirnames (map path->string (explode-path p))])
               (match (reverse dirnames) ; goofy backwards matching because ... matches greedily
                 match-clause ...
                 [_ #f]))))]))

#;(define-excluder default-excluder
    "compiled" ".svn" #;("collects" "drscheme") #;("collects" "framework"))

#;(define: exclude-directory? : (Parameter (Path -> Any)) (make-parameter default-excluder))

;; ----------------------------------------
;; apply-to-scheme-files: (path[file] -> X) path[directory] -> (listof X)
;; applies the given function to each .rkt or .ss or .scm file in the given
;; directory hierarchy; returns all results in a list
#;(define:  (apply-to-scheme-files [f : (Path -> (Listof(Listof(Listof NumF))))]
                                   [root : Path])
    : (Listof (Listof(Listof(Listof NumF)))) ;;FOLD-FILES

    (fold-files
     (lambda: ([path : Path] [kind : Symbol]
                             [acc : (Listof (Listof(Listof(Listof NumF))))])
              (case kind
                [(file)
                 (let ([extension (filename-extension path)])
                   (cond
                     [(not extension) #;acc (values acc #t)]
                     [(regexp-match #rx"(ss|scm)$" extension)
                      (let ([resl (f path)])
                        (if resl
                            #;(cons resl acc) (values (cons resl acc) #t) ;;values added
                            #;acc (values acc #t)))]
                     [else #;acc (values acc #t)]))]
                [(dir)
                 (let* ([p (normalize-path path root)])
                   (if ((exclude-directory?) p)
                       #; acc (values acc #f)
                       #;acc (values acc #t)))]         ;; values added
                [(link) #;acc (values acc #t)]
                [else (error "never happen")]))  ;;error added
     '()
     root
     #t)) ;;value added

(define-typed-struct (a b c) result ([metric : (Metric b c a)] [seqA : (Listof a)] [seqB : (Listof a)]))
(define-type-alias Result result)

;; get-sequences : (listof 'a metric) path -> (listof (listof 'a))

(pdefine: (b c) (get-sequences [metrics : (Listof (U (Metric  b c (Listof NumF))))]
                                 [path : Path])
          :  (Listof (Listof (Listof NumF)))
          (let* ([metric-fns ; : (Listof (Path -> (Listof (U #f(Listof NumF)))))
                  (map (lambda: ([m : (Metric  b c (Listof NumF))])
                                ((metric-analysis-unit m)
                                 (metric-computation m))) metrics)]
                 [#{result-seqs  : (Listof (U #f (Listof (Listof ( U #f (Listof NumF))))))}
                  (apply-to-scheme-files
                   (lambda: ([file : Path])
                            (map (lambda: ([fn : (Path -> (Listof (U #f (Listof NumF))))]) (fn file)) metric-fns)) path)])
            (map (lambda: ([l :  (Listof(Listof (Option (Listof NumF))))])
                          ;; FIXME - problem with inference and ordering
                          (nonfalses (apply append l)))
                 (pivot (nonfalses result-seqs)))))

;; compare* : (listof metric) -> (listof result)
(: compare* (All (b c)
                 ((Listof (Metric b c (Listof NumF)))
                  ->
                  (Listof (Result (Listof NumF) b c)))))
(define (compare* metrics)
  (let* ([seqAs (get-sequences metrics (COLLECTS-PATH))]
         [seqBs (get-sequences metrics (PLANET-CODE-PATH))])
    (map #{make-result @ (Listof NumF) b c} metrics seqAs seqBs)))

(pdefine: (a b c) (show [result : (Result a b c)]) : b
          ((metric->display (result-metric  result))
           (result-seqA result )
           (result-seqB result )))

(define: (a) (pretty-print-result [result : (Result (Listof NumF) (Listof Atom-display) a)]) : Void
  (for-each
   (lambda: ([l : (Listof Any)])
            (apply srfi48::format ;;sfri48:format
                   (current-output-port)
                   "~26F  | ~8,2F  | ~6,2F  | ~12,2F\n"
                   (format "~a" (car l))
                   (cdr l)))
   (list* '("test name" "collects" "planet" "significance")  ;;list instead of list*
          '("---------" "--------" "------" "------------")
          (show result ))))

;; applies only to the combined metric [or more generally to listof-answer results]
(pdefine: (a b c) (total [experiment-number : Integer] [result : (Result (Listof number) b c)]) : (Listof number)
          (define: (total/s [s : Table]) : number (apply + (list-ref (pivot s) experiment-number)))
          (list (total/s (result-seqA result)) (total/s (result-seqB result))))

;; ============================================================
;; UTILITY

(pdefine: (X Y) (imap [f : (X -> Y)] [il : (Listof X)]) : (Listof Y)
          (cond
            [(null? il) '()]
            [(not (pair? il)) (list (f il))]
            [else (cons (f (car il)) (imap f (cdr il)))]))

(pdefine: (X) (pivot [l : (Listof (Listof X))]): (Listof (Listof X))
          (cond
            [(null? l) '()]
            [else
             (let ([n (length (car l))])
               (build-list n (lambda: ([i : Integer]) (map (lambda: ([j : (Listof X)]) (list-ref j i)) l))))]))

(define: (sqr [x : Real]) : Real (* x x))
(define: (variance [xs : (Listof Real)]): Real
  (let ([avg (/ (apply + xs) (length xs))])
    (/ (apply + (map (lambda: ([x : number]) (sqr (- x avg))) xs))
       (sub1 (length xs)))))

(define: (table-ref [i : Integer] [j : Integer] [table : Table]): number
  (list-ref (list-ref table i) j))
(define: (row-total [i : Integer] [table : Table]) : number
  (apply + (list-ref table i)))
(define: (col-total [j : Integer] [table : Table]) : number
  (apply + (map (lambda: ([x : (Listof number)]) (list-ref x j)) table)))
(define: (table-sum [f : (Integer Integer -> Real)] [table : Table]) : number
  (let ([rows (length table)]
        [cols (length (car table))])
    (let loop ([i 0] [j 0] [#{sum : Real} 0])
          (cond
            [(>= j cols) sum]
            [(>= i rows) (loop 0 (add1 j) sum)]
            [else        (loop (add1 i) j (+ sum (f i j)))]))))

(pdefine: (Y) (improper-foldr [f : (Any Y -> Y)] [b : Y] [l : Any]) : Y
          (cond
            [(null? l) b]
            [(not (pair? l))
             (f l b)]
            [else
             (f (car l) (improper-foldr f b (cdr l)))]))

;; unused (and untypeable)
#;(define: (/* . [args : (Listof number)]) : number  ;;((number)) against (number) and USELESS
    (apply map (lambda: ([ns : number]) (apply / ns)) args))


;; ============================================================
;; MAIN ENTRY POINT

(define: results :
  #;Any
  ;; FIXME bug in typed scheme when this type is used

  (Listof (U (Result (Listof NumF) (Listof Atom-display) (Listof Sexpr))
             (Result (Listof NumF) (Listof Atom-display) Sexpr)))
  '())
; just in case i want to do some more analysis on the results afterwards,
; so i don't have to waste a minute if i forget to bind the return value to something
(define: (run-all-tests) : top
  (let*: ([rs1 :  (Listof (Result (Listof NumF) (Listof Atom-display) (Listof Any)))
               (#{compare* @ (Listof Atom-display) (Listof Any)}
                (list module-metrics))]
          [rs2 :  (Listof (Result (Listof NumF) (Listof Atom-display) Any))
               (#{compare* @ (Listof Atom-display) Any}
                (list tl-expr-metrics))])
         (let
             ([rs (append rs1 rs2)])
           (set! results rs)
           (for-each #{pretty-print-result @ (Listof Any)} rs1)
           (for-each #{pretty-print-result @ Any} rs2)
           rs)))

