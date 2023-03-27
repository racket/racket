#lang racket/base
(require racket/list
         racket/syntax
         "residual.rkt"
         "minimatch.rkt"
         (submod "rep-patterns.rkt" simple)
         "runtime-reflect.rkt")

(provide (all-defined-out))

;; ============================================================
;; Backtracking Monad

;; BT[t...] = SK[t...] FH FH UndoStack -> Answer
;; SK[t...] = FH FH UndoStack t... -> Answer
;; FH = (UndoStack Failure -> Answer)

;; FIXME: would need to update classic impl to use same type
;; FH = (cons Failure FH) | (UndoStack Failure -> Answer)
;; fh-add-failure : FH Failure -> FH
;;(define (fh-add-failure fh f)
;;  (cons f fh))

(define (fh-add-failure fh f1)
  (lambda (us f2) (fh us (cons f1 f2))))

;; call-fh : FH UndoStack Failure -> Answer
(define (call-fh fh us f)
  (define (failure-join f1 f2) (cons f1 f2))
  (if (pair? fh)
      (let ([f2 (car fh)] [fh (cdr fh)])
        (call-fh fh us (failure-join f f2)))
      (fh us f)))

;; BT's succeed and bind are "optimized" with case-lambda for 1 and 4
;; arguments, because those are common in SinglePattern and HeadPattern.

;; succeed : a... -> BT[a...]
(define succeed
  (case-lambda
    [(v)        (lambda (sk fh cp us) (sk fh cp us v))]
    [(u v w x)  (lambda (sk fh cp us) (sk fh cp us u v w x))]
    [vs         (lambda (sk fh cp us) (apply sk fh cp us vs))]))

;; fail : Failure -> BT[a...]
(define ((fail f) sk fh cp us)
  (call-fh fh us f))

;; bind : BT[a...] (a... -> BT[b...]) -> BT[b...]
(define ((bind c f) sk fh cp us)
  (define sk/bind
    (case-lambda
      [(fh cp us v)
       ((f v) sk fh cp us)]
      [(fh cp us u v w x)
       ((f u v w x) sk fh cp us)]
      [(fh cp us . vs)
       ((apply f vs) sk fh cp us)]))
  (c sk/bind fh cp us))

;; disj* : BT[a...] (-> BT[a...]) -> BT[a...]
(define ((disj* c1 cp2) sk fh cp us)
  (define (make-fh/failure fh f) (cons f fh))
  (define (fh/disj us1 f)
    (unwind-to us1 us)
    ((cp2) sk (fh-add-failure fh f) cp us))
  (c1 sk fh/disj cp us))

;; delimit : BT[a...] -> BT[a...]
(define ((delimit c) sk fh cp us)
  (define (sk/delimit fh cp* us . vs)
    (apply sk fh cp us vs))
  (let ([cp fh])
    (c sk/delimit fh cp us)))

;; commit : BT[a...] -> BT[a...]
(define ((commit c) sk fh cp us)
  (define (sk/commit fh* cp* us . vs)
    (apply sk fh cp us vs))
  (let ([cp fh])
    (c sk/commit fh cp us)))

;; if-goal : BT[a...] BT[b...] BT[b...] -> BT[b...]
;; If the first goal succeeds once, discard result and act like c1,
;; else discard failure and act like c2.
(define ((if-goal c c1 c2) sk fh cp us)
  (define (sk/if fh* cp* us* . vs)
    (c1 sk fh cp us))
  (define (fh/if us* fs)
    (unwind-to us* us)
    (c2 sk fh cp us))
  (c sk/if fh/if fh/if us))

;; do/undo : (-> Void) (-> Undo) -> BT[]
(define ((do/undo do-now get-undo) sk fh cp us)
  (define undo (get-undo))
  (begin (do-now) (sk fh cp (cons undo us))))

;; ============================================================
;; Pattern Runtime Support

;; ----------------------------------------
;; p : SinglePattern | HeadPattern
;; => (Stxish Syntax Progress ExpectStack REnv -> BT[?...])

(define ((p-or . ps) x cx pr es renv)
  (let loop ([ps ps])
    (match ps
      [(list p) (p x cx pr es renv)]
      [(cons p ps) (disj* (p x cx pr es renv) (lambda () (loop ps)))])))

(define ((p-action ap p) x cx pr es renv)
  (bind (ap pr es renv)
        (lambda (renv) (p x cx pr es renv))))

(define ((p-delimit p) x cx pr es renv)
  (delimit (p x cx pr es renv)))
(define ((p-commit p) x cx pr es renv)
  (commit (p x cx pr es renv)))
(define ((p-ord p ord) x cx pr es renv)
  (p x cx (ps-add pr ord) es renv))
(define ((p-post p) x cx pr es renv)
  (p x cx (ps-add-post pr) es renv))

(define (apply-reordering reo renv)
  (match reo
    [(cons n mapping)
     (define vec (make-vector n #f))
     (define renv-base
       (for/fold ([renv renv])
                 ([idx (in-vector mapping)] ;; shorter than renv!
                  [v (in-list renv)])
         (when idx (vector-set! vec idx v))
         (cdr renv)))
     (append (vector->list vec) renv-base)]))

(define ((reordering reo) renv)
  (apply-reordering reo renv))

(define (stx-e v) (if (syntax? v) (syntax-e v) v))

;; ----------------------------------------
;; s : SinglePattern
;; => (Stxish Syntax Progress ExpectStack REnv -> BT[REnv])

(define (s-any x cx pr es renv)
  (succeed renv))

(define (s-var x cx pr es renv)
  (succeed (cons (datum->syntax cx x cx) renv)))

(define ((s-parser parser -args+role bind-name? bind-nested?) x cx pr es renv)
  (lambda (sk fh cp us)
    (define (sk/parser fh us . avs)
      (sk fh cp us
          (let* ([renv (if bind-name? (cons (datum->syntax cx x cx) renv) renv)]
                 [renv (if bind-nested? (append (reverse avs) renv) renv)])
            renv)))
    (define-values (kws kwargs pargs role) (unwrap renv -args+role))
    (kwapply parser kws kwargs x cx pr es us fh cp role sk/parser pargs)))

(define (s-reflect -obj+args arity attr-decls bind-all?)
  (lambda (x cx pr es renv)
    (define-values (obj kws kwargs pargs) (unwrap renv -obj+args))
    (define parser (reflect-parser obj arity attr-decls #f))
    (lambda (sk fh cp us)
      (kwapply parser kws kwargs x cx pr es us fh cp #f
               (lambda (fh us . avs)
                 (sk fh cp us
                     (let* ([renv (if bind-all? (cons (datum->syntax cx x cx) renv) renv)]
                            [renv (if bind-all? (append (reverse avs) renv) renv)])
                       renv)))
               pargs))))

(define (s-match-expr x cx pr es renv)
  (define (expr-stx? v) (not (and (syntax? v) (keyword? (syntax-e v)))))
  (if (expr-stx? x)
      (succeed (cons (datum->syntax cx x cx) renv))
      (fail (failure* pr (es-add-thing pr "expression" #t #f es)))))

(define (s-match-id x cx pr es renv)
  (if (identifier? x)
      (succeed (cons x renv))
      (fail (failure* pr (es-add-thing pr "identifier" #t #f es)))))

(define ((s-integrated bind? pred desc -role) x cx pr es renv)
  (if (pred x)
      (succeed (if bind? (cons x renv) renv))
      (fail (failure* pr (es-add-thing pr desc #t (unwrap renv -role) es)))))

(define ((s-literal lit -phases) x cx pr es renv)
  (define-values (in-phase lit-phase)
    (if -phases
        (unwrap renv -phases)
        (let ([p (syntax-local-phase-level)]) (values p p))))
  (if (and (identifier? x)
           (free-identifier=? x lit in-phase lit-phase))
      (bind (do/undo (lambda () (state-cons! 'literals x))
                     (let ([st (current-state)]) (lambda () st)))
            (lambda () (succeed renv)))
      (fail (failure* pr (es-add-literal lit es)))))

(define (s-null x cx pr es renv)
  (if (null? (stx-e x))
      (succeed renv)
      (fail (failure* pr (es-add-atom '() es)))))

(define ((s-datum datum) x cx pr es renv)
  (if (equal? (syntax->datum (datum->syntax #f x)) datum) ;; FIXME
      (succeed renv)
      (fail (failure* pr (es-add-atom datum es)))))

(define ((s-head headp tailp) x cx pr es renv)
  (bind (headp x cx pr es renv)
        (lambda (renv x cx pr) (tailp x cx pr es renv))))

(define ((s-and . ps) x cx pr es renv)
  (let s-and-loop ([ps ps] [renv renv])
    (match ps
      ['() (succeed renv)]
      [(cons p ps)
       (bind (p x cx pr es renv)
             (lambda (renv) (s-and-loop ps renv)))])))

(define ((s-not p) x cx pr es renv)
  (if-goal (p x cx pr es renv) (fail (failure* pr es)) (succeed renv)))

(define ((s-pair headp tailp head-first-desc) x cx pr es renv)
  (define d (stx-e x))
  (cond [(pair? d)
         (define hx (car d))
         (define tx (cdr d))
         (define cx* (if (syntax? x) x cx))
         (bind (headp hx hx (ps-add-car pr) es renv)
               (lambda (renv) (tailp tx cx* (ps-add-cdr pr) es renv)))]
        [(null? d)
         (let ([es (es-add-proper-pair head-first-desc es)])
           (fail (failure* pr es)))]
        [else (fail (failure* pr es))]))

(define ((s-vector sp) x cx pr es renv)
  (define d (stx-e x))
  (if (vector? d)
      (sp (vector->list d) cx (ps-add-unvector pr) es renv)
      (fail (failure* pr es))))

(define ((s-box sp) x cx pr es renv)
  (define d (stx-e x))
  (if (box? d)
      (sp (unbox d) cx (ps-add-unbox pr) es renv)
      (fail (failure* pr es))))

(define ((s-pstruct key sp) x cx pr es renv)
  (define d (stx-e x))
  (define xkey (prefab-struct-key d))
  (if (and xkey (equal? xkey key))
      (let ([d (cdr (vector->list (struct->vector d)))])
        (sp d x pr es renv))
      (fail (failure* pr es))))

(define ((s-describe -info transp? sp [rl #f]) x cx pr es renv)
  (define-values (desc role) (unwrap renv -info))
  (let ([es (es-add-thing pr desc transp? (or rl role) es)]
        [pr (if transp? pr (ps-add-opaque pr))])
    (sp x cx pr es renv)))

(define (s-seq-end x cx pr es renv)
  (succeed (list* x cx pr renv)))

(define ((s-reorder reordering p) x cx pr es renv)
  (bind (p x cx pr es renv)
        (lambda (renv)
          (succeed (apply-reordering reordering renv)))))

;; ----------------------------------------

(define (s-dots1-v-null bind? plus?)
  (s-dots1-i-null bind? void #f plus?))

(define ((s-dots1-i-null bind? pred? desc plus?) x cx pr es renv)
  (define-values (status result)
    (predicate-ellipsis-parser x cx pr es pred? desc #f))
  (case status
    [(ok) (if (and plus? (null? result))
              (fail (failure* pr (es-add-proper-pair desc es)))
              (succeed (if bind? (cons result renv) renv)))]
    [else (fail result)]))

(define (s-dots1-null hp attrs-n)
  (define (dots1-loop x cx pr es renv acc)
    (define d (stx-e x))
    (cond [(pair? d)
           (bind (hp x cx pr es renv)
                 (lambda (renv* rx rcx rpr)
                   ;; renv* = (append hp-renv renv)
                   ;; Delay (take renv* attrs-n), transpose discards extra.
                   (dots1-loop rx rcx rpr es renv (cons renv* acc))))]
          [(null? d)
           (succeed (append (transpose attrs-n (reverse acc)) renv))]
          [else (fail (failure* pr es))]))
  (lambda (x cx pr es renv)
    (dots1-loop x cx pr es renv null)))

(define (s-dots1 headp tailp attrs-n headp-first-desc plus?)
  (define (dots1-loop x cx pr es renv acc)
    (disj* (bind (headp x cx pr es renv)
                 (lambda (renv* x cx pr)
                   ;; renv* = (append hp-renv renv)
                   ;; Delay (take renv* attrs-n), transpose discards extra.
                   (dots1-loop x cx pr es renv (cons renv* acc))))
           (lambda ()
             (if (and plus? (null? acc))
                 (fail (failure* pr (es-add-proper-pair headp-first-desc es)))
                 (let ([renv (append (transpose attrs-n (reverse acc)) renv)])
                   (tailp x cx pr es renv))))))
  (lambda (x cx pr es renv)
    (dots1-loop x cx pr es renv null)))

;; transpose : n:Nat (Listof*m (Listof*[n+p] X)) -> (Listof*n (Listof*m X))
(define (transpose ncols rows)
  (if (zero? ncols)
      null
      (cons (map car rows) (transpose (sub1 ncols) (map cdr rows)))))

(define (s-dots hs attrs-maps first-descs mins maxs -name+under+over-list defaults tailp)
  (define iter-ehs (map s-dots-iter hs attrs-maps maxs -name+under+over-list))
  (define finishs (map s-dots-finish attrs-maps first-descs mins -name+under+over-list))

  (lambda (x cx pr es renv0)

    ;; dots-loop : ... -> BT[REnv]
    (define (dots-loop x cx pr renv reph acc)
      ;; acc : EHMatch = (listof (vector attr-map single? local-renv))
      (disj* (bind (heads-iter-loop 0 iter-ehs x cx pr renv reph acc)
                   (lambda (x cx pr reph acc) (dots-loop x cx pr renv reph acc)))
             (lambda ()
               (let* ([renv (append (dots-compose-renv attrs-maps acc defaults renv0) renv)])
                 (bind (heads-finish-loop 0 finishs pr renv reph)
                       (lambda (renv) (tailp x cx pr es renv)))))))

    ;; heads-iter-loop : ... -> BT[x cx pr reph acc]
    (define (heads-iter-loop n iter-ehs x cx pr renv reph acc)
      ;; reph = (hasheqv Nat => Nat), rep counters
      (match iter-ehs
        [(list iter-eh)
         (iter-eh x cx pr es renv n reph acc)]
        [(cons iter-eh iter-ehs)
         (disj* (iter-eh x cx pr es renv n reph acc)
                (lambda () (heads-iter-loop (add1 n) iter-ehs x cx pr renv reph acc)))]))

    ;; heads-finish-loop : ... -> BT[renv]
    (define (heads-finish-loop n finishs pr renv reph)
      (match finishs
        [(cons finish finishs)
         (bind (finish pr es (hash-ref reph n 0) renv0 renv)
               (lambda (renv) (heads-finish-loop (add1 n) finishs pr renv reph)))]
        ['() (succeed renv)]))

    (dots-loop x cx pr renv0 (hasheqv) null)))

;; RepHash = (hasheqv Nat => Nat), maps EH index to repetition count so far

;; EHMatch = (vector (cons Boolean AttrsMap) REnv)
;; Represents one match of one eh alternative.

;; dots-compose-renv : (Listof AttrsMap) (Listof EHMatch) (Listof #f/Wrapped[REnv]) REnv
;;                  -> REnv
(define (dots-compose-renv attrs-maps rmatches defaults renv0)
  (define attrs-n (for/sum ([am (in-list attrs-maps)]) (vector-length (cdr am))))
  (define renv-vec (make-vector attrs-n 'uninitialized))
  (for ([am (in-list attrs-maps)])
    (define single? (car am))
    (for ([all-idx (in-vector (cdr am))])
      (vector-set! renv-vec all-idx (if single? #f null))))
  ;; ----
  (for ([rmatch (in-list rmatches)])
    (match rmatch
      [(vector (cons single? mapping) local-renv)
       (for ([all-idx (in-vector mapping)]  ;; shorter than local-renv! ("delayed take")
             [v (in-list local-renv)])
         (if single?
             (vector-set! renv-vec all-idx v)
             (vector-set! renv-vec all-idx (cons v (vector-ref renv-vec all-idx)))))]))
  ;; ----
  ;; Add defaults. For backwards compatibility, defaults are set on any absent attribute,
  ;; even if the enclosing alternative matched. (See test suite for ~optional w/ defaults.)
  (for ([attrs-map (in-list attrs-maps)]
        [default (in-list defaults)]
        [index (in-naturals)]
        #:when default)
    (define mapping (cdr attrs-map))
    (for ([all-idx (in-vector mapping)]
          [v (in-list (unwrap renv0 default))]) ;; FIXME: delay vs individually?
      (vector-set! renv-vec all-idx (or (vector-ref renv-vec all-idx) v))))
  ;; ----
  ;; Since we process most recent match first, attr value lists are in correct order.
  (vector->list renv-vec))

;; s-dots-iter : ... -> {x cx pr es reps} -> BT[x cx pr reph acc]
;; responsible for trying an iteration
(define (s-dots-iter hp attrs-map maxreps -name+under+over)
  (lambda (x cx pr es renv n reph acc)
    (bind (hp x cx pr es renv)
          (lambda (renv* rx rcx rpr)
            (define reps (add1 (hash-ref reph n 0)))
            (cond [(> reps maxreps)
                   (define-values (name under over) (unwrap renv -name+under+over))
                   (let ([es (es-add-too-many es reps name over)])
                     (fail (failure* rpr es)))]
                  [else
                   (when (and (zero? (ps-difference pr rpr)) (= maxreps +inf.0))
                     (error/null-eh-match))
                   ;; renv* = (append hp-renv renv), but delay take until later
                   (succeed rx rcx rpr (hash-set reph n reps)
                            (cons (vector attrs-map renv*) acc))])))))

;; s-dots-finish : ... -> {pr es reps renv0 renv} -> BT[renv]
(define (s-dots-finish attrs-map first-desc minreps -name+under+over)
  (lambda (pr es reps renv0 renv)
    (cond [(< reps minreps)
           (define-values (name under over) (unwrap renv0 -name+under+over))
           (define once? (and (= minreps 1) (car attrs-map)))
           (let ([es (es-add-too-few es reps once? name under first-desc)])
             (fail (failure* pr es)))]
          [else (succeed renv)])))

(define (es-add-too-many es reps name over-msg)
  (es-add-message (or over-msg (name->too-many name)) es))

(define (es-add-too-few es reps once? name under-msg first-desc)
  (define (too-few-name) (if once? (name->too-few/once name) (name->too-few name)))
  (cond [(or under-msg (too-few-name))
         => (lambda (msg) (es-add-message msg es))]
        [first-desc (es-add-proper-pair first-desc es)]
        [else es]))

;; ----------------------------------------
;; a : ActionPattern
;; => (Progress ExpectStack REnv -> BT[REnv])

(define (a-cut pr es renv)
  (lambda (sk fh cp us)
    (let ([fh cp])
      (sk fh cp us renv))))

(define ((a-fail -cnd -msg) pr es renv)
  (define cnd (unwrap renv -cnd))
  (if cnd
      (let ([pr (if (syntax? cnd) (ps-add-stx pr cnd) pr)])
        (fail (failure* pr (es-add-message (unwrap renv -msg) es))))
      (succeed renv)))

(define ((a-bind -expr) pr es renv)
  (define val (unwrap renv -expr))
  (succeed (cons val renv)))

(define ((a-parse sp -expr) pr es renv)
  (define y0 (unwrap renv -expr))
  (define y (datum->syntax/with-clause y0))
  (sp y y (ps-add-stx pr y) es renv))

(define ((a-and . ps) pr es renv)
  (let a-and-loop ([ps ps] [renv renv])
    (match ps
      ['() (succeed renv)]
      [(cons ap ps)
       (bind (ap pr es renv)
             (lambda (renv) (a-and-loop ps renv)))])))

(define ((a-do -proc) pr es renv)
  (define proc (unwrap renv -proc))
  (lambda (sk fh cp us)
    (parameterize ((current-state-writable? #t))
      (define init-state (current-state))
      (define bundle (proc))
      (parameterize ((current-state-writable? #f))
        (let ([us (maybe-add-state-undo init-state (current-state) us)])
          (sk fh cp us (cons bundle renv)))))))

(define ((a-undo -proc) pr es renv)
  (define proc (unwrap renv -proc))
  (lambda (sk fh cp us)
    (let ([us (cons proc us)]
          [cp illegal-cut-error])
      (sk fh cp us renv))))

(define ((a-ord ap ord) pr es renv)
  (ap (ps-add pr ord) es renv))

(define ((a-post ap) pr es renv)
  (ap (ps-add-post pr) es renv))

;; ----------------------------------------
;; h : HeadPattern
;; => (Stxish Syntax Progress ExpectStack REnv -> BT[REnv Stxish Syntax Progress])

(define ((h-parser parser -args+role bind-name? bind-nested?) x cx pr es renv)
  (lambda (sk fh cp us)
    (define (sk/parser fh us rx rcx rpr . avs)
      (define (get-list) (stx-list-take x (ps-difference pr rpr)))
      (sk fh cp us
          (let* ([renv (if bind-name? (cons (get-list) renv) renv)]
                 [renv (if bind-nested? (append (reverse avs) renv) renv)])
            renv)
          rx rcx rpr))
    (define-values (kws kwargs pargs role) (unwrap renv -args+role))
    (kwapply parser kws kwargs x cx pr es us fh cp role sk/parser pargs)))

(define (h-reflect -obj+args arity attr-decls bind-all?)
  (lambda (x cx pr es renv)
    (define-values (obj kws kwargs pargs) (unwrap renv -obj+args))
    (define parser (reflect-parser obj arity attr-decls #t))
    (lambda (sk fh cp us)
      (kwapply parser kws kwargs x cx pr es us fh cp #f
               (lambda (fh us rx rcx rpr . avs)
                 (define (get-list) (stx-list-take x (ps-difference pr rpr)))
                 (sk fh cp us
                     (let* ([renv (if bind-all? (cons (get-list) renv) renv)]
                            [renv (if bind-all? (append (reverse avs) renv) renv)])
                       renv)
                     rx rcx rpr))
               pargs))))

(define ((h-single sp sp-first-desc) x cx pr es renv)
  (define d (stx-e x))
  (cond [(pair? d)
         (define hx (car d))
         (define tx (cdr d))
         (define cx* (if (syntax? x) x cx))
         (bind (sp hx hx (ps-add-car pr) es renv)
               (lambda (renv)
                 (succeed renv tx cx* (ps-add-cdr pr))))]
        [(null? d)
         (let ([es (es-add-proper-pair sp-first-desc es)])
           (fail (failure* pr es)))]
        [else (fail (failure* pr es))]))

(define ((h-seq sp) x cx pr es renv)
  (bind (sp x cx pr es renv)
        (lambda (renv)
          (match renv
            [(list* x cx pr renv)
             (succeed renv x cx pr)]))))

(define ((h-and hp sp) x cx pr es renv)
  (bind (hp x cx pr es renv)
        (lambda (renv rx rcx rpr)
          (define lst (stx-list-take x (ps-difference pr rpr)))
          (bind (sp lst cx pr es renv)
                (lambda (renv)
                  (succeed renv rx rcx rpr))))))

(define ((h-describe -info transp? hp [rl #f]) x cx pr es renv)
  (define-values (desc role) (unwrap renv -info))
  (let ([es (es-add-thing pr desc transp? (or rl role) es)])
    (if transp?
        (hp x cx pr es renv)
        (let ([pr (ps-add-opaque pr)])
          (bind (hp x cx pr es renv)
                (lambda (renv x cx pr)
                  (succeed renv x cx (ps-pop-opaque pr))))))))

(define ((h-peek hp) x cx pr es renv)
  (bind (hp x cx pr es renv)
        (lambda (renv _x _cx _pr)
          (succeed renv x cx pr))))

(define ((h-peek-not hp) x cx pr es renv)
  (if-goal (hp x cx pr es renv)
           (fail (failure* pr es))
           (succeed renv x cx pr)))

(define ((h-reorder reordering hp) x cx pr es renv)
  (bind (hp x cx pr es renv)
        (lambda (renv rx rcx rpr)
          (succeed (apply-reordering reordering renv) rx rcx rpr))))

(define ((h-ord hp ord) x cx pr es renv)
  (bind (hp x cx (ps-add pr ord) es renv)
        (lambda (renv x cx pr)
          (succeed renv x cx (ps-pop-ord pr)))))

(define ((h-post hp) x cx pr es renv)
  (bind (hp x cx (ps-add-post pr) es renv)
        (lambda (renv x cx pr)
          (succeed renv x cx (ps-pop-post pr)))))

;; ============================================================
;; Simple Patterns (see also (submod "rep-patterns.rkt" simple))

;; Result[a] = a | Failure    -- specialize to a is list

;; runit : a -> Result[a]
(define (runit v) v)

;; rfail : Failure -> Result[a]
(define (rfail f) f)

;; rbind : Result[a] (a -> Result[b]) -> Result[b]
(define (rbind c f)
  (if (or (null? c) (pair? c)) (f c) c))

(define ((s-simple simple) x cx pr es renv)
  (define (pair/null? v) (or (pair? v) (null? v)))
  (define (expr-stx? v) (not (and (syntax? v) (keyword? (syntax-e v)))))

  ;; parse : Simple Stxish Syntax Progress ExpectStack REnv -> Result[REnv]
  (define (parse simple x cx pr es renv)
    (define (runit-matched) (runit (cons (datum->syntax cx x cx) renv)))
    (define (rfail-thing desc) (rfail (failure* pr (es-add-thing pr desc #t #f es))))
    (if (symbol? simple)
        (case simple
          [(var) (runit-matched)]
          [(id) (if (identifier? x) (runit (cons x renv)) (rfail-thing "identifier"))]
          [(expr) (if (expr-stx? x) (runit-matched) (rfail-thing "expression"))]
          [(_) (runit renv)]
          [(_id) (if (identifier? x) (runit renv) (rfail-thing "identifier"))]
          [(_expr) (if (expr-stx? x) (runit renv) (rfail-thing "expression"))]
          [(seq-end) (runit (list* x cx pr renv))]
          [else (error 's-simple "internal error: bad simple pattern: ~e" simple)])
        (match simple
          [(cons simple1 simple2)
           (define d (stx-e x))
           (cond [(pair? d)
                  (define hx (car d))
                  (define tx (cdr d))
                  (define cx* (if (syntax? x) x cx))
                  (rbind (parse simple1 hx hx (ps-add-car pr) es renv)
                         (lambda (renv) (parse simple2 tx cx* (ps-add-cdr pr) es renv)))]
                 [(null? d)
                  (let ([es (es-add-proper-pair (first-desc-simple simple1) es)])
                    (rfail (failure* pr es)))]
                 [else (rfail (failure* pr es))])]
          ['()
           (if (null? (stx-e x))
               (runit renv)
               (rfail (failure* pr (es-add-atom '() es))))]
          [(sim:dots simple nattrs plus?)
           (define pred (case simple [(var) void] [(id) identifier?] [(expr) expr-stx?] [else #f]))
           (define desc (case simple [(id) "identifier"] [(expr) "expression"] [else #f]))
           (cond [pred
                  (define-values (status result)
                    (predicate-ellipsis-parser x cx pr es pred desc #f))
                  (case status
                    [(ok)
                     (cond [(and plus? (null? result))
                            (let ([es (es-add-proper-pair desc es)])
                              (rfail (failure* pr es)))]
                           [else (runit (cons result renv))])]
                    [else (rfail result)])]
                 [else (parse-dots simple nattrs plus? x cx pr es renv)])]
          [(sim:datum d)
           (if (equal? (syntax->datum (datum->syntax #f x)) d) ;; FIXME?
               (runit renv)
               (rfail (failure* pr (es-add-atom d es))))]
          [(sim:describe simple desc transp?)
           (let ([es (es-add-thing pr desc transp? #f es)]
                 [pr (if transp? pr (ps-add-opaque pr))])
             (parse simple x cx pr es renv))])))

  (define (parse-dots simple nattrs plus? x cx pr es renv)
    (define (dots-loop x cx pr acc)
      (define d (stx-e x))
      (cond [(pair? d)
             (rbind (let ([hx (car d)])
                      (parse simple hx hx (ps-add-car pr) es null))
                    (lambda (local-renv)
                      (dots-loop (cdr d) (if (syntax? x) x cx) (ps-add-cdr pr)
                                 (cons local-renv acc))))]
            [(null? d)
             (runit (append (transpose nattrs (reverse acc)) renv))]
            [else (rfail (failure* pr es))]))
    (dots-loop x cx pr null))

  (define r (parse simple x cx pr es renv))
  (cond [(pair/null? r) (succeed r)]
        [else (fail r)]))

;; first-desc-simple : Simple -> #f | String | '(any) | `(datum ,Datum)
(define (first-desc-simple simple)
  (if (symbol? simple)
      (match simple
        ['_ '(any)]
        ['var '(any)]
        ['id "identifier"]
        ['expr "expression"]
        [_ #f])
      (match simple
        [(sim:describe _ desc _) desc]
        [(sim:datum d) `(datum ,d)]
        [_ #f])))

;; ============================================================
;; Optimizer Support

;; An optimized Matrix of N columns is compiled to
;;   (Listof[N] (vector Stx Syntax Progress)) ExpectStack REnv -> BT[REnv]

(define ((s-matrix mat) x cx pr es renv)
  (mat (list (vector x cx pr)) es renv))

(define ((m-or mats) inputs es renv)
  (let loop ([mats mats])
    (match mats
      [(cons mat '())
       (mat inputs es renv)]
      [(cons mat mats)
       (disj* (mat inputs es renv)
              (lambda () (loop mats)))])))

(define ((m-row ps k) inputs es renv)
  (let loop ([ps ps] [inputs inputs] [renv renv])
    (match ps
      [(cons p ps)
       (match inputs
         [(cons (vector x cx pr) inputs)
          (bind (p x cx pr es renv)
                (lambda (renv) (loop ps inputs renv)))])]
      ['() (succeed (k renv))])))

(define ((m-and mat) inputs es renv)
  (mat (cons (car inputs) inputs) es renv))

(define ((m-pair first-descs mat) inputs es renv)
  (match inputs
    [(cons (vector x cx pr) inputs)
     (define d (stx-e x))
     (cond [(pair? d)
            (define cx* (if (syntax? x) x cx))
            (mat (list* (vector (car d) cx* (ps-add-car pr))
                        (vector (cdr d) cx* (ps-add-cdr pr))
                        inputs)
                 es renv)]
           [(null? d)
            (fail (for/list ([first-desc (in-list first-descs)])
                    (let ([es (es-add-proper-pair first-desc es)])
                      (failure* pr es))))]
           [else (fail (failure* pr es))])]))

(define ((m-same p mat) inputs es renv)
  (match inputs
    [(cons (vector x cx pr) inputs)
     (bind (p x cx pr es renv)
           (lambda (renv) (mat inputs es renv)))]))


;; ============================================================
;; RHS and Clauses Support

(define (run-clauses who context x no-fail? track-literals? parser)
  (define ctx0 (normalize-context who context x))
  (define pr (ps-empty x (cadr ctx0)))
  (define es (if no-fail? #f #t))
  (define fh (syntax-patterns-fail ctx0))
  (define us null)
  (parameterize ((current-syntax-context (cadr ctx0))
                 (current-state '#hasheq())
                 (current-state-writable? #f))
    (let ([sk (if track-literals? (track-literals-sk who) base-sk)])
      ((parser x x pr es null) sk fh fh null))))

;; base-sk : SK[(cons (-> Answer) _)]
(define (base-sk fh cp us renv)
  ((car renv)))

;; track-literals-sk : Symbol -> SK[(cons (-> Syntax) _)]
(define ((track-literals-sk who) fh cp us renv)
  (track-literals who ((car renv))))

;; ============================================================
;; Wrapped Expressions

;; Wrapped[t...] is one of
;; - (list t ...)                  -- quoted
;; - (REnv -> (values t ...))      -- REnv corresponds to AEnv
;; - (bundle-ref offset index)     -- offset of bundle in renv, index in vector

(struct bundle-ref (offset index) #:prefab)

;; unwrap : REnv Wrapped[t...] -> (values t ...)
(define (unwrap renv wrapped)
  (cond [(list? wrapped) (apply values wrapped)]
        [(bundle-ref? wrapped)
         (define bundle (list-ref renv (bundle-ref-offset wrapped)))
         (unless (vector? bundle) (error 'unwrap "expected bundle, got ~e" bundle))
         (define wrapped2 (vector-ref bundle (bundle-ref-index wrapped)))
         (wrapped2 renv)]
        [else (wrapped renv)]))

;; ============================================================
;; Keyword Apply w/ unsorted keyword arguments

(define-syntax-rule (kwapply f kws kwargs parg ...)
  (let-values ([(sorted-kws sorted-kwargs)
                (sort-kw-args kws kwargs)])
    (keyword-apply f sorted-kws sorted-kwargs parg ...)))

(define (sort-kw-args kws kwargs)
  (if (or (null? kws) (null? (cdr kws)))
      (values kws kwargs)
      (let* ([kw+arg-list (map cons kws kwargs)]
             [sorted-kw+arg-list (sort kw+arg-list keyword<? #:key car)])
        (values (map car sorted-kw+arg-list)
                (map cdr sorted-kw+arg-list)))))
