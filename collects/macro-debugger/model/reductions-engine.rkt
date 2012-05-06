#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/contract)
         racket/contract/base
         syntax/stx
         "deriv-util.rkt"
         "stx-util.rkt"
         "context.rkt"
         "steps.rkt"
         "reductions-config.rkt")
(provide (all-from-out "steps.rkt")
         (all-from-out "reductions-config.rkt")
         DEBUG
         R
         !)

(define-syntax ! (syntax-rules ()))

(define-syntax-rule (with-syntax1 ([pattern rhs]) . body)
  (syntax-case rhs ()
    [pattern (let () . body)]
    [x (raise-syntax-error 'with-syntax1
                           (format "failed pattern match against ~s"
                                   'pattern)
                           #'x)]))

(define-syntax-rule (DEBUG form ...)
  (when #f
    form ... (void)))

(define-syntax-rule (STRICT-CHECKS form ...)
  (when #f
    form ... (void)))

(define RST/c (syntaxish? syntaxish? state/c list? . -> . RS/c))

;; (R R-clause ...) : RST

;; An R-clause is one of
;;   [! expr]
;;   [#:set-syntax expr]
;;   [#:expect-syntax expr]
;;   [#:pattern pattern]
;;   [#:do expr ...]
;;   [#:let var expr]
;;   [#:left-foot]
;;   [#:walk term2 description]
;;   [#:rename pattern rename [description]]
;;   [#:rename/no-step pattern stx stx]
;;   [#:reductions expr]
;;   [#:learn ids]
;;   [#:if test R-clause ...]
;;   [#:when test R-clause ...]
;;   [#:hide-check ids]
;;   [#:seek-check]
;;   [generator hole fill]

(define-syntax R
  (syntax-parser
    [(R . clauses)
     #'(lambda (f v s ws)
         (R** f v _ s ws . clauses))]))

(define-syntax RP
  (syntax-parser
   [(RP p . clauses)
    #'(lambda (f v s ws)
        (R** f v p s ws . clauses))]))

;; (R** form virtual-form pattern . clauses)
(define-syntax R**
  (syntax-parser #:literals (! =>)

    ;; (R** f v p s ws . clauses)
    ;;   f is the "real" form
    ;;   v is the "virtual" form (used for steps)
    ;;      - vis=#t: starts as f
    ;;      - vis=#f: starts as last visible term
    ;;   s is the last marked state
    ;;   ws is the list of steps, reversed

    ;; Base: done
    [(R** f v p s ws)
     #'(RSunit ws f v s)]

    [(R** f v p s ws => k . more)
     #:declare k (expr/c #'RST/c)
     #'(RSbind (k f v s ws)
               (RP p . more))]

    ;; Error-point case
    [(R** f v p s ws [! maybe-exn] . more)
     #:declare maybe-exn (expr/c #'(or/c exn? false/c))
     #'(let ([x maybe-exn])
         (if x
             ;; FIXME
             (RSfail (cons (stumble v x) ws) x)
             (R** f v p s ws . more)))]

    ;; Change patterns
    [(R** f v p s ws [#:pattern p2] . more)
     #'(R** f v p2 s ws . more)]

    ;; Execute expressions for effect
    [(R** f v p s ws [#:do expr ...] . more)
     #'(begin
         (with-syntax1 ([p f])
           expr ... (void))
         (R** f v p s ws . more))]

    [(R** f v p s ws [#:let var expr] . more)
     #'(let ([var (with-syntax1 ([p f]) expr)])
         (R** f v p s ws . more))]

    [(R** f v p s ws [#:parameterize ((param expr) ...) . clauses] . more)
     #:declare param (expr/c #'parameter?)
     #'(RSbind (parameterize ((param expr) ...)
                 (R** f v p s ws . clauses))
               (RP p . more))]

    ;; Change syntax
    [(R** f v p s ws [#:set-syntax form] . more)
     #:declare form (expr/c #'syntaxish?)
     #'(let ([f2 (with-syntax1 ([p f]) form)])
         ;; FIXME: should (current-pass-hides?) be relevant?
         (let ([v2 (if (visibility) f2 v)])
           (R** f2 v2 p s ws . more)))]

    [(R** f v p s ws [#:expect-syntax expr ds] . more)
     #:declare expr (expr/c #'syntax?)
     #'(let ([expected (with-syntax1 ([p f]) expr)])
         (STRICT-CHECKS
          (check-same-stx 'expect-syntax f expected ds))
         (R** f v p s ws . more))]

    [(R** f v p s ws [#:left-foot] . more)
     #'(R** f v p s ws [#:step #f v] . more)]
    [(R** f v p s ws [#:left-foot fs] . more)
     #'(R** f v p s ws [#:step #f fs] . more)]

    [(R** f v p s ws [#:step type] . more)
     #'(R** f v p s ws [#:step type v] . more)]

    [(R** f v p s ws [#:step type fs] . more)
     #:declare fs (expr/c #'syntaxish?)
     #:declare type (expr/c #'(or/c step-type? false/c))
     #'(let ([s2 (and (visibility)
                      (current-state-with v (with-syntax1 ([p f]) fs)))]
             [type-var type])
         (DEBUG
          (printf "visibility = ~s\n" (if (visibility) 'VISIBLE 'HIDDEN))
          (printf "step: s1 = ~s\n" s)
          (printf "step: s2 = ~s\n\n" s2))
         (let ([ws2
                (if (and (visibility) type-var)
                    (cons (make step type-var s s2) ws)
                    ws)])
           (R** f v p s2 ws2 . more)))]

    [(R** f v p s ws [#:walk form2 description] . more)
     #:declare form2 (expr/c #'syntaxish?)
     #'(let ([wfv (with-syntax1 ([p f]) form2)])
         (R** f v p s ws
              [#:left-foot]
              [#:set-syntax wfv]
              [#:step description]
              . more))]

    [(R** f v p s ws [#:reductions rs] . more)
     #:declare rs (expr/c #'(listof step?))
     #'(let ([ws2
              (if (visibility)
                  (revappend (with-syntax1 ([p f]) rs) ws)
                  ws)])
         (R** f v p s ws2 . more))]

    [(R** f v p s ws [#:in-hole hole . clauses] . more)
     #'(let ([k (RP p . more)]
             [reducer
              (lambda (_)
                (R . clauses))])
         (Run reducer f v p s ws hole #f k))]

    ;; Rename
    [(R** f v p s ws [#:rename pattern renames] . more)
     #'(R** f v p s ws [#:rename pattern renames #f] . more)]
    [(R** f v p s ws [#:rename pattern renames description] . more)
     #'(R** f v p s ws [#:rename* pattern renames description #f]. more)]

    [(R** f v p s ws [#:rename* pattern renames description mark-flag] . more)
     #'(let-values ([(renames-var description-var)
                     (with-syntax1 ([p f])
                       (values renames description))])
         (let* ([pre-renames-var
                 (with-syntax1 ([p f]) (syntax pattern))]
                [f2
                 ((CC pattern f p) renames)]
                [whole-form-rename? (eq? f pre-renames-var)]
                [renames-mapping
                 (make-renames-mapping pre-renames-var renames-var)]
                [v2
                 (cond [(or (visibility) (eq? mark-flag #f))
                        (apply-renames-mapping renames-mapping v)]
                       [(eq? mark-flag 'mark)
                        v]
                       [(eq? mark-flag 'unmark)
                        (apply-renames-mapping
                         (compose-renames-mappings
                          (table->renames-mapping (marking-table))
                          renames-mapping)
                         v)])]
                [ws2
                 (if (and description-var (visibility))
                     (cons (walk v v2 description-var
                                 #:foci1 pre-renames-var
                                 #:foci2 renames-var)
                           ws)
                     ws)])
           (parameterize ((subterms-table
                           (table-apply-renames-mapping
                            (subterms-table)
                            renames-mapping
                            whole-form-rename?)))
             (R** f2 v2 p s ws2 . more))))]

    [(R** f v p s ws [#:rename/mark pvar from to] . more)
     #:declare from (expr/c #'syntaxish?)
     #:declare to (expr/c #'syntaxish?)
     #'(let ([real-from (with-syntax1 ([p f]) #'pvar)])
         (STRICT-CHECKS
          (check-same-stx 'rename/mark real-from from))
         (when (marking-table)
           (add-to-renames-table (marking-table) from to))
         (R** f v p s ws [#:rename* pvar to #f 'mark] . more))]

    [(R** f v p s ws [#:rename/unmark pvar from to] . more)
     #:declare from (expr/c #'syntaxish?)
     #:declare to (expr/c #'syntaxish?)
     #'(let ([real-from (with-syntax1 ([p f]) #'pvar)])
         (STRICT-CHECKS
          (check-same-stx 'rename/mark real-from from))
         (R** f v p s ws [#:rename* pvar to #f 'unmark] . more))]

    ;; Change syntax with rename (but no step)
    [(R** f v p s ws [#:rename/no-step pvar from to] . more)
     #:declare from (expr/c #'syntaxish?)
     #:declare to (expr/c #'syntaxish?)
     #'(let ([real-from (with-syntax1 ([p f]) #'pvar)])
         (STRICT-CHECKS
          (check-same-stx 'rename/no-step real-from from))
         (R** f v p s ws [#:rename pvar to] . more))]

    ;; Add to definite binders
    [(R** f v p s ws [#:binders ids] . more)
     #:declare ids (expr/c #'(listof identifier))
     #'(begin (learn-binders (flatten-identifiers (with-syntax1 ([p f]) ids)))
              (R** f v p s ws . more))]

    ;; Add to definite uses
    [(R** f v p s ws [#:learn ids] . more)
     #:declare ids (expr/c #'(listof identifier?))
     #'(begin (learn-definites (with-syntax1 ([p f]) ids))
              (R** f v p s ws . more))]

    ;; Conditional (pattern changes lost afterwards ...)
    [(R** f v p s ws [#:if test [consequent ...] [alternate ...]] . more)
     #'(let ([continue (RP p . more)])
         (if (with-syntax1 ([p f]) test)
             (R** f v p s ws consequent ... => continue)
             (R** f v p s ws alternate ... => continue)))]

    ;; Conditional (pattern changes lost afterwards ...)
    [(R** f v p s ws [#:when test consequent ...] . more)
     #'(let ([continue (RP p . more)])
         (if (with-syntax1 ([p f]) test)
             (R** f v p s ws consequent ... => continue)
             (continue f v s ws)))]

    ;; HIDING DIRECTIVES
    [(R** f v p s ws [#:hide-check ids] . more)
     #:declare ids (expr/c #'(listof identifier?))
     #'(visibility-off (andmap (macro-policy) ids)
                       v
                       (lambda () (R** f v p s ws . more)))]

    [(R** f v p s ws [#:seek-check] . more)
     #'(seek-point f v (lambda (v2) (R** f v2 p s ws . more)))]

    [(R** f v p s ws [#:print-state msg] . more)
     #'(begin (printf "** ~s\n" msg)
              (printf "f = ~.s\n" (stx->datum f))
              (printf "v = ~.s\n" (stx->datum v))
              (printf "s = ~.s\n" (stx->datum s))
              (R** f v p s ws . more))]

    ;; ** Multi-pass reductions **

    ;; Pass1 does expansion.
    ;; If something should happen regardless of whether hiding occurred
    ;; in pass1 (eg, lifting), put it before the Pass2 marker.

    ;; Use #:unsafe-bind-visible to access 'v'
    ;; Warning: don't do anything that relies on real 'f' before pass2

    ;; If something should be hidden if any hiding occurred in pass1,
    ;; put it after the Pass2 marker (eg, splice, block->letrec).

    [(R** f v p s ws [#:pass1] . more)
     #'(parameterize ((hides-flags
                       (cons (box (not (visibility))) (hides-flags))))
         (DEBUG (printf "** pass1\n"))
         (R** f v p s ws . more))]

    [(R** f v p s ws [#:pass2 clause ...] . more)
     #'(let* ([previous-pass-hides? (current-pass-hides?)]
              [k (lambda (f2 v2 s2 ws2)
                   (parameterize ((hides-flags (cdr (hides-flags))))
                     (when previous-pass-hides? (current-pass-hides? #t))
                     (R** f2 v2 p s2 ws2 . more)))])
         (DEBUG (printf "** pass2\n"))
         ;; FIXME: maybe refresh subterms table from v?
         (visibility-off (not previous-pass-hides?)
                         v
                         (lambda ()
                           (when #f (print-viable-subterms v))
                           (R** f v p s ws clause ... => k))
                         #t))]

    [(R** f v p s ws [#:with-visible-form clause ...] . more)
     #'(let ([k (RP p #| [#:set-syntax f] |# . more)])
         (if (visibility)
             (R** v v p s ws clause ... => k)
             (k f v s ws)))]

    [(R** f v p s ws [#:new-local-context clause ...] . more)
     ;; If vis = #t, then (clause ...) do not affect local config
     ;; If vis = #f, then proceed normally
     ;;   *except* must save & restore real term
     #'(let* ([vis (visibility)]
              [process-clauses (lambda () (R** #f (if vis #f v) _ #f ws clause ...))])
         (RSbind (if vis
                     (with-new-local-context v (process-clauses))
                     (process-clauses))
                 (lambda (f2 v2 s2 ws2)
                   (let ([v2 (if vis v v2)]
                         [s2 (if vis s s2)])
                     (R** f v2 p s2 ws2 . more)))))]

    ;; Subterm handling
    [(R** f v p s ws [reducer hole fill] . more)
     #:declare reducer (expr/c #'(any/c . -> . RST/c))
     #'(let ([k (RP p . more)]
             [reducer-var reducer])
         (Run reducer-var f v p s ws hole fill k))]))

(define-syntax (Run stx)
  (syntax-case stx ()
    ;; Implementation of subterm handling for (hole ...) sequences
    [(Run reducer f v p s ws (hole :::) fills-e k)
     (and (identifier? #':::)
          (free-identifier=? #'::: (quote-syntax ...)))
     #'(let* ([fctx (CC (hole :::) f p)]
              [init-e1s (with-syntax1 ([p f]) (syntax->list #'(hole :::)))]
              [fills fills-e])
         (DEBUG
           (printf "Run (multi, vis=~s)\n" (visibility))
           (printf " f: ~.s\n" (stx->datum f))
           (printf " v: ~.s\n" (stx->datum v))
           (printf " p: ~.s\n" 'p)
           (printf " hole: ~.s\n" '(hole :::))
           (print-viable-subterms v))
         (if (visibility)
             (let ([vctx (CC (hole :::) v p)]
                   [vsubs (with-syntax1 ([p v]) (syntax->list #'(hole :::)))])
               (run-multiple/visible reducer init-e1s fctx vsubs vctx fills s ws k))
             (run-multiple/nonvisible reducer init-e1s fctx v fills s ws k)))]
    ;; Implementation of subterm handling
    [(Run reducer f v p s ws hole fill k)
     #'(let* ([init-e (with-syntax1 ([p f]) #'hole)]
              [fctx (CC hole f p)])
         (DEBUG
           (printf "Run (single, vis=~s)\n" (visibility))
           (printf " f: ~.s\n" (stx->datum f))
           (printf " v: ~.s\n" (stx->datum v))
           (printf " p: ~.s\n" 'p)
           (printf " hole: ~.s\n" 'hole)
           (print-viable-subterms v))
         (if (visibility)
             (let ([vctx (CC hole v p)]
                   [vsub (with-syntax1 ([p v]) #'hole)])
               (run-one reducer init-e fctx vsub vctx fill s ws k))
             (run-one reducer init-e fctx v values fill s ws k)))]))

;; run-one
(define (run-one reducer init-e fctx vsub vctx fill s ws k)
  (DEBUG
    (printf "run-one\n")
    (printf "  fctx: ~.s\n" (stx->datum (fctx #'HOLE)))
    (printf "  vctx: ~.s\n" (stx->datum (vctx #'HOLE))))
  (RSbind (with-context vctx
            ((reducer fill) init-e vsub s ws))
          (lambda (f2 v2 s2 ws2) (k (fctx f2) (vctx v2) s2 ws2))))

;; run-multiple/visible
(define (run-multiple/visible reducer init-e1s fctx vsubs vctx fills s ws k)
  (DEBUG
    (printf "run-multiple/visible\n")
    (printf "  fctx: ~.s\n" (stx->datum (fctx (for/list ([dummy init-e1s]) #'HOLE))))
    (printf "  vctx: ~.s\n" (stx->datum (vctx (for/list ([dummy init-e1s]) #'HOLE))))
    (unless (= (length fills) (length init-e1s))
      (printf "  fills(~s): ~.s\n" (length fills) fills)
      (printf "  init-e1s: ~.s\n" (stx->datum init-e1s))
      (printf "  vsubs: ~.s\n" (stx->datum vsubs))))
  (let loop ([fills fills] [prefix null] [vprefix null] [suffix init-e1s] [vsuffix vsubs] [s s] [ws ws])
    (cond
     [(pair? fills)
      (RSbind (with-context (lambda (x) (vctx (revappend vprefix (cons x (cdr vsuffix)))))
                ((reducer (car fills)) (car suffix) (car vsuffix) s ws))
              (lambda (f2 v2 s2 ws2)
                (loop (cdr fills)
                      (cons f2 prefix)
                      (cons v2 vprefix)
                      (cdr suffix)
                      (cdr vsuffix)
                      s2
                      ws2)))]
     [(null? fills)
      (k (fctx (reverse prefix)) (vctx (reverse vprefix)) s ws)])))

;; run-multiple/nonvisible
(define (run-multiple/nonvisible reducer init-e1s fctx v fills s ws k)
  (DEBUG
    (printf "run-multiple/nonvisible\n")
    (printf "  fctx: ~.s\n" (stx->datum (fctx (for/list ([dummy init-e1s]) #'HOLE)))))
  (let loop ([fills fills] [prefix null] [suffix init-e1s] [v v] [s s] [ws ws])
    (DEBUG
      (printf "  v: ~.s\n" (stx->datum (datum->syntax #f v))))
    (cond
     [(pair? fills)
      (RSbind ((reducer (car fills)) (car suffix) v s ws)
              (lambda (f2 v2 s2 ws2)
                (loop (cdr fills)
                      (cons f2 prefix)
                      (cdr suffix)
                      v2
                      s2
                      ws2)))]
     [(null? fills)
      (k (fctx (reverse prefix)) v s ws)])))

;; ------------------------------------

;; CC
;; the context constructor
(define-syntax (CC stx)
  (syntax-case stx ()
    [(CC HOLE expr pattern)
     #'(syntax-copier HOLE expr pattern)]))

(define (revappend a b)
  (cond [(pair? a) (revappend (cdr a) (cons (car a) b))]
        [(null? a) b]))


;; visibility-off : boolean stx stx (-> a) -> a
(define (visibility-off new-visible? stx k [reset-subterms? #f])
  (cond [(and (not new-visible?) (or (visibility) reset-subterms?))
         (begin
           (DEBUG
            (printf "hide => seek: ~.s\n" (stx->datum stx)))
           (current-pass-hides? #t)
           (let* ([subterms (gather-proper-subterms stx)]
                  [marking (marking-table)]
                  [subterms
                   (if marking
                       (table-apply-renames-mapping
                        subterms
                        (table->renames-mapping marking)
                        #f)
                       subterms)])
             (parameterize ((visibility #f)
                            (subterms-table subterms)
                            (marking-table (or marking (make-hasheq))))
               (k))))]
        [else (k)]))

;; Seek

(provide/contract
 [seek-point (syntaxish? syntaxish? (syntaxish? . -> . RS/c) . -> . RS/c)])

;; seek-point : stx (-> RS/c) -> RS/c
(define (seek-point stx vstx k)
  (if (visibility)
      (k vstx)
      (begin
        (DEBUG (printf "Seek point\n")
               (print-viable-subterms stx))
        (let ([paths (table-get (subterms-table) stx)])
          (cond [(null? paths)
                 (DEBUG (printf "seek-point: failed on ~.s\n" (stx->datum stx)))
                 (k vstx)]
                [(null? (cdr paths))
                 (let ([path (car paths)])
                   (DEBUG (printf "seek => hide: ~.s\n" (stx->datum stx)))
                   (let ([ctx (lambda (x) (path-replace vstx path x))])
                     (RScase (parameterize ((visibility #t)
                                            (subterms-table #f)
                                            (marking-table #f))
                               ;; Found stx within vstx
                               (with-context ctx (k stx)))
                             (lambda (ws2 stx2 vstx2 s2)
                               (let ([vstx2 (ctx vstx2)])
                                 (RSunit ws2 stx2 vstx2 s2)))
                             (lambda (ws exn)
                               (RSfail ws exn)))))]
                [else
                 (raise (make nonlinearity stx paths))])))))

(provide print-viable-subterms)
(define (print-viable-subterms stx)
  (DEBUG
    (let ([t (subterms-table)])
      (when t
        (printf "viable subterms:\n")
        (let loop ([stx stx])
          (cond [(syntax? stx)
                 (let ([paths (table-get t stx)])
                   (if (pair? paths)
                       (printf "  ~s\n" (stx->datum stx))
                       (loop (syntax-e stx))))]
                [(pair? stx)
                 (loop (car stx))
                 (loop (cdr stx))]))))))

(define (check-same-stx function actual expected [derivs null])
  (unless (eq? actual expected)
    (let* ([actual-datum (stx->datum actual)]
           [expected-datum (stx->datum expected)]
           [same-form? (equal? actual-datum expected-datum)])
      (if same-form?
          (eprintf "same form but wrong wrappings:\n~.s\nwrongness:\n~.s\n"
                   actual-datum
                   (wrongness actual expected))
          (eprintf "got:\n~.s\n\nexpected:\n~.s\n"
                   actual-datum
                   expected-datum))
      (for ([d derivs]) (eprintf "\n~.s\n" d))
      (error function
             (if same-form?
                 "wrong starting point (wraps)!"
                 "wrong starting point (form)!")))))

(define (wrongness a b)
  (cond [(eq? a b)
         '---]
        [(stx-list? a)
         (map wrongness (stx->list a) (stx->list b))]
        [(stx-pair? a)
         (cons (wrongness (stx-car a) (stx-car b))
               (wrongness (stx-cdr a) (stx-cdr b)))]
        [else (stx->datum a)]))


;; flatten-identifiers : syntaxlike -> (list-of identifier)
(define (flatten-identifiers stx)
  (syntax-case stx ()
    [id (identifier? #'id) (list #'id)]
    [() null]
    [(x . y) (append (flatten-identifiers #'x) (flatten-identifiers #'y))]
    [else (error 'flatten-identifiers "neither syntax list nor identifier: ~s"
                 (if (syntax? stx)
                     (syntax->datum stx)
                     stx))]))
