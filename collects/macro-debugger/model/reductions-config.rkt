#lang racket/base
(require (for-syntax racket/base)
         racket/contract/base
         racket/match
         "../util/eomap.rkt"
         "deriv-util.rkt"
         "stx-util.rkt"
         "context.rkt"
         "steps.rkt")

(define-syntax-rule (STRICT-CHECKS form ...)
  (when #f
    form ... (void)))

(define state/c (or/c state? false/c))
(define context/c any/c)
(define big-context/c any/c)

(define (parameterlike/c c)
  (case-> [-> c] [c . -> . any/c]))

(define (list-parameter/c c)
  (parameter/c (listof (box/c c))))

(define subterms-table/c hash?)

(define-syntax-rule (provide/contract* [name c] ...)
  #;(provide/contract [name c] ...)
  (provide name ...))

(provide/contract*
 [state/c contract?]
 [context (parameter/c context/c)]
 [big-context (parameter/c big-context/c)]
 [marking-table (parameter/c (or/c hash? false/c))]
 [current-binders (parameter/c (listof identifier?))]
 [current-definites (parameter/c eomap?)] ;; eomap[identifier => phase-level]
 [current-binders (parameter/c hash?)] ;; hash[identifier => phase-level]
 [current-frontier (parameter/c (listof syntax?))]
 [sequence-number (parameter/c (or/c false/c exact-nonnegative-integer?))]
 [phase (parameter/c exact-nonnegative-integer?)]
 [visibility (parameter/c boolean?)]
 [macro-policy (parameter/c (identifier? . -> . any))]
 [subterms-table (parameter/c (or/c subterms-table/c false/c))]
 [hides-flags (list-parameter/c boolean?)]

 [learn-binders ((listof identifier?) . -> . any)]
 [learn-definites ((listof identifier?) . -> . any)]

 [add-frontier ((listof syntax?) . -> . any)]
 [blaze-frontier (syntax? . -> . any)]

 [current-state-with (syntaxish? syntaxish? . -> . state?)]
 [walk ([syntaxish? syntaxish? symbol?]
        [#:foci1 syntaxish? #:foci2 syntaxish?]
        . ->* . step?)]
 [stumble ([syntaxish? exn?] [#:focus syntaxish?] . ->* . misstep?)]
 [walk/talk
  (-> (or/c symbol? string?) (listof (or/c syntax? string? 'arrow))
      remarkstep?)]

 [current-pass-hides? (parameterlike/c boolean?)]

 [available-lift-stxs (parameter/c (listof syntaxish?))]
 [visible-lift-stxs (parameter/c (listof syntaxish?))])

(provide with-context
         with-new-local-context)

;; FIXME: Steps are pairs of Configurations
;; Configurations contain contexts, definites, etc.

;; Classical Parameters

;; context: parameter of Context
(define context (make-parameter null))

;; big-context: parameter of BigContext
(define big-context (make-parameter null))

;; marking-table
(define marking-table (make-parameter #f))

;; current-binders : parameter of hash[identifier => phase-level]
(define current-binders (make-parameter #f))

;; current-definites : parameter of eomap[identifier => phase-level]
(define current-definites (make-parameter #f))

;; current-frontier : parameter of (list-of syntax)
(define current-frontier (make-parameter null))

;; sequence-number : parameter of nat
(define sequence-number (make-parameter #f))

;; New Hiding Parameters

;; visibility : (parameterof boolean)
(define visibility (make-parameter #t))

;; macro-policy : (parameterof (identifier -> boolean))
(define macro-policy (make-parameter (lambda (id) #t)))

;; phase : (parameterof nat)
(define phase (make-parameter 0))

;; subterms-table : parameter of hash[syntax => (list-of Path)]
(define subterms-table (make-parameter #f))

;; hides-flags : (parameterof (listof (boxof boolean)))
(define hides-flags (make-parameter null))

;; lift params
(define available-lift-stxs (make-parameter null))
(define visible-lift-stxs (make-parameter null))

;; Hiding Structures

(provide (struct-out hiding-failure)
         (struct-out nonlinearity)
         (struct-out localactions)
         (struct-out hidden-lift-site))

;; Machinery for reporting things that macro hiding can't handle
(define-struct hiding-failure ())
(define-struct (nonlinearity hiding-failure) (term paths))
(define-struct (localactions hiding-failure) ())
(define-struct (hidden-lift-site hiding-failure) ())

;; Operations

(define-syntax with-context
  (syntax-rules ()
    [(with-context f . body)
     (let ([c (context)])
       (parameterize ([context
                       (if (visibility)
                           (cons f c)
                           c)])
         (let () . body)))]))

(define-syntax with-new-local-context
  (syntax-rules ()
    [(with-new-local-context e . body)
     (parameterize ([big-context
                     (cons (make bigframe (context) (list e) e)
                           (big-context))]
                    [context null])
       . body)]))

(define (learn-definites ids)
  (current-definites
   (eomap-set* (current-definites) ids (phase))))

(define (learn-binders ids)
  (current-binders
   (for/fold ([binders (current-binders)]) ([id (in-list ids)])
     (hash-set binders id (phase)))))

(define (get-frontier) (or (current-frontier) null))

(define (add-frontier stxs)
  (current-frontier
   (let ([frontier0 (current-frontier)])
     (and frontier0 (append stxs frontier0)))))

(define (blaze-frontier stx)
  (current-frontier
   (let ([frontier0 (current-frontier)])
     (and frontier0
          (remq stx frontier0)))))

;; Renames mapping

(define renames-mapping/c
  ([syntax?] [#:allow-nonstx? boolean? #:default any/c] . ->* . any))

(provide/contract*
 [renames-mapping/c contract?]
 [make-renames-mapping
  (syntaxish? syntaxish? . -> . renames-mapping/c)]
 [compose-renames-mappings
  (renames-mapping/c renames-mapping/c . -> . renames-mapping/c)]
 [apply-renames-mapping (renames-mapping/c syntaxish? . -> . syntaxish?)]

 [table->renames-mapping
  (hash? . -> . renames-mapping/c)]
 [make-renames-table
  (syntaxish? syntaxish? . -> . hash?)]
 [add-to-renames-table
  (hash? syntaxish? syntaxish? . -> . any)]

 [rename-frontier/mapping
  (renames-mapping/c . -> . any)])

(define (rename-frontier/mapping mapping)
  (current-frontier
   (with-handlers ([exn:fail? (lambda _ #f)])
     (for/list ([fstx (current-frontier)])
       (let ([renamed-fstx (mapping fstx #:allow-nonstx? #t #:default null)])
         (flatten-syntaxes renamed-fstx))))))

;; apply-renames-mapping : (stx -> stx) stx -> stx
(define (apply-renames-mapping mapping stx)
  (cond [(and (syntax? stx)
              (mapping stx #:allow-nonstx? #t #:default #f))
         => (lambda (rstx)
              (datum->syntax stx rstx stx stx))]
        [(syntax? stx)
         (let* ([inner (syntax-e stx)]
                [rinner (apply-renames-mapping mapping inner)])
           (if (eq? rinner inner)
               stx
               (datum->syntax stx rinner stx stx)))]
        [(pair? stx)
         (let ([ra (apply-renames-mapping mapping (car stx))]
               [rb (apply-renames-mapping mapping (cdr stx))])
           (if (and (eq? ra (car stx)) (eq? rb (cdr stx)))
               stx
               (cons ra rb)))]
        [(vector? stx)
         (let* ([elems (vector->list stx)]
                [relems (apply-renames-mapping mapping elems)])
           (if (eq? relems elems)
               stx
               (list->vector relems)))]
        [(box? stx)
         (let* ([inner (unbox stx)]
                [rinner (apply-renames-mapping mapping inner)])
           (if (eq? rinner inner)
               stx
               (box inner)))]
        [(prefab-struct-key stx)
         (let* ([inner (struct->vector stx)]
                [rinner (apply-renames-mapping mapping inner)])
           (if (eq? rinner inner)
               stx
               (apply make-prefab-struct
                      (prefab-struct-key stx)
                      (cdr (vector->list rinner)))))]
        [else stx]))

;; make-renames-mapping : stx stx -> stx kw-args -> stx
(define (make-renames-mapping from0 to0)
  (define table (make-renames-table from0 to0))
  (table->renames-mapping table))

(define (table->renames-mapping table)
  (lambda (stx #:allow-nonstx? [allow-nonstx? #f] #:default [default #f])
    (let ([replacement (hash-ref table stx #f)])
      (if replacement
          (begin #;(printf "  replacing ~s with ~s\n" stx replacement)
                 replacement)
          (begin #;(printf "  not replacing ~s\n" stx)
                 default)))))

(define (make-renames-table from0 to0)
  (define table (make-hasheq))
  (add-to-renames-table table from0 to0)
  table)

(define (add-to-renames-table table from0 to0)
  (let loop ([from from0] [to to0])
    (cond [(and (syntax? from) (syntax? to))
           (hash-set! table from to)
           (loop (syntax-e from) (syntax-e to))]
          [(syntax? from)
           (hash-set! table from to)
           (loop (syntax-e from) to)]
          [(syntax? to)
           (loop from (syntax-e to))]
          [(and (pair? from) (pair? to))
           (loop (car from) (car to))
           (loop (cdr from) (cdr to))]
          [(and (vector? from) (vector? to))
           (loop (vector->list from) (vector->list to))]
          [(and (box? from) (box? to))
           (loop (unbox from) (unbox to))]
          [(and (struct? from) (struct? to))
           (loop (struct->vector from) (struct->vector to))]
          [(eqv? from to)
           (void)]
          [else
           ;; FIXME: bad rename indicates something out of sync
           ;; But for now, just drop it to avoid macro stepper error.
           ;; Only bad effect should be missed subterms (usually at phase1).
           (STRICT-CHECKS
            (eprintf "from:\n~.s\n\nto:\n~.s\n\n"
                     (stx->datum from)
                     (stx->datum to))
            (eprintf "original from:\n~.s\n\noriginal to:\n~.s\n\n"
                     (stx->datum from0)
                     (stx->datum to0))
            (error 'add-to-renames-table))
           (void)])))

(define (compose-renames-mappings first second)
  (lambda (stx #:allow-nonstx? [allow-nonstx? #f] #:default [default #f])
    (let ([r (first stx #:allow-nonstx? allow-nonstx? #:default #f)])
      (if r
          (second r #:allow-nonstx? allow-nonstx? #:default default)
          default))))

(define (flatten-syntaxes x)
  (cond [(syntax? x)
         (list x)]
        [(pair? x)
         (append (flatten-syntaxes (car x))
                 (flatten-syntaxes (cdr x)))]
        [(vector? x)
         (flatten-syntaxes (vector->list x))]
        [(box? x)
         (flatten-syntaxes (unbox x))]
        [else null]))

;; -----------------------------------

(define (current-state-with e fs)
  (make state e (foci fs) (context) (big-context)
        (current-binders) (current-definites)
        (current-frontier) (sequence-number)))

(define (walk e1 e2 type
              #:foci1 [foci1 e1]
              #:foci2 [foci2 e2])
  (make step type
        (current-state-with e1 foci1)
        (current-state-with e2 foci2)))

(define (stumble stx exn
                 #:focus [focus stx])
  (make misstep 'error
        (current-state-with stx focus)
        exn))

(define (walk/talk type contents)
  (make remarkstep type
        (current-state-with #f null)
        contents))

(define (foci x)
  (cond [(syntax? x)
         (list x)]
        [(null? x)
         null]
        [(pair? x)
         (append (foci (car x))
                 (foci (cdr x)))]))


;; RS: the reductions monad

;; Datastructure RS
;; Better for debugging

;; RS = (rsok ReductionSequence stx stx state)
;;    | (rsfailed ReductionSequence exn)

(define-struct rsok (rs real vis s))
(define-struct rsfailed (rs exn))

(define RS/c
  (lambda (x)
    (or (rsok? x) (rsfailed? x))))

(define (RSunit steps x y s) (make rsok steps x y s))

(define (RSfail steps exn) (make rsfailed steps exn))

(define (RSbind a f)
  (match a
    [(struct rsok (rs a b s))
     (f a b s rs)]
    [(struct rsfailed (rs exn))
     a]))

(define (RScase a k f)
  (match a
    [(struct rsok (rs a b s))
     (k rs a b s)]
    [(struct rsfailed (rs exn))
     (f rs exn)]))

(provide RS/c)
(provide/contract*
 [RSunit ((listof protostep?) any/c any/c state/c . -> . RS/c)]
 [RSfail ((listof protostep?) exn? . -> . RS/c)]
 [RSbind (RS/c (any/c any/c state/c (listof protostep?) . -> . RS/c) . -> . RS/c)]
 [RScase (RS/c
          ((listof protostep?) any/c any/c state/c . -> . any)
          ((listof protostep?) exn? . -> . any)
          . -> . any)])

#|
;; Alternate RS = (values ?exn steps ?stx ?stx state)
;; Avoids allocation
;; Doesn't seem to actually matter

(define (RSunit ws x y s)
  (values #f ws x y s))

(define (RSfail ws e)
  (values e ws #f #f #f))

(define-syntax-rule (RSbind a f)
  (let-values ([(e ws x y s) a])
    (if (not e)
        (f x y s ws)
        (values e ws x y s))))

(define-syntax-rule (RScase a k f)
  (let-values ([(e ws x y s) a])
    (if (not e)
        (k ws x y s)
        (f ws e))))

(define-syntax RS/c (make-rename-transformer #'any/c))

(provide RS/c
         RSunit
         RSfail
         RSbind
         RScase)
|#


;; Table

(provide/contract*
 [gather-proper-subterms (syntaxish? . -> . subterms-table/c)]
 [table-get (subterms-table/c syntax? . -> . list?)]
 [table-apply-renames-mapping
  ((or/c subterms-table/c false/c) renames-mapping/c boolean?
   . -> . (or/c subterms-table/c false/c))])

;; gather-proper-subterms : Syntax -> SubtermTable
;; FIXME: Eventually, need to descend into vectors, boxes, etc.
(define (gather-proper-subterms stx0)
  (define (table-add! table stx v)
    (hash-set! table stx (cons v (table-get table stx))))
  (define (table-get table stx)
    (hash-ref table stx null))
  (let ([table (make-hasheq)])
    ;; loop : Syntax Path -> void
    (define (loop stx rpath)
      (unless (eq? stx0 stx)
        (table-add! table stx (reverse rpath)))
      (let ([p (if (syntax? stx) (syntax-e stx) stx)])
        (when (pair? p)
          (loop-cons p rpath 0))))
    ;; loop-cons : (cons Syntax ?) Path number -> void
    (define (loop-cons p rpath pos)
      (loop (car p) (cons (make ref pos) rpath))
      (let ([t (cdr p)])
        (cond [(syntax? t)
               (let ([te (syntax-e t)])
                 (if (pair? te)
                     (begin
                       (table-add! table t (reverse (cons (make tail pos) rpath)))
                       (loop-cons te rpath (add1 pos)))
                     (loop t (cons (make tail pos) rpath))))]
              [(pair? t)
               (loop-cons t rpath (add1 pos))]
              [(null? t)
               (void)])))
    (loop stx0 null)
    table))

;; table-get : Table stx -> (listof Path)
(define (table-get t x)
  (hash-ref t x null))

;; table-apply-renames-mapping boolean : Table (stx -> stx) -> Table
(define (table-apply-renames-mapping old mapping whole-form-rename?)
  (and old
       (let ([t (make-hasheq)])
         (hash-for-each
          old
          (if whole-form-rename?
              (lambda (stx paths)
                (let ([rstx (mapping stx #:default #f)])
                  (when rstx
                    (hash-set! t rstx paths))))
              (lambda (stx paths)
                (let ([rstx (mapping stx #:default stx)])
                  (hash-set! t rstx paths)))))
         t)))

;; list-parameter->parameterlike : (list-parameter/c X) -> (parameterlike X)
(define (list-parameter->parameterlike p)
  (case-lambda
    [() (unbox (car (p)))]
    [(v) (set-box! (car (p)) v)]))

;; current-pass-hides?
(define current-pass-hides? (list-parameter->parameterlike hides-flags))
