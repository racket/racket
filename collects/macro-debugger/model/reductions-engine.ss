
#lang scheme/base

(require (for-syntax scheme/base)
         scheme/list
         scheme/contract
         "deriv.ss"
         "stx-util.ss"
         "steps.ss")
(provide (all-from-out "steps.ss")
         context
         big-context
         current-derivation
         current-definites
         learn-definites
         current-frontier
         add-frontier
         blaze-frontier
         rename-frontier
         with-context
         with-derivation
         with-new-local-context

         RSunit
         RSzero
         RSbind
         RSadd
         RSseq
         RSforeach
         RS-steps

         CC
         R
         revappend

         walk
         walk/foci
         walk/mono
         stumble
         stumble/E)

;; FIXME: Steps are pairs of Configurations
;; Configurations contain contexts, definites, etc.

;; context: parameter of Context
(define context (make-parameter null))

;; big-context: parameter of BigContext
(define big-context (make-parameter null))

;; current-derivation : parameter of Derivation
(define current-derivation (make-parameter #f))

;; current-definites : parameter of (list-of identifier)
(define current-definites (make-parameter null))

;; current-frontier : parameter of (list-of syntax)
(define current-frontier (make-parameter null))

(define-syntax with-context
  (syntax-rules ()
    [(with-context f . body)
     (let ([c (context)])
       (parameterize ([context (cons f c)])
         (let () . body)))]))

(define-syntax with-derivation
  (syntax-rules ()
    [(with-derivation d . body)
     (parameterize ((current-derivation d)) . body)]))

(define-syntax with-new-local-context
  (syntax-rules ()
    [(with-new-local-context e . body)
     (parameterize ([big-context
                     (cons (make-bigframe (current-derivation) (context) (list e) e)
                           (big-context))]
                    [context null])
       . body)]))

(define (learn-definites ids)
  (current-definites
   (append ids (current-definites))))

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

;; -----------------------------------

;; RS: The "reductions monad"
;; (RS a) = (values ReductionSequence ?a ?exn)
;; Not a proper monad, because of 'values'

(define-syntax ->RS/c
  (syntax-rules ()
    [(->RS/c domain-c ...)
     (-> domain-c ...
         (values (listof protostep?) any/c (or/c exn? false/c)))]))

(define/contract RSzero
  (->RS/c)
  (lambda () (values null #f #f)))

(define/contract RSunit
  (->RS/c any/c)
  (lambda (v)
    (values null v #f)))

(define/contract RSbind
  (->RS/c (->RS/c) (->RS/c any/c))
  (lambda (a f)
    (let-values ([(rseq1 final1 exn1) (a)])
      (if (not exn1)
          (let-values ([(rseq2 final2 exn2) (f final1)])
            (values (append rseq1 rseq2) final2 exn2))
          (values rseq1 final1 exn1)))))

(define/contract RSseq
  (->RS/c (->RS/c) (->RS/c))
  (lambda (a b)
    (RSbind a (lambda (_) (b)))))

(define/contract RSforeach
  (->RS/c (->RS/c any/c) (listof any/c))
  (lambda (f xs)
    (let loop ([xs xs])
      (if (pair? xs)
          (RSseq (lambda () (f (car xs)))
                 (lambda () (loop (cdr xs))))
          (RSunit (void))))))

(define/contract RSadd
  (->RS/c (listof protostep?) (->RS/c))
  (lambda (steps a)
    (let-values ([(rseq1 final1 exn1) (a)])
      (values (append steps rseq1) final1 exn1))))

(define-syntax RS-steps
  (syntax-rules ()
    [(RS-steps expr)
     (let-values ([(rseq final exn) expr])
       rseq)]))

;; CC
;; the context constructor
(define-syntax (CC stx)
  (syntax-case stx ()
    [(CC HOLE expr pattern)
     #'(syntax-copier HOLE expr pattern)]))

;; R
;; the threaded reductions engine

;; (R stx R-clause ...) : (values (list-of Step) ?stx ?exn)
;; An R-clause is one of
;;   [! expr]
;;   [#:pattern pattern]
;;   [#:bind pattern stx-expr]
;;   [#:let-values (var ...) expr]
;;   [#:walk term2 description]
;;   [#:walk/ctx pattern term2 description]
;;   [#:walk/foci term2 foci1 foci2 description]
;;   [#:rename* pattern rename [description]]
;;   [#:rename/no-step pattern stx stx]
;;   [#:reductions expr]
;;   [#:learn ids]
;;   [#:frontier stxs]
;;   [#:when test R-clause ...]
;;   [#:if/np test R-clause ...]
;;   [generator hole fill]

(define-syntax R
  (syntax-rules ()
    [(R form . clauses)
     (let ([form-var form])
       (R** form-var _ . clauses))]))

(define-syntax R**
  (syntax-rules (! =>)
    ;; Base: done
    [(R** form-var pattern)
     (RSunit form-var)]

    ;; Base: explicit continuation
    [(R** f p => k)
     (k f)]

    ;; Error-point case
    [(R** f p [! maybe-exn] . more)
     (let ([x maybe-exn])
       (unless (or (not x) (exn? x))
         (raise-type-error 'R "exception" x))
       (if x
           (values (list (stumble f x)) #f x)
           (R** f p . more)))]

    ;; Change patterns
    [(R** f p [#:pattern p2] . more)
     (R** f p2 . more)]

    ;; Bind pattern variables
    [(R** f p [#:bind pattern rhs] . more)
     (with-syntax ([pattern (with-syntax ([p f]) rhs)])
       (R** f p . more))]

    ;; Bind variables
    [(R** f p [#:let-values (var ...) rhs] . more)
     (let-values ([(var ...) (with-syntax ([p f]) rhs)])
       (R** f p . more))]

    ;; Change syntax
    [(R** f p [#:set-syntax form] . more)
     (let ([form-variable form])
       (R** form-variable p . more))]

    ;; Change syntax and Step (infer foci)
    [(R** f p [#:walk form2 description] . more)
     (let-values ([(form2-var description-var)
                   (with-syntax ([p f])
                     (values form2 description))])
       (RSadd (list (walk f form2-var description-var))
              (lambda () (R** form2-var p . more))))]

    ;; Change syntax and Step (explicit foci)
    [(R** f p [#:walk/foci form2 foci1 foci2 description] . more)
     (let-values ([(form2-var foci1-var foci2-var description-var)
                   (with-syntax ([p f])
                     (values form2 foci1 foci2 description))])
       (RSadd (list (walk/foci foci1-var foci2-var f form2-var description-var))
              (lambda () (R** form2-var p . more))))]

    [(R** f p [#:walk/ctx hole form2 desc] . more)
     (let-values ([(form2-var desc-var)
                   (with-syntax ([p f])
                     (values form2 desc))])
       (let ([k (lambda (f2) (R** f2 p . more))]
             [generator
              (lambda ()
                (lambda (d init-e1)
                  (R init-e1
                     [#:walk form2-var desc-var])))])
         (Run f p generator hole form2 k)))]

    ;; Rename
    [(R** f p [#:rename* pattern renames] . more)
     (R** f p [#:rename* pattern renames #f] . more)]

    [(R** f p [#:rename* pattern renames description] . more)
     (let-values ([(renames-var description-var)
                   (with-syntax ([p f])
                     (values renames description))])
       (let ([pre-renames-var
              (with-syntax ([p f]) (syntax pattern))]
             [f2
              (with-syntax ([p f])
                (with-syntax ([pattern renames])
                  (syntax p)))])
         (rename-frontier pre-renames-var renames-var)
         (with-context (make-renames pre-renames-var renames-var)
           (RSadd (if description-var
                      (list (walk/foci pre-renames-var renames-var
                                       f f2
                                       description-var))
                      null)
                  (lambda () (R** f2 p . more))))))]

    ;; Change syntax with rename
    #;
    [(R** f p [#:rename form2 foci1 foci2 description] . more)
     (let-values ([(form2-var foci1-var foci2-var description-var)
                   (with-syntax ([p f])
                     (values form2 foci1 foci2 description))])
       (rename-frontier f form2-var)
       (with-context (make-renames foci1-var foci2-var)
         (RSadd (list (walk/foci foci1-var foci2-var
                                 f form2-var
                                 description-var))
                (lambda () (R** form2-var p . more)))))]

    ;; Change syntax with rename (but no step)
    [(R** f p [#:rename/no-step pvar from to] . more)
     (let-values ([(from-var to-var)
                   (with-syntax ([p f]) (values from to))])
       (let ([f2 (with-syntax ([p f])
                   (with-syntax ([pvar to])
                     (syntax p)))])
         (rename-frontier from-var to-var)
         (with-context (make-renames from-var to-var)
           (R** f2 p . more))))]

    ;; Add in arbitrary other steps
    [(R** f p [#:reductions steps] . more)
     (RSseq (lambda () steps)
            (lambda () (R** f p . more)))]

    ;; Add to definites
    [(R** f p [#:learn ids] . more)
     (begin (learn-definites (with-syntax ([p f]) ids))
            (R** f p . more))]

    ;; Add to frontier
    [(R** f p [#:frontier stxs] . more)
     (begin (add-frontier (with-syntax ([p f]) stxs))
            (R** f p . more))]

    ;; Conditional (pattern changes lost afterwards ...)
    [(R** f p [#:if/np test [consequent ...] [alternate ...]] . more)
     (let ([continue (lambda (f2) (R** f2 p . more))])
       (if (with-syntax ([p f]) test)
           (R** f p consequent ... => continue)
           (R** f p alternate ... => continue)))]

    ;; Conditional (pattern changes lost afterwards ...)
    [(R** f p [#:when/np test consequent ...] . more)
     (let ([continue (lambda (f2) (R** f2 p . more))])
       (if (with-syntax ([p f]) test)
           (R** f p consequent ... => continue)
           (continue f)))]

    ;; Conditional
    [(R** f p [#:when test consequent ...] . more)
     (if (with-syntax ([p f]) test)
         (R** f p consequent ... . more)
         (R** f p . more))]

    ;; Subterm handling
    [(R** f p [generator hole fill] . more)
     (let ([k (lambda (f2) (R** f2 p . more))])
       (Run f p generator hole fill k))]))


(define-syntax Run 
  (syntax-rules ()
    [(Run f p generator hole fill k)
     (let ([reducer (generator)])
       (Run* reducer f p hole fill k))]))

(define-syntax (Run* stx)
  (syntax-case stx ()
    ;; Implementation of subterm handling for (hole ...) sequences
    [(Run* reducer f p (hole :::) fills k)
     (and (identifier? #':::)
          (free-identifier=? #'::: (quote-syntax ...)))
     #'(let ([ctx (CC (hole :::) f p)])
         (let ([e1s (with-syntax ([p f]) (syntax->list #'(hole :::)))])
           (run-multiple reducer ctx fills e1s k)))]
    ;; Implementation of subterm handling
    [(Run* reducer f p hole fill k)
     #'(let ([init-e (with-syntax ([p f]) #'hole)]
             [ctx (CC hole f p)])
         (run-one reducer init-e ctx fill k))]))

;; run-one : (a stx -> RS(b)) stx (b -> c) (c -> RS(d)) -> RS(d)
(define (run-one f init-e ctx fill k)
  (RSbind (lambda () (with-context ctx (f fill init-e)))
          (lambda (final) (k (ctx final)))))

;; run-multiple : (a -> RS(b)) ((list-of b) -> c) (list-of a) (list-of b) (c -> RS(d))
;;             -> RS(d)
;; For example: a = Deriv; b = c = d = Syntax
(define (run-multiple f ctx fills suffix k)
  (let loop ([fills fills] [prefix null] [suffix suffix])
    (cond
     [(pair? fills)
      (RSbind (lambda ()
                (with-context ctx
                  (with-context (lambda (x) (revappend prefix (cons x (cdr suffix))))
                    (f (car fills) (car suffix)))))
              (lambda (final)
                (loop (cdr fills)
                      (cons final prefix)
                      (cdr suffix))))]
     [(null? fills)
      (let ([form (ctx (reverse prefix))])
        (k form))])))

;; Rename mapping

(define (rename-frontier from to)
  (current-frontier
   (with-handlers ([exn:fail? (lambda _ #f)])
     (apply append
            (map (make-rename-mapping from to)
                 (current-frontier))))))

(define (make-rename-mapping from0 to0)
  (define table (make-hasheq))
  (let loop ([from from0] [to to0])
    (cond [(syntax? from)
           (hash-set! table from (flatten-syntaxes to))
           (loop (syntax-e from) to)]
          [(syntax? to)
           (loop from (syntax-e to))]
          [(pair? from)
           #;
           (unless (pair? to)
             (fprintf (current-error-port)
                      "from:\n~s\n\n" (syntax->datum from0))
             (fprintf (current-error-port)
                      "to:\n~s\n\n" (syntax->datum to0))
             (error 'frontier-renaming))
           (loop (car from) (car to))
           (loop (cdr from) (cdr to))]
          [(vector? from)
           (loop (vector->list from) (vector->list to))]
          [(box? from)
           (loop (unbox from) (unbox to))]
          [else (void)]))
  (lambda (stx)
    (let ([replacement (hash-ref table stx #f)])
      (if replacement
          (begin #;(printf "  replacing ~s with ~s~n" stx replacement)
                 replacement)
          (begin #;(printf "  not replacing ~s~n" stx)
                 (list stx))))))

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

;; walk : syntax(es) syntax(es) StepType -> Reduction
;; Lifts a local step into a term step.
(define (walk e1 e2 type)
  (make-step (current-derivation) (big-context) type (context)
             (current-definites) (get-frontier)
             (foci e1) (foci e2) e1 e2))

;; walk/foci : syntaxes syntaxes syntax syntax StepType -> Reduction
(define (walk/foci foci1 foci2 Ee1 Ee2 type)
  (make-step (current-derivation) (big-context) type (context)
             (current-definites) (get-frontier)
             (foci foci1) (foci foci2) Ee1 Ee2))

;; walk/mono : syntax StepType -> Reduction
(define (walk/mono e1 type)
  (make-mono (current-derivation) (big-context) type (context)
             (current-definites) (get-frontier)
             (foci e1) e1))

;; stumble : syntax exception -> Reduction
(define (stumble stx exn)
  (make-misstep (current-derivation) (big-context) 'error (context)
                (current-definites) (get-frontier)
                (foci stx) stx exn))

;; stumble/E : syntax(s) syntax exn -> Reduction
(define (stumble/E focus Ee1 exn)
  (make-misstep (current-derivation) (big-context) 'error (context)
                (current-definites) (get-frontier)
                (foci focus) Ee1 exn))

;; ------------------------------------

(define (revappend a b)
  (cond [(pair? a) (revappend (cdr a) (cons (car a) b))]
        [(null? a) b]))

(define (foci x)
  (if (list? x)
      x
      (list x)))
