#lang racket/base
(require (for-syntax racket/base
                     racket/lazy-require
                     "sc.rkt"
                     "lib.rkt"
                     "kws.rkt"
                     racket/syntax
                     syntax/private/keyword)
         syntax/parse/private/residual-ct ;; keep abs. path
         syntax/parse/private/residual    ;; keep abs. path
         (only-in unstable/syntax phase-of-enclosing-module))
(begin-for-syntax
 (lazy-require
  [syntax/parse/private/rep ;; keep abs. path
   (parse-kw-formals
    check-conventions-rules
    check-datum-literals-list
    create-aux-def)]))
;; FIXME: workaround for phase>0 bug in racket/runtime-path (and thus lazy-require)
;; Without this, dependencies don't get collected.
(require racket/runtime-path (for-meta 2 '#%kernel))
(define-runtime-module-path-index _unused_ 'syntax/parse/private/rep)

(provide define-conventions
         define-literal-set
         literal-set->predicate
         kernel-literals)

(define-syntax (define-conventions stx)

  (define-syntax-class header
    #:description "name or name with formal parameters"
    #:commit
    (pattern name:id
             #:with formals #'()
             #:attr arity (arity 0 0 null null))
    (pattern (name:id . formals)
             #:attr arity (parse-kw-formals #'formals #:context stx)))

  (syntax-parse stx
    [(define-conventions h:header rule ...)
     (let ()
       (define rules (check-conventions-rules #'(rule ...) stx))
       (define rxs (map car rules))
       (define dens0 (map cadr rules))
       (define den+defs-list
         (for/list ([den0 (in-list dens0)])
           (let-values ([(den defs) (create-aux-def den0)])
             (cons den defs))))
       (define dens (map car den+defs-list))
       (define defs (apply append (map cdr den+defs-list)))

       (define/with-syntax (rx ...) rxs)
       (define/with-syntax (def ...) defs)
       (define/with-syntax (parser ...)
         (map den:delayed-parser dens))
       (define/with-syntax (class-name ...)
         (map den:delayed-class dens))

       ;; FIXME: could move make-den:delayed to user of conventions
       ;; and eliminate from residual.rkt
       #'(begin
           (define-syntax h.name
             (make-conventions
              (quote-syntax get-parsers)
              (lambda ()
                (let ([class-names (list (quote-syntax class-name) ...)])
                  (map list
                       (list 'rx ...)
                       (map make-den:delayed
                            (generate-temporaries class-names)
                            class-names))))))
           (define get-parsers
             (lambda formals
               def ...
               (list parser ...)))))]))

(define-for-syntax (check-phase-level stx ctx)
  (unless (or (exact-integer? (syntax-e stx))
              (eq? #f (syntax-e stx)))
    (raise-syntax-error #f "expected phase-level (exact integer or #f)" ctx stx))
  stx)

;; check-litset-list : stx stx -> (listof (cons id literalset))
(define-for-syntax (check-litset-list stx ctx)
  (syntax-case stx ()
    [(litset-id ...)
     (for/list ([litset-id (syntax->list #'(litset-id ...))])
       (let* ([val (and (identifier? litset-id)
                        (syntax-local-value/record litset-id literalset?))])
         (if val
             (cons litset-id val)
             (raise-syntax-error #f "expected literal set name" ctx litset-id))))]
    [_ (raise-syntax-error #f "expected list of literal set names" ctx stx)]))

;; check-literal-entry/litset : stx stx -> (list id id)
(define-for-syntax (check-literal-entry/litset stx ctx)
  (syntax-case stx ()
    [(internal external)
     (and (identifier? #'internal) (identifier? #'external))
     (list #'internal #'external)]
    [id
     (identifier? #'id)
     (list #'id #'id)]
    [_ (raise-syntax-error #f "expected literal entry" ctx stx)]))

(define-for-syntax (check-duplicate-literals ctx imports lits datum-lits)
  (let ([lit-t (make-hasheq)]) ;; sym => #t
    (define (check+enter! key blame-stx)
      (when (hash-ref lit-t key #f)
        (raise-syntax-error #f (format "duplicate literal: ~a" key) ctx blame-stx))
      (hash-set! lit-t key #t))
    (for ([id+litset (in-list imports)])
      (let ([litset-id (car id+litset)]
            [litset (cdr id+litset)])
        (for ([entry (in-list (literalset-literals litset))])
          (cond [(lse:lit? entry)
                 (check+enter! (lse:lit-internal entry) litset-id)]
                [(lse:datum-lit? entry)
                 (check+enter! (lse:datum-lit-internal entry) litset-id)]))))
    (for ([datum-lit (in-list datum-lits)])
      (let ([internal (den:datum-lit-internal datum-lit)])
        (check+enter! (syntax-e internal) internal)))
    (for ([lit (in-list lits)])
      (check+enter! (syntax-e (car lit)) (car lit)))))

(define-syntax (define-literal-set stx)
  (syntax-case stx ()
    [(define-literal-set name . rest)
     (let-values ([(chunks rest)
                   (parse-keyword-options
                    #'rest
                    `((#:literal-sets ,check-litset-list)
                      (#:datum-literals ,check-datum-literals-list)
                      (#:phase ,check-phase-level)
                      (#:for-template)
                      (#:for-syntax)
                      (#:for-label))
                    #:incompatible '((#:phase #:for-template #:for-syntax #:for-label))
                    #:context stx
                    #:no-duplicates? #t)])
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected identifier" stx #'name))
       (let ([relphase
              (cond [(assq '#:for-template chunks) -1]
                    [(assq '#:for-syntax chunks) 1]
                    [(assq '#:for-label chunks) #f]
                    [else (options-select-value chunks '#:phase #:default 0)])]
             [datum-lits
              (options-select-value chunks '#:datum-literals #:default null)]
             [lits (syntax-case rest ()
                     [( (lit ...) )
                      (for/list ([lit (in-list (syntax->list #'(lit ...)))])
                        (check-literal-entry/litset lit stx))]
                     [_ (raise-syntax-error #f "bad syntax" stx)])]
             [imports (options-select-value chunks '#:literal-sets #:default null)])
         (check-duplicate-literals stx imports lits datum-lits)
         (with-syntax ([((internal external) ...) lits]
                       [(datum-internal ...) (map den:datum-lit-internal datum-lits)]
                       [(datum-external ...) (map den:datum-lit-external datum-lits)]
                       [(litset-id ...) (map car imports)]
                       [relphase relphase])
           #`(begin
               (define phase-of-literals
                 (and 'relphase
                      (+ (variable-reference->module-base-phase (#%variable-reference))
                         'relphase)))
               (define-syntax name
                 (make-literalset
                  (append (literalset-literals (syntax-local-value (quote-syntax litset-id)))
                          ...
                          (list (make-lse:lit 'internal
                                              (quote-syntax external)
                                              (quote-syntax phase-of-literals))
                                ...
                                (make-lse:datum-lit 'datum-internal
                                                    'datum-external)
                                ...))))
               (begin-for-syntax/once
                (for ([x (in-list (syntax->list #'(external ...)))])
                  (unless (identifier-binding x 'relphase)
                    (raise-syntax-error #f
                                        (format "literal is unbound in phase ~a~a~a"
                                                'relphase
                                                (case 'relphase
                                                  ((1) " (for-syntax)")
                                                  ((-1) " (for-template)")
                                                  ((#f) " (for-label)")
                                                  (else ""))
                                                " relative to the enclosing module")
                                        (quote-syntax #,stx) x))))))))]))

#|
NOTES ON PHASES AND BINDINGS

(module M ....
  .... (define-literal-set LS #:phase PL ....)
  ....)

For the expansion of the define-literal-set form, the bindings of the literals
can be accessed by (identifier-binding lit PL), because the phase of the enclosing
module (M) is 0.

LS may be used, however, in a context where the phase of the enclosing
module is not 0, so each instantiation of LS needs to calculate the
phase of M and add that to PL.

--

Normally, literal sets that define the same name conflict. But it
would be nice to allow them to both be imported in the case where they
refer to the same binding.

Problem: Can't do the check eagerly, because the binding of L may
change between when define-literal-set is compiled and the comparison
involving L. For example:

  (module M racket
    (require syntax/parse)
    (define-literal-set LS (lambda))
    (require (only-in some-other-lang lambda))
    .... LS ....)

The expansion of the LS definition sees a different lambda than the
one that the literal in LS actually refers to.

Similarly, a literal in LS might not be defined when the expander
runs, but might get defined later. (Although I think that will already
cause an error, so don't worry about that case.)
|#

;; FIXME: keep one copy of each identifier (?)

(define-syntax (literal-set->predicate stx)
  (syntax-case stx ()
    [(literal-set->predicate litset-id)
     (let ([val (and (identifier? #'litset-id)
                     (syntax-local-value/record #'litset-id literalset?))])
       (unless val (raise-syntax-error #f "expected literal set name" stx #'litset-id))
       (let ([lits (literalset-literals val)])
         (with-syntax ([((lit phase-var) ...)
                        (for/list ([lit (in-list lits)]
                                   #:when (lse:lit? lit))
                          (list (lse:lit-external lit) (lse:lit-phase lit)))])
           #'(make-literal-set-predicate (list (list (quote-syntax lit) phase-var) ...)))))]))

(define (make-literal-set-predicate lits)
  (lambda (x [phase (syntax-local-phase-level)])
    (for/or ([lit (in-list lits)])
      (let ([lit-id (car lit)]
            [lit-phase (cadr lit)])
        (free-identifier=? x lit-id phase lit-phase)))))

;; Literal sets

(define-literal-set kernel-literals
  (begin
   begin0
   define-values
   define-syntaxes
   define-values-for-syntax ;; kept for compat.
   begin-for-syntax
   set!
   let-values
   letrec-values
   #%plain-lambda
   case-lambda
   if
   quote
   quote-syntax
   letrec-syntaxes+values
   with-continuation-mark
   #%expression
   #%plain-app
   #%top
   #%datum
   #%variable-reference
   module module* #%provide #%require #%declare
   #%plain-module-begin))
