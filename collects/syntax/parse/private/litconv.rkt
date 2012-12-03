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

(define-for-syntax (check-duplicate-literals stx imports lits)
  (let ([lit-t (make-hasheq)]) ;; sym => #t
    (define (check+enter! key blame-stx)
      (when (hash-ref lit-t key #f)
        (raise-syntax-error #f (format "duplicate literal: ~a" key) stx blame-stx))
      (hash-set! lit-t key #t))
    (for ([id+litset (in-list imports)])
      (let ([litset-id (car id+litset)]
            [litset (cdr id+litset)])
        (for ([entry (in-list (literalset-literals litset))])
          (check+enter! (car entry) litset-id))))
    (for ([lit (in-list lits)])
      (check+enter! (syntax-e (car lit)) (car lit)))))

(define-syntax (define-literal-set stx)
  (syntax-case stx ()
    [(define-literal-set name . rest)
     (let-values ([(chunks rest)
                   (parse-keyword-options
                    #'rest
                    `((#:literal-sets ,check-litset-list)
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
             [lits (syntax-case rest ()
                     [( (lit ...) )
                      (for/list ([lit (in-list (syntax->list #'(lit ...)))])
                        (check-literal-entry/litset lit stx))]
                     [_ (raise-syntax-error #f "bad syntax" stx)])]
             [imports (options-select-value chunks '#:literal-sets #:default null)])
         (check-duplicate-literals stx imports lits)
         (with-syntax ([((internal external) ...) lits]
                       [(litset-id ...) (map car imports)]
                       [relphase relphase])
           #`(begin
               (define phase-of-literals
                 (if 'relphase
                     (+ (phase-of-enclosing-module) 'relphase)
                     'relphase))
               (define-syntax name
                 (make-literalset
                  (append (literalset-literals (syntax-local-value (quote-syntax litset-id)))
                          ...
                          (list (list 'internal
                                      (quote-syntax external)
                                      (quote-syntax phase-of-literals))
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
Literal sets: The goal is for literals to refer to their bindings at

  phase 0 relative to the enclosing module

Use cases, explained:
1) module X with def-lit-set is required-for-syntax
     phase-of-mod-inst = 1
     phase-of-def = 0
     literals looked up at abs phase 1
       which is phase 0 rel to module X
2) module X with local def-lit-set within define-syntax
     phase-of-mod-inst = 1 (mod at 0, but +1 within define-syntax)
     phase-of-def = 1
     literals looked up at abs phase 0
       which is phase 0 rel to module X
3) module X with def-lit-set in phase-2 position (really uncommon case!)
     phase-of-mod-inst = 1 (not 2, apparently)
     phase-of-def = 2
     literals looked up at abs phase 0
       (that's why the weird (if (z?) 0 1) term)
|#

;; FIXME: keep one copy of each identifier (?)

(define-syntax (literal-set->predicate stx)
  (syntax-case stx ()
    [(literal-set->predicate litset-id)
     (let ([val (and (identifier? #'litset-id)
                     (syntax-local-value/record #'litset-id literalset?))])
       (unless val (raise-syntax-error #f "expected literal set name" stx #'litset-id))
       (let ([lits (literalset-literals val)])
         (with-syntax ([((_sym lit phase-var) ...) lits])
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
   module #%provide #%require
   #%plain-module-begin))
