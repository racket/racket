#lang racket/base
(require (for-syntax racket/base)
         racket/stxparam
         racket/lazy-require
         racket/private/promise)

;; ============================================================
;; Compile-time

(require (for-syntax racket/private/sc "residual-ct.rkt"))
(provide (for-syntax (all-from-out "residual-ct.rkt")))

(require racket/private/template)
(provide (for-syntax attribute-mapping attribute-mapping?))

;; ============================================================
;; Run-time

(require "runtime-progress.rkt"
         "3d-stx.rkt"
         syntax/stx)

(provide (all-from-out "runtime-progress.rkt")

         this-syntax
         this-role
         this-context-syntax
         attribute
         attribute-binding
         check-attr-value
         stx-list-take
         stx-list-drop/cx
         datum->syntax/with-clause
         check-literal*
         error/null-eh-match
         begin-for-syntax/once

         name->too-few/once
         name->too-few
         name->too-many
         normalize-context
         syntax-patterns-fail)

;; == from runtime.rkt

;; this-syntax
;; Bound to syntax being matched inside of syntax class
(define-syntax-parameter this-syntax
  (lambda (stx)
    (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

(define-syntax-parameter this-role
  (lambda (stx)
    (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

;; this-context-syntax
;; Bound to (expression that extracts) context syntax (bottom frame in progress)
(define-syntax-parameter this-context-syntax
  (lambda (stx)
    (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

(define-syntax (attribute stx)
  (syntax-case stx ()
    [(attribute name)
     (identifier? #'name)
     (let ([mapping (syntax-local-value #'name (lambda () #f))])
       (unless (syntax-pattern-variable? mapping)
         (raise-syntax-error #f "not bound as a pattern variable" stx #'name))
       (let ([var (syntax-mapping-valvar mapping)])
         (let ([attr (syntax-local-value var (lambda () #f))])
           (unless (attribute-mapping? attr)
             (raise-syntax-error #f "not bound as an attribute" stx #'name))
           (syntax-property (attribute-mapping-var attr)
                            'disappeared-use
                            (list (syntax-local-introduce #'name))))))]))

;; (attribute-binding id)
;; mostly for debugging/testing
(define-syntax (attribute-binding stx)
  (syntax-case stx ()
    [(attribute-bound? name)
     (identifier? #'name)
     (let ([value (syntax-local-value #'name (lambda () #f))])
       (if (syntax-pattern-variable? value)
           (let ([value (syntax-local-value (syntax-mapping-valvar value) (lambda () #f))])
             (if (attribute-mapping? value)
                 #`(quote #,(make-attr (attribute-mapping-name value)
                                       (attribute-mapping-depth value)
                                       (if (attribute-mapping-check value) #f #t)))
                 #'(quote #f)))
           #'(quote #f)))]))

;; stx-list-take : stxish nat -> syntax
(define (stx-list-take stx n)
  (datum->syntax #f
                 (let loop ([stx stx] [n n])
                   (if (zero? n)
                       null
                       (cons (stx-car stx)
                             (loop (stx-cdr stx) (sub1 n)))))))

;; stx-list-drop/cx : stxish stx nat -> (values stxish stx)
(define (stx-list-drop/cx x cx n)
  (let loop ([x x] [cx cx] [n n])
    (if (zero? n)
        (values x
                (if (syntax? x) x cx))
        (loop (stx-cdr x)
              (if (syntax? x) x cx)
              (sub1 n)))))

;; check-attr-value : Any d:Nat b:Boolean Syntax/#f -> (Listof^d (if b Syntax Any))
(define (check-attr-value v0 depth0 base? ctx)
  (define (bad kind v)
    (raise-syntax-error #f (format "attribute contains non-~s value\n  value: ~e" kind v) ctx))
  (define (depthloop depth v)
    (if (zero? depth)
        (if base? (baseloop v) v)
        (let listloop ([v v] [root? #t])
          (cond [(null? v) null]
                [(pair? v) (let ([new-car (depthloop (sub1 depth) (car v))]
                                 [new-cdr (listloop (cdr v) #f)])
                             (cond [(and (eq? (car v) new-car) (eq? (cdr v) new-cdr)) v]
                                   [else (cons new-car new-cdr)]))]
                [(promise? v) (listloop (force v) root?)]
                [(and root? (eq? v #f)) (begin (signal-absent-pvar) (bad 'list v))]
                [else (bad 'list v)]))))
  (define (baseloop v)
    (cond [(syntax? v) v]
          [(promise? v) (baseloop (force v))]
          [(eq? v #f) (begin (signal-absent-pvar) (bad 'syntax v))]
          [else (bad 'syntax v)]))
  (depthloop depth0 v0))

;; datum->syntax/with-clause : any -> syntax
(define (datum->syntax/with-clause x)
  (cond [(syntax? x) x]
        [(2d-stx? x #:traverse-syntax? #f)
         (datum->syntax #f x #f)]
        [else
         (error 'datum->syntax/with-clause
                (string-append
                 "implicit conversion to 3D syntax\n"
                 " right-hand side of #:with clause or ~~parse pattern would be 3D syntax\n"
                 "  value: ~e")
                x)]))

;; check-literal* : id phase phase (listof phase) stx -> void
(define (check-literal* id used-phase mod-phase ok-phases/ct-rel ctx)
  (unless (or (memv (and used-phase (- used-phase mod-phase))
                    ok-phases/ct-rel)
              (identifier-binding id used-phase))
    (raise-syntax-error
     #f
     (format "literal is unbound in phase ~a (phase ~a relative to the enclosing module)"
             used-phase
             (and used-phase (- used-phase mod-phase)))
     ctx id)))

;; error/null-eh-match : -> (escapes)
(define (error/null-eh-match)
  (error 'syntax-parse "an ellipsis-head pattern matched an empty sequence"))

;; (begin-for-syntax/once expr/phase1 ...)
;; evaluates in pass 2 of module/intdefs expansion
(define-syntax (begin-for-syntax/once stx)
  (syntax-case stx ()
    [(bfs/o e ...)
     (cond [(list? (syntax-local-context))
            #`(define-values ()
                (begin (begin-for-syntax/once e ...)
                       (values)))]
           [else
            #'(let-syntax ([m (lambda _ (begin e ...) #'(void))])
                (m))])]))

;; == parse.rkt

(define (name->too-few/once name)
  (and name (format "missing required occurrence of ~a" name)))

(define (name->too-few name)
  (and name (format "too few occurrences of ~a" name)))

(define (name->too-many name)
  (and name (format "too many occurrences of ~a" name)))

;; == parse.rkt

;; normalize-context : Symbol Any Syntax -> (list Symbol/#f Syntax)
(define (normalize-context who ctx stx)
  (cond [(syntax? ctx)
         (list #f ctx)]
        [(symbol? ctx)
         (list ctx stx)]
        [(eq? ctx #f)
         (list #f stx)]
        [(and (list? ctx)
              (= (length ctx) 2)
              (or (symbol? (car ctx)) (eq? #f (car ctx)))
              (syntax? (cadr ctx)))
         ctx]
        [else (error who "bad #:context argument\n  expected: ~s\n  given: ~e"
                     '(or/c syntax? symbol? #f (list/c (or/c symbol? #f) syntax?))
                     ctx)]))

;; == parse.rkt

(lazy-require
 ["runtime-report.rkt"
  (call-current-failure-handler)])

;; syntax-patterns-fail : (list Symbol/#f Syntax) -> (Listof (-> Any)) FailureSet -> escapes
(define ((syntax-patterns-fail ctx) undos fs)
  (unwind-to undos null)
  (call-current-failure-handler ctx fs))

;; == specialized ellipsis parser
;; returns (values 'ok attr-values) or (values 'fail failure)

(provide predicate-ellipsis-parser)

(define (predicate-ellipsis-parser x cx pr es pred? desc rl)
  (let ([elems (stx->list x)])
    (if (and elems (list? elems) (andmap pred? elems))
        (values 'ok elems)
        (let loop ([x x] [cx cx] [i 0])
          (cond [(syntax? x)
                 (loop (syntax-e x) x i)]
                [(pair? x)
                 (if (pred? (car x))
                     (loop (cdr x) cx (add1 i))
                     (let* ([pr (ps-add-cdr pr i)]
                            [pr (ps-add-car pr)]
                            [es (es-add-thing pr desc #t rl es)])
                       (values 'fail (failure pr es))))]
                [else ;; not null, because stx->list failed
                 (let ([pr (ps-add-cdr pr i)]
                       #|
                       ;; Don't extend es! That way we don't get spurious "expected ()"
                       ;; that *should* have been cancelled out by ineffable pair failures.
                       |#)
                   (values 'fail (failure pr es)))])))))

(provide illegal-cut-error)

(define (illegal-cut-error . _)
  (error 'syntax-parse "illegal use of cut"))

;; ----

(provide unwind-to
         maybe-add-state-undo
         current-state
         current-state-writable?
         state-cons!
         track-literals)

(define (unwind-to undos base)
  ;; PRE: undos = (list* proc/hash ... base)
  (unless (eq? undos base)
    (let ([top-undo (car undos)])
      (cond [(procedure? top-undo) (top-undo)]
            [(hash? top-undo) (current-state top-undo)]))
    (unwind-to (cdr undos) base)))

(define (maybe-add-state-undo init-state new-state undos)
  (if (eq? init-state new-state)
      undos
      (cons init-state undos)))

;; To make adding undos to rewind current-state simpler, only allow updates
;; in a few contexts:
;; - literals (handled automatically)
;; - in ~do/#:do blocks (sets current-state-writable? = #t)

(define current-state (make-parameter (hasheq)))
(define current-state-writable? (make-parameter #f))

(define (state-cons! key value)
  (define state (current-state))
  (current-state (hash-set state key (cons value (hash-ref state key null)))))

(define (track-literals who v #:introduce? [introduce? #t])
  (unless (syntax? v)
    (raise-argument-error who "syntax?" v))
  (let* ([literals (hash-ref (current-state) 'literals '())])
    (if (null? literals)
        v
        (let ([literals* (if (and introduce? (syntax-transforming?) (list? literals))
                             (for/list ([literal (in-list literals)])
                               (if (identifier? literal)
                                   (syntax-local-introduce literal)
                                   literal))
                             literals)]
              [old-val (syntax-property v 'disappeared-use)])
          (syntax-property v 'disappeared-use
                           (if old-val
                               (cons literals* old-val)
                               literals*))))))
