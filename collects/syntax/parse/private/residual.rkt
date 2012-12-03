#lang racket/base
(require (for-syntax racket/base)
         racket/stxparam
         racket/lazy-require)

;; ============================================================
;; Compile-time

(require (for-syntax racket/private/sc
                     syntax/parse/private/residual-ct))
(provide (for-syntax (all-from-out syntax/parse/private/residual-ct)))

(begin-for-syntax
  ;; == from runtime.rkt

 (provide make-attribute-mapping
          attribute-mapping?
          attribute-mapping-var
          attribute-mapping-name
          attribute-mapping-depth
          attribute-mapping-syntax?)

 (define-struct attribute-mapping (var name depth syntax?)
   #:omit-define-syntaxes
   #:property prop:procedure
   (lambda (self stx)
     (if (attribute-mapping-syntax? self)
         #`(#%expression #,(attribute-mapping-var self))
         (let ([source-name
                (or (let loop ([p (syntax-property stx 'disappeared-use)])
                      (cond [(identifier? p) p]
                            [(pair? p) (or (loop (car p)) (loop (cdr p)))]
                            [else #f]))
                    (attribute-mapping-name self))])
           #`(let ([value #,(attribute-mapping-var self)])
               (check-attr-value-is-syntax '#,(attribute-mapping-depth self)
                                           value
                                            (quote-syntax #,source-name))
               value)))))
 )

;; ============================================================
;; Run-time

(require "runtime-progress.rkt"
         syntax/stx)

(provide (all-from-out "runtime-progress.rkt")

         this-syntax
         this-role
         this-context-syntax
         attribute
         attribute-binding
         stx-list-take
         stx-list-drop/cx
         check-list^depth*
         check-literal*
         begin-for-syntax/once

         name->too-few/once
         name->too-few
         name->too-many
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
                            #'name))))]))

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
                                       (attribute-mapping-syntax? value)))
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

;; check-attr-value-is-syntax : nat any id -> boolean
;; returns #t if value is a (listof^depth syntax)
;; used by attribute-mapping code above
(define (check-attr-value-is-syntax depth value source-id)
  (define (check-syntax depth value)
    (if (zero? depth)
        (syntax? value)
        (and (list? value)
             (for/and ([part (in-list value)])
                      (check-syntax (sub1 depth) part)))))
  (unless (check-syntax depth value)
    (raise-syntax-error #f
                        (format "attribute is bound to non-syntax value: ~e" value)
                        source-id)))

;; check-list^depth* : symbol nat any -> list^depth
(define (check-list^depth* aname n0 v0)
  (define (loop n v)
    (when (positive? n)
      (unless (list? v)
        (raise-type-error aname (format "lists nested ~s deep" n0) v))
      (for ([x (in-list v)]) (loop (sub1 n) x))))
  (loop n0 v0)
  v0)

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

(lazy-require
 ["runtime-report.rkt"
  (syntax-patterns-fail)])

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
