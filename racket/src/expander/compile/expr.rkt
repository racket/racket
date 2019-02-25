#lang racket/base
(require "../common/set.rkt"
         "../common/performance.rkt"
         "../syntax/syntax.rkt"
         "../syntax/to-list.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/property.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../syntax/binding.rkt"
         "../syntax/match.rkt"
         "../common/module-path.rkt"
         "../expand/parsed.rkt"
         "built-in-symbol.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "self-quoting.rkt"
         "../host/correlate.rkt"
         "correlate.rkt")

(provide compile
         compile-quote-syntax)

;; Convert an expanded syntax object to an expression that is
;; represented by a plain S-expression plus source location info (so,
;; still represented as a syntax object). The expression is compiled
;; for a particular phase, but if the expression is in a module, its
;; phase can be shifted at run time by the amount bound to
;; `phase-shift-id`. Module bindings are accessed through a namespace
;; that is bound to `ns-id` at run time.
;; The `result-used?` hint lets us drop `quote-syntax` forms that will
;; not be used in the result, so we can avoid serializing them; a value
;; of `#f` for `result-used?` means that the expression can be replaced
;; by a boolean-equivalent value if it has no side effect.
(define (compile p cctx [name #f] [result-used? #t])
  (let ([compile (lambda (p name result-used?) (compile p cctx name result-used?))])
    (define s (parsed-s p))
    (cond
     [(parsed-id? p)
      (compile-identifier p cctx)]
     [(parsed-lambda? p)
      (cond
       [result-used?
        (add-lambda-properties
         (correlate* s `(lambda ,@(compile-lambda (parsed-lambda-keys p) (parsed-lambda-body p) cctx)))
         name
         s)]
       [else (correlate~ s `(quote unused-lambda))])]
     [(parsed-case-lambda? p)
      (cond
       [result-used?
        (add-lambda-properties
         (correlate* s `(case-lambda ,@(for/list ([clause (in-list (parsed-case-lambda-clauses p))])
                                    (compile-lambda (car clause) (cadr clause) cctx))))
         name
         s)]
       [else (correlate~ s `(quote unused-case-lambda))])]
     [(parsed-app? p)
      (define rands (parsed-app-rands p))
      (correlate/app s (cons
                        (compile (parsed-app-rator p) #f #t)
                        (for/list ([r (in-list rands)])
                          (compile r #f #t))))]
     [(parsed-if? p)
      (define tst-e (compile (parsed-if-tst p) #f #f))
      ;; Ad hoc optimization of `(if #t ... ...)` or `(if #f ... ...)`
      ;; happens to help avoid syntax literals in pattern matching.
      (cond
       [(eq? (correlated-e tst-e) #t) (compile (parsed-if-thn p) name result-used?)]
       [(eq? (correlated-e tst-e) #f) (compile (parsed-if-els p) name result-used?)]
       [else
        (correlate~ s `(if
                        ,tst-e
                        ,(compile (parsed-if-thn p) name result-used?)
                        ,(compile (parsed-if-els p) name result-used?)))])]
     [(parsed-with-continuation-mark? p)
      (correlate~ s `(with-continuation-mark
                      ,(compile (parsed-with-continuation-mark-key p) #f #t)
                      ,(compile (parsed-with-continuation-mark-val p) #f #t)
                      ,(compile (parsed-with-continuation-mark-body p) name result-used?)))]
     [(parsed-begin0? p)
      (correlate~ s `(begin0
                      ,(compile (car (parsed-begin0-body p)) name result-used?)
                      ,@(for/list ([e (in-list (cdr (parsed-begin0-body p)))])
                          (compile e #f #f))))]
     [(parsed-begin? p)
      (correlate~ s (compile-begin (parsed-begin-body p) cctx name result-used?))]
     [(parsed-set!? p)
      (correlate~ s `(,@(compile-identifier (parsed-set!-id p) cctx
                                            #:set-to? #t
                                            #:set-to (compile (parsed-set!-rhs p) (parsed-s (parsed-set!-id p)) #t))))]
     [(parsed-let-values? p)
      (compile-let p cctx name #:rec? #f result-used?)]
     [(parsed-letrec-values? p)
      (compile-let p cctx name #:rec? #t result-used?)]
     [(parsed-quote? p)
      (define datum (parsed-quote-datum p))
      (cond
       [(self-quoting-in-linklet? datum)
        (correlate~ s datum)]
       [else
        (correlate~ s `(quote ,datum))])]
     [(parsed-quote-syntax? p)
      (if result-used?
          (compile-quote-syntax (parsed-quote-syntax-datum p) cctx)
          (correlate~ s `(quote ,(syntax->datum s))))]
     [(parsed-#%variable-reference? p)
      (define id (parsed-#%variable-reference-id p))
      (correlate~ s 
                  (if id
                      `(#%variable-reference ,(compile-identifier id cctx))
                      `(#%variable-reference)))]
     [else
      (error "unrecognized parsed form:" p)])))

(define (compile-lambda formals bodys cctx)
  `(,formals ,(compile-sequence bodys cctx #f #t)))

(define (compile-sequence bodys cctx name result-used?)
  (if (null? (cdr bodys))
      (compile (car bodys) cctx name result-used?)
      (compile-begin bodys cctx name result-used?)))

(define (compile-begin es cctx name result-used?)
  (define used-pos (sub1 (length es)))
  `(begin ,@(for/list ([e (in-list es)]
                       [i (in-naturals)])
              (define used? (= i used-pos))
              (compile e cctx (and used? name) (and used? result-used?)))))

(define (add-lambda-properties s inferred-name orig-s)
  ;; Allow pairs formed by origin tracking to provide the
  ;; same name multiple times:
  (define (simplify-name v)
    (cond
     [(pair? v)
      (define n1 (simplify-name (car v)))
      (define n2 (simplify-name (cdr v)))
      (if (eq? n1 n2) n1 v)]
     [else v]))
  ;; Get either a declared 'inferred-name or one accumulated by the compiler
  (define name (or (let ([v (simplify-name (syntax-property orig-s 'inferred-name))])
                     (and (or (symbol? v) (syntax? v) (void? v))
                          v))
                   inferred-name))
  (define named-s (if name
                      (correlated-property (->correlated s)
                                           'inferred-name
                                           (if (syntax? name) (syntax-e name) name))
                      s))
  (define as-method (syntax-property orig-s 'method-arity-error))
  (if as-method
      (correlated-property (->correlated named-s) 'method-arity-error as-method)
      named-s))

(define (compile-let p cctx name #:rec? rec? result-used?)
  (define body (parsed-let_-values-body p))
  (correlate~ (parsed-s p)
              `(,(if rec? 'letrec-values 'let-values)
                ,(for/list ([clause (in-list (parsed-let_-values-clauses p))]
                            [ids (in-list (parsed-let_-values-idss p))])
                   `[,(if rec?
                          (for/list ([sym (in-list (car clause))]
                                     [id (in-list ids)])
                            (add-undefined-error-name-property sym id))
                          (car clause))
                     ,(compile (cadr clause)
                               cctx
                               (and (= 1 (length ids)) (car ids)))])
                ,(compile-sequence body cctx name result-used?))))

(define (add-undefined-error-name-property sym orig-id)
  (define id (correlate~ orig-id sym))
  (correlated-property (->correlated id) 'undefined-error-name
                       (or (syntax-property orig-id 'undefined-error-name)
                           (syntax-e orig-id))))

(define (compile-identifier p cctx #:set-to? [set-to? #f] #:set-to [rhs #f])
  (define normal-b (parsed-id-binding p))
  ;; If `normal-b`, then `(parsed-s p)` might be #f
  (define b
    (or normal-b
        ;; Assume a variable reference
        (make-module-binding (compile-context-self cctx)
                             (compile-context-phase cctx)
                             (syntax-e (parsed-s p)))))
  (define sym
    (cond
     [(local-binding? b)
      (local-binding-key b)]
     [(module-binding? b)
      (define mpi (if (parsed-top-id? p)
                      (compile-context-self cctx)
                      (module-binding-module b)))
      (cond
       [(parsed-primitive-id? p)
        ;; Direct reference to a runtime primitive:
        (unless (zero? (module-binding-phase b))
          (error "internal error: non-zero phase for a primitive"))
        (when set-to?
          (error "internal error: cannot assign to a primitive:" (module-binding-sym b)))
        ;; Expect each primitive to be bound:
        (module-binding-sym b)]
       [(and (eq? mpi (compile-context-module-self cctx))
             ;; Direct reference to a variable defined in the same module:
             (hash-ref (header-binding-sym-to-define-sym (compile-context-header cctx))
                       (module-binding-sym b)
                       ;; If this `#f` is used as the result, then the identifier must be a
                       ;; reference to a binding that was introduced through `local-expand`,
                       ;; but didn't survive to a definition in the full expansion; treat it
                       ;; as an undefined export.
                       #f))
        => (lambda (sym) sym)]
       [else
        ;; Reference to a variable defined in another module or in an
        ;; environment (such as the top level) other than a module
        ;; context; register as a linklet import or export
        (register-required-variable-use! (compile-context-header cctx)
                                         (if (inside-module-context? mpi (compile-context-self cctx))
                                             (compile-context-self cctx)
                                             mpi)
                                         (module-binding-phase b)
                                         (module-binding-sym b)
                                         (or (module-binding-extra-inspector b)
                                             (parsed-id-inspector p)
                                             (and (parsed-s p)
                                                  (syntax-inspector (parsed-s p)))))])]
     [else
      (error "not a reference to a module or local binding:" b (parsed-s p))]))
  (correlate~ (parsed-s p) (if set-to?
                               `(set! ,sym ,rhs)
                               sym)))

(define (compile-quote-syntax q cctx)
  (define pos (add-syntax-literal! (compile-context-header cctx) q))
  (cond
   [(compile-context-lazy-syntax-literals? cctx)
    (generate-lazy-syntax-literal-lookup pos)]
   [else
    (generate-eager-syntax-literal-lookup pos)]))
