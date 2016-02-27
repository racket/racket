#lang racket/base
;; this finds the free variables of fully-expanded racket expressions
;; adapted from code by mflatt

(require syntax/kerncase
         syntax/boundmap
         (for-template racket/base))

(provide free-vars)

;; An id-tree is either
;;  - null
;;  - id
;;  - (cons id-tree id-tree)

;; merge : id-tree -> (listof id)
;;  merges lists of identifiers, removing module-identifier=?
;;  duplicates
(define (merge t)
  (define m (make-module-identifier-mapping))
  (reverse
   (let loop ([t t] [a null])
     (cond
      [(null? t) a]
      [(identifier? t)
       (if (module-identifier-mapping-get m t (lambda () #f))
           a
           (begin
             (module-identifier-mapping-put! m t #t)
             (cons t a)))]
      [(pair? t) (loop (cdr t) (loop (car t) a))]
      [else (error "internal error")]))))

;; formals->ids : formals-stx -> (listof identifier?)
;; Parses a procedure "formals" and returns the binding ids
;;  in a table
(define (formals->ids f)
  (let loop ([f f])
    (cond
     [(identifier? f) (list f)]
     [(pair? f) (cons (car f)
                      (loop (cdr f)))]
     [(null? f) null]
     [(syntax? f) (loop (syntax-e f))])))

;; free-vars : expr-stx [inspector?] [#:module-bound? any/c] -> (listof id)
;; Returns a list of free lambda- and let-bound identifiers in a
;;  given epression. The expression must be fully expanded.
;; If `module-bound?` is true, also return module-bound variables.
(define (free-vars e
                   [code-insp
                    (variable-reference->module-declaration-inspector
                     (#%variable-reference))]
                   #:module-bound? [module-bound? #f])
  (define (submodule-error e)
    (error 'free-vars "submodules not supported: ~a" e))
  ;; It would be nicer to have a functional mapping:
  (define bindings (make-bound-identifier-mapping))
  (merge
   (let free-vars ([e e])
     (kernel-syntax-case (syntax-disarm e code-insp) #f
       [id
        (identifier? #'id)
        (let ([b (identifier-binding #'id)])
          (cond [(and (eq? 'lexical b)
                      (not (bound-identifier-mapping-get bindings #'id (lambda () #f))))
                 (list #'id)]
                [(and module-bound? ; do we count module-bound vars too?
                      ;; we're in an expression context, so any module-bound
                      ;; variable is free
                      (list? b))
                 (list #'id)]
                [else
                 null]))]
       [(#%top . id) null]
       [(quote q) null]
       [(quote-syntax . _) null]
       [(#%plain-lambda formals expr ...)
        (let ([ids (formals->ids #'formals)])
          (for ([id (in-list ids)])
            (bound-identifier-mapping-put! bindings id #t))
          (begin0
           (map free-vars (syntax->list #'(expr ...)))
           ;; Since every binding should be distinct, it shouldn't
           ;; matter whether we map them back to #f, but just in case
           ;; we get a weird expression...
           (for ([id (in-list ids)])
             (bound-identifier-mapping-put! bindings id #f))))]
       [(case-lambda [formals expr ...] ...)
        (map free-vars (syntax->list
                        #'((#%plain-lambda formals expr ...) ...)))]
       [(let-values ([(id ...) rhs] ...) expr ...)
        (cons (free-vars #'(#%plain-lambda (id ... ...) expr ...))
              (map free-vars (syntax->list #'(rhs ...))))]
       [(letrec-values ([(id ...) rhs] ...) expr ...)
        (free-vars #'(#%plain-lambda (id ... ...) rhs ... expr ...))]
       [(letrec-syntaxes+values stx-bindings ([(id ...) rhs] ...) expr ...)
        (free-vars #'(#%plain-lambda (id ... ...) rhs ... expr ...))]
       [(kw expr ...)
        (ormap (lambda (k) (free-identifier=? k #'kw))
               (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%expression
                     #'#%variable-reference #'with-continuation-mark))
        (map free-vars (syntax->list #'(expr ...)))]
       [(module . _)
        (submodule-error e)]
       [(module* . _)
        (submodule-error e)]
       [(kw . _)
        (error 'free-vars "unknown core form: ~a" (syntax->datum #'kw))]))))
