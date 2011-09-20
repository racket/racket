#lang racket/base
;; this finds the free variables of fully-expanded mzscheme expressions
;; adapted from code by mflatt

(require syntax/kerncase
         syntax/boundmap
         (for-template racket/base))

(provide free-vars)

;; merge : (liftof (listof id)) -> (listof id)
;;  merges lists of identifiers, removing module-identifier=?
;;  duplicates
(define (merge l)
  (cond
    [(null? l) null]
    [(null? (cdr l)) (car l)]
    [else (let ([m (make-module-identifier-mapping)])
            (for-each (lambda (ids)
                        (for-each (lambda (id)
                                    (module-identifier-mapping-put! m id #t))
                                  ids))
                      l)
            (module-identifier-mapping-map m (lambda (k v) k)))]))

;; formals->boundmap : formals-stx -> bound-map-table
;; Parses a procedure "formals" and returns the binding ids
;;  in a table
(define (formals->boundmap f)
  (let ([ids (let loop ([f f])
               (cond
                 [(identifier? f) (list f)]
                 [(pair? f) (cons (car f)
                                  (loop (cdr f)))]
                 [(null? f) null]
                 [(syntax? f) (loop (syntax-e f))]))]
        [b (make-bound-identifier-mapping)])
    (for-each (lambda (id)
                (bound-identifier-mapping-put! b id #t))
              ids)
    b))

;; free-vars : expr-stx -> (listof id)
;; Returns a list of free lambda- and let-bound identifiers in a
;;  given epression. The expression must be fully expanded.
(define (free-vars e [code-insp
                      (variable-reference->module-declaration-inspector
                       (#%variable-reference))])
  (let free-vars ([e e])
    (kernel-syntax-case (syntax-disarm e code-insp) #f
      [id 
       (identifier? #'id) 
       (if (eq? 'lexical (identifier-binding #'id))
           (list #'id)
           null)]
      [(#%top . id) null]
      [(quote q) null]
      [(quote-syntax q) null]
      [(#%plain-lambda formals expr ...)
       (let ([free (merge (map free-vars (syntax->list #'(expr ...))))]
             [bindings (formals->boundmap #'formals)])
         (filter (lambda (id)
                   (not (bound-identifier-mapping-get bindings id (lambda () #f))))
                 free))]
      [(case-lambda [formals expr ...] ...)
       (merge (map free-vars (syntax->list
                              #'((#%plain-lambda formals expr ...) ...))))]
      [(let-values ([(id ...) rhs] ...) expr ...)
       (merge (cons (free-vars #'(#%plain-lambda (id ... ...) expr ...))
                    (map free-vars (syntax->list #'(rhs ...)))))]
      [(letrec-values ([(id ...) rhs] ...) expr ...)
       (free-vars #'(#%plain-lambda (id ... ...) rhs ... expr ...))]
      [(letrec-syntaxes+values stx-bindings ([(id ...) rhs] ...) expr ...)
       (free-vars #'(#%plain-lambda (id ... ...) rhs ... expr ...))]
      [(kw expr ...)
       (ormap (lambda (k) (free-identifier=? k #'kw))
              (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%expression
                    #'#%variable-reference #'with-continuation-mark))
       (merge (map free-vars (syntax->list #'(expr ...))))]
      [(kw . _)
       (error 'free-vars "unknown core form: ~a" (syntax->datum #'kw))])))
