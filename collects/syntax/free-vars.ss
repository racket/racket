#lang scheme/base
;; this finds the free variables of fully-expanded mzscheme expressions
;; adapted from code by mflatt


(require syntax/kerncase
         syntax/boundmap
         (for-template scheme/base))

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
(define (free-vars e)
  (kernel-syntax-case e #f
    [id 
     (identifier? #'id) 
     (if (eq? 'lexical (identifier-binding #'id))
         (list #'id)
         null)]
    [(#%top . id) null]
    [(#%expression e) (free-vars #'e)]
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
                            #'((lambda formals expr ...) ...))))]
    [(let-values ([(id ...) rhs] ...) expr ...)
     (merge (cons (free-vars #'(lambda (id ... ...) expr ...))
                  (map free-vars (syntax->list #'(rhs ...)))))]
    [(letrec-values ([(id ...) rhs] ...) expr ...)
     (free-vars #'(lambda (id ... ...) rhs ... expr ...))]      
    [(_ expr ...)
     ;; if, begin, begin0, set!, #%app, #%variable-reference, with-continuation-mark
     (merge (map free-vars (syntax->list #'(expr ...))))]))

