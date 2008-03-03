#lang scheme/unit

(require "signatures.ss"
         "type-effect-convenience.ss"
         "lexical-env.ss"
         "type-annotation.ss"
         "type-alias-env.ss"
         "type-env.ss"
         "parse-type.ss"
         "utils.ss"
         syntax/free-vars
         mzlib/trace
         syntax/kerncase
         (for-template 
          scheme/base
          "internal-forms.ss"))

(require (only-in srfi/1 [member member1]))


(import tc-expr^)
(export tc-let^)

(define (do-check expr->type namess types form exprs body clauses expected)
  ;; just for error reporting
  #;(define clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)]))
  ;; extend the lexical environment for checking the body
  (with-lexical-env/extend 
   ;; the list of lists of name
   namess
   ;; the types
   types
   (for-each (lambda (stx e t) (check-type stx (expr->type e) t))
             clauses
             exprs 
             (map list->values-ty types))
   (if expected 
       (tc-exprs/check (syntax->list body) expected)
       (tc-exprs (syntax->list body)))))

#|
;; this is more abstract, but sucks
  (define ((mk f) namess exprs body form)
    (let* ([names (map syntax->list (syntax->list namess))]
           [exprs (syntax->list exprs)])
      (f (lambda (e->t namess types exprs) (do-check e->t namess types form exprs body)) names exprs)))
  
  (define tc/letrec-values 
    (mk (lambda (do names exprs)
          (let ([types (map (lambda (l) (map get-type l)) names)])
            (do tc-expr/t names types exprs)))))
  
  (define tc/let-values
    (mk (lambda (do names exprs)
          (let* (;; the types of the exprs
                 [inferred-types (map tc-expr/t exprs)]
                 ;; the annotated types of the name (possibly using the inferred types)
                 [types (map get-type/infer names inferred-types)])
            (do (lambda (x) x) names types inferred-types)))))
  |#

(define (tc/letrec-values/check namess exprs body form expected)
  (tc/letrec-values/internal namess exprs body form expected))

(define (tc/letrec-values namess exprs body form)
  (tc/letrec-values/internal namess exprs body form #f))

(define (tc/letrec-values/internal namess exprs body form expected)
  (let* ([names (map syntax->list (syntax->list namess))]
         [flat-names (apply append names)]
         [exprs (syntax->list exprs)]           
         ;; the clauses for error reporting
         [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])
    (for-each (lambda (names body)
                (kernel-syntax-case* body #f (values :-internal define-type-alias-internal)
                  [(begin (quote-syntax (define-type-alias-internal nm ty)) (#%plain-app values))
                   (register-resolved-type-alias #'nm (parse-type #'ty))]
                  [(begin (quote-syntax (:-internal nm ty)) (#%plain-app values))
                   (register-type/undefined #'nm (parse-type #'ty))]
                  [_ (void)]))
              names
              exprs)
    (let loop ([names names] [exprs exprs] [flat-names flat-names] [clauses clauses])
      (cond 
        ;; after everything, check the body expressions
        [(null? names) 
         (if expected (tc-exprs/check (syntax->list body) expected) (tc-exprs (syntax->list body)))]
        ;; if none of the names bound in the letrec are free vars of this rhs
        [(not (ormap (lambda (n) (member1 n flat-names bound-identifier=?)) (free-vars (car exprs))))
         ;; then check this expression separately
         (let ([t (tc-expr/t (car exprs))])               
           (with-lexical-env/extend
            (list (car names))
            (list (get-type/infer (car names) t))
            (loop (cdr names) (cdr exprs) (apply append (cdr names)) (cdr clauses))))]
        [else
         ;(for-each (lambda (vs) (for-each (lambda (v) (printf/log "Letrec Var: ~a~n" (syntax-e v))) vs)) names)
         (do-check tc-expr/t names (map (lambda (l) (map get-type l)) names) form exprs body clauses expected)]))))

(define (tc/let-values/internal namess exprs body form expected)
  (let* (;; a list of each name clause
         [names (map syntax->list (syntax->list namess))]
         ;; all the trailing expressions - the ones actually bound to the names
         [exprs (syntax->list exprs)]
         ;; the types of the exprs
         [inferred-types (map tc-expr/t exprs)]
         ;; the annotated types of the name (possibly using the inferred types)
         [types (map get-type/infer names inferred-types)]
         ;; the clauses for error reporting
         [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])
    (do-check (lambda (x) x) names types form inferred-types body clauses expected)))

(define (tc/let-values/check namess exprs body form expected)
  (tc/let-values/internal namess exprs body form expected))

(define (tc/let-values namess exprs body form)
  (tc/let-values/internal namess exprs body form #f))

;(trace tc/letrec-values/internal)


