#lang eopl

(provide equal-types?)

(define equal-types?
  (lambda (ty1 ty2)
    (equal-up-to-gensyms? ty1 ty2)))

;; S-exp = Sym | Listof(S-exp)
;; A-list = Listof(Pair(TvarTypeSym, TvarTypesym))
;; a tvar-type-sym is a symbol ending with a digit.

;; equal-up-to-gensyms? : S-exp * S-exp -> Bool
;; Page: 271
(define equal-up-to-gensyms?
  (lambda (sexp1 sexp2)
    (equal?
     (apply-subst-to-sexp (canonical-subst sexp1) sexp1)
     (apply-subst-to-sexp (canonical-subst sexp2) sexp2))))

;; canonicalize : S-exp -> A-list
;; usage: replaces all tvar-syms with tvar1, tvar2, etc.
;; Page: 271
(define canonical-subst
  (lambda (sexp)
    ;; loop : sexp * alist -> alist
    (let loop ((sexp sexp) (table '()))
      (cond
        ((null? sexp) table)
        ((tvar-type-sym? sexp) 
         (cond
           ((assq sexp table) ; sexp is already bound, no more to
            ; do 
            table)
           (else 
            (cons 
             ;; the length of the table serves as a counter!
             (cons sexp (ctr->ty (length table))) 
             table))))
        ((pair? sexp)
         (loop (cdr sexp)
               (loop (car sexp) table)))
        (else table)))))

;; tvar-type-sym? : Sym -> Bool
;; Page: 272
(define tvar-type-sym?
  (lambda (sym)
    (and (symbol? sym)
         (char-numeric? (car (reverse (symbol->list sym)))))))

;; symbol->list : Sym -> List
;; Page: 272
(define symbol->list
  (lambda (x) (string->list (symbol->string x))))

;; apply-subst-to-sexp : A-list * S-exp -> S-exp
;; Page: 272
(define apply-subst-to-sexp
  (lambda (subst sexp)
    (cond
      ((null? sexp) sexp)
      ((tvar-type-sym? sexp)
       (cdr (assq sexp subst)))
      ((pair? sexp)
       (cons
        (apply-subst-to-sexp subst (car sexp))
        (apply-subst-to-sexp subst (cdr sexp))))
      (else sexp))))

;; ctr->ty : N -> Sym
;; Page: 272
(define ctr->ty
  (lambda (n)
    (string->symbol
     (string-append
      "tvar"
      (number->string n)))))

