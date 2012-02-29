#lang racket

(require redex/reduction-semantics)

;; Ariola and Felleisen's formulation:
;;
;; M ::= .... | letrec D in M
;; D ::= x_1 be M_1, ..., x_n be M_n
;; E ::= ....
;;     | letrec D in E
;;     | letrec D, x be E in E[x]
;;     | letrec x_n be E, D[x, x_n] in E[x]
;; D[x, x_n] ::= x be E[x_1], ..., x_n-1 be E[x_n], D
;;
;; Redex's pattern language does not support this formuation directly.
;; There are two obstacles, both related to letrec's binding clauses:
;; 1. The clauses are considered to be unordered. For example, the term
;;                  letrec x be λx.x, y be x in y
;; is equivalent to the term:
;;                  letrec y be y, x be λx.x in y
;; 2. The ellipsis notation in the definition of D[x, x_n] expresses a
;; relationship between adjacent bindings that cannot be directly 
;; expressed using Redex's ellipsis, namely that
;;                  x_i be E[x_i+1]
;; precedes the binding for x_i+1.
;;
;; The definition below works around these obstacles using a 
;; metafunction that checks the dependency expressed in D[x, x_n].

(define-language cbn-letrec
  ((M N) x (λ x M) (M M) (letrec D M))
  (D ([x M] ...))
  (V (λ x M))
  (A V (letrec D A))
  (E hole (E M) (letrec D E)
     (side-condition
      (letrec ([x_0 M_0] ... [x_i E_i] [x_i+1 M_i+1] ...) 
        (in-hole E_j x_j))
      (and (term (reachable? x_j x_i ([x_0 M_0] ... [x_i+1 M_i+1] ...)))
           (term (does-not-bind? E_j x_j)))))
  (x variable-not-otherwise-mentioned))

(define-metafunction cbn-letrec
  reachable? : x x D -> #t or #f
  [(reachable? x_j x_j D) #t]
  [(reachable? x_j x_i ([x_0 M_0] ... [x_j (in-hole E_j x_k)] [x_j+1 M_j+1] ...))
   ,(and (term (does-not-bind? E_j x_k))
         (term (reachable? x_k x_i ([x_0 M_0] ... [x_j+1 M_j+1] ...))))])

;; unnessary if we assume binding occurrences are chosen to avoid shadowing
(define-metafunction cbn-letrec
  does-not-bind? : E x -> #t or #f
  [(does-not-bind? hole x) #t]
  [(does-not-bind? (E M) x)
   (does-not-bind? E x)]
  [(does-not-bind? (letrec ([x_0 M_0] ...) E) x)
   ,(and (not (member (term x) (term (x_0 ...))))
         (term (does-not-bind? E x)))]
  [(does-not-bind? (letrec ([x_0 M_0] ... [x_i E_i] [x_i+1 M_i+1] ...) M) x)
   ,(and (not (member (term x) (term (x_0 ... x_i x_i+1 ...))))
         (term (does-not-bind? E_i x)))])

(define-syntax-rule (test-match t p)
  (test-match/count t p 1))
(define-syntax-rule (test-no-match t p)
  (test-match/count t p 0))
(define-syntax (test-match/count stx)
  (syntax-case stx ()
    [(_ p t n)
     #`(let ([matches (redex-match cbn-letrec p (term t))])
         #,(syntax/loc stx
             (test-equal (if matches (length matches) 0) 
                         n)))]))

(test-match E (hole (λ x x)))
(test-no-match E ((λ x x) hole))

(test-match E (letrec ([x x]) hole))

(test-no-match E (letrec ([x hole]) (λ x x)))
(test-match E (letrec ([x x] [y hole] [z z]) y))
(test-match E (letrec ([x hole]) (x (λ x x))))
(test-match E (letrec ([x hole] [y (x (λ x x))]) y))
(test-match E (letrec ([x y] [y z] [z hole]) x))
(test-match E (letrec ([z hole] [y z] [x y]) x))
(test-no-match E (letrec ([x hole]) (letrec ([x (λ x x)]) x)))
(test-no-match E (letrec ([x y] [y (letrec ([z (λ x x)]) z)] [z hole]) x))

(test-equal (term (does-not-bind? (hole (λ x x)) x)) #t)
(test-equal (term (does-not-bind? (letrec ([x x] [y y] [z z]) hole) x)) #f)
(test-equal (term (does-not-bind? (letrec ([x x] [y y] [z z]) hole) y)) #f)
(test-equal (term (does-not-bind? (letrec ([x x] [y y] [z z]) hole) z)) #f)
(test-equal (term (does-not-bind? (letrec ([x x] [y y] [z z]) hole) a)) #t)
(test-equal (term (does-not-bind? (letrec ([x x] [y hole] [z z]) y) x)) #f)
(test-equal (term (does-not-bind? (letrec ([x x] [y hole] [z z]) y) y)) #f)
(test-equal (term (does-not-bind? (letrec ([x x] [y hole] [z z]) y) z)) #f)
(test-equal (term (does-not-bind? (letrec ([x x] [y hole] [z z]) y) a)) #t)
(test-equal (term (does-not-bind? (letrec ([x x]) (letrec ([y y]) hole)) y)) #f)
(test-equal (term (does-not-bind? (letrec ([x x]) (letrec ([y hole]) y)) y)) #f)
