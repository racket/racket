#lang scheme

(define srcloc/c
  (or/c syntax?
        false/c
        (list/c any/c
                (or/c exact-positive-integer? #f)
                (or/c exact-nonnegative-integer? #f)
                (or/c exact-nonnegative-integer? #f)
                (or/c exact-positive-integer? #f))))

(define datum/c (or/c string? symbol?))
(define datum-equal? equal?)

(define-struct variable (srcloc sym) #:prefab)  
(define (variable-equal? v1 v2)
  (eq? (variable-sym v1) (variable-sym v2)))
(define-struct constant (srcloc datum) #:prefab)
(define (constant-equal? v1 v2)
  (datum-equal? (constant-datum v1) (constant-datum v2)))

(define term/c (or/c variable? constant?))
(define (term-equal? t1 t2)
  (cond
    [(and (variable? t1) (variable? t2))
     (variable-equal? t1 t2)]
    [(and (constant? t1) (constant? t2))
     (constant-equal? t1 t2)]
    [else
     #f]))

(define-struct literal (srcloc predicate terms) #:prefab)
(define (literal-equal? l1 l2)
  (and (datum-equal? (literal-predicate l1)
                     (literal-predicate l2))
       (= (length (literal-terms l1))
          (length (literal-terms l2)))
       (andmap term-equal?
               (literal-terms l1)
               (literal-terms l2))))

(define-struct clause (srcloc head body) #:prefab)
(define (clause-equal? c1 c2)
  (and (literal-equal? (clause-head c1)
                       (clause-head c2))
       (= (length (clause-body c1))
          (length (clause-body c2)))
       (andmap literal-equal?
               (clause-body c1)
               (clause-body c2))))

(define-struct assertion (srcloc clause) #:prefab)
(define-struct retraction (srcloc clause) #:prefab)
(define-struct query (srcloc literal) #:prefab)

(define statement/c (or/c assertion? retraction? query?))
(define program/c (listof statement/c))

(provide/contract
 [srcloc/c contract?]
 [datum/c contract?]
 [datum-equal? (datum/c datum/c . -> . boolean?)]
 [struct variable ([srcloc srcloc/c]
                   [sym symbol?])]
 [variable-equal? (variable? variable? . -> . boolean?)]
 [struct constant ([srcloc srcloc/c]
                   [datum datum/c])]
 [constant-equal? (constant? constant? . -> . boolean?)]
 [term/c contract?]
 [term-equal? (term/c term/c . -> . boolean?)]
 [struct literal ([srcloc srcloc/c]
                  [predicate datum/c]
                  [terms (listof term/c)])]
 [literal-equal? (literal? literal? . -> . boolean?)]
 [struct clause ([srcloc srcloc/c]
                 [head literal?]
                 [body (listof literal?)])]
 [clause-equal? (clause? clause? . -> . boolean?)]
 [struct assertion ([srcloc srcloc/c]
                    [clause clause?])]
 [struct retraction ([srcloc srcloc/c]
                     [clause clause?])]
 [struct query ([srcloc srcloc/c]
                [literal literal?])]
 [statement/c contract?]
 [program/c contract?])
