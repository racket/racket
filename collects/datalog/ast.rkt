#lang racket/base
(require racket/contract
         racket/match)

(define srcloc/c
  (or/c #f
        (list/c any/c
                (or/c exact-positive-integer? #f)
                (or/c exact-nonnegative-integer? #f)
                (or/c exact-nonnegative-integer? #f)
                (or/c exact-positive-integer? #f))))

(define-struct predicate-sym (srcloc sym) #:prefab)
(define datum/c (or/c string? symbol? predicate-sym?))
(define (datum-equal? x y)
  (match* (x y)
    [((predicate-sym _ x) y)
     (datum-equal? x y)]
    [(x (predicate-sym _ y))
     (datum-equal? x y)]
    [(x y)
     (equal? x y)]))

(define-struct variable (srcloc sym) #:prefab)  
(define (variable-equal? v1 v2)
  (eq? (variable-sym v1) (variable-sym v2)))
(define-struct constant (srcloc value) #:prefab)
(define (constant-equal? v1 v2)
  (equal? (constant-value v1) (constant-value v2)))

(define term/c (or/c variable? constant?))
(define (term-equal? t1 t2)
  (cond
    [(and (variable? t1) (variable? t2))
     (variable-equal? t1 t2)]
    [(and (constant? t1) (constant? t2))
     (constant-equal? t1 t2)]
    [else
     #f]))

(define (terms-equal? t1 t2)
  (and (= (length t1)
          (length t2))
       (andmap term-equal? t1 t2)))

(define-struct literal (srcloc predicate terms) #:prefab)
(define (literal-equal? l1 l2)
  (and (datum-equal? (literal-predicate l1)
                     (literal-predicate l2))
       (terms-equal? (literal-terms l1) (literal-terms l2))))

(define-struct external (srcloc predicate-sym predicate arg-terms ans-terms) #:prefab)
(define (external-equal? e1 e2)
  (match-define (external _1 _s1 p1 ar1 an1) e1)
  (match-define (external _2 _s2 p2 ar2 an2) e2)
  (and (equal? p1 p2)
       (terms-equal? ar1 ar2)
       (terms-equal? an1 an2)))

(define question/c (or/c literal? external?))
(define (question-equal? q1 q2)
  (or (and (literal? q1) (literal? q2)
           (literal-equal? q1 q2))
      (and (external? q1) (external? q2)
           (external-equal? q1 q2))))

(define-struct clause (srcloc head body) #:prefab)
(define (clause-equal? c1 c2)
  (and (literal-equal? (clause-head c1)
                       (clause-head c2))
       (= (length (clause-body c1))
          (length (clause-body c2)))
       (andmap question-equal?
               (clause-body c1)
               (clause-body c2))))

(define-struct assertion (srcloc clause) #:prefab)
(define-struct retraction (srcloc clause) #:prefab)
(define-struct query (srcloc question) #:prefab)

(define statement/c (or/c assertion? retraction? query?))
(define program/c (listof statement/c))

(provide/contract
 [srcloc/c contract?]
 [datum/c contract?]
 [datum-equal? (datum/c datum/c . -> . boolean?)]
 [struct predicate-sym ([srcloc srcloc/c] [sym symbol?])]
 [struct variable ([srcloc srcloc/c]
                   [sym symbol?])]
 [variable-equal? (variable? variable? . -> . boolean?)]
 [struct constant ([srcloc srcloc/c]
                   [value any/c])]
 [constant-equal? (constant? constant? . -> . boolean?)]
 [term/c contract?]
 [term-equal? (term/c term/c . -> . boolean?)]
 [struct literal ([srcloc srcloc/c]
                  [predicate (or/c predicate-sym? string? symbol?)]
                  [terms (listof term/c)])]
 [literal-equal? (literal? literal? . -> . boolean?)]
 [struct external ([srcloc srcloc/c]
                   [predicate-sym symbol?]
                   [predicate procedure?]
                   [arg-terms (listof term/c)]
                   [ans-terms (listof term/c)])]
 [external-equal? (external? external? . -> . boolean?)]
 [question/c contract?]
 [question-equal? (question/c question/c . -> . boolean?)]
 [struct clause ([srcloc srcloc/c]
                 [head literal?]
                 [body (listof question/c)])]
 [clause-equal? (clause? clause? . -> . boolean?)]
 [struct assertion ([srcloc srcloc/c]
                    [clause clause?])]
 [struct retraction ([srcloc srcloc/c]
                     [clause clause?])]
 [struct query ([srcloc srcloc/c]
                [question question/c])]
 [statement/c contract?]
 [program/c contract?])
