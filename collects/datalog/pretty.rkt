#lang racket/base
(require racket/match
         racket/list
         racket/contract
         "private/pprint.rkt"
         "ast.rkt")

(define format-datum
  (match-lambda
   [(predicate-sym _ s)
    (format-datum s)]
   [(? symbol? s)
    (text (symbol->string s))]
   [(? string? s)
    (text (format "~S" s))]
   [(? number? s)
    (text (format "~S" s))]))
(define (format-variable v)
  (format-datum (variable-sym v)))
(define (format-constant c)
  (format-datum (constant-value c)))
(define format-term 
  (match-lambda
    [(? variable? t)
     (format-variable t)]
    [(? constant? t)
     (format-constant t)]))
(define (format-literal l)
  (match l
    [(struct literal (_ pred (list)))
     (format-datum pred)]
    [(struct literal (_ '= (list a b)))
     (h-append (format-term a) space (text "=") space (format-term b))]
    [(struct literal (_ pred terms))
     (h-append (format-datum pred)
               lparen
               (v-concat/s (apply-infix ", " (map format-term terms)))
               rparen)]))
(define format-external
  (match-lambda
    [(external _ pred-sym pred args anss)
     (h-append (format-datum pred-sym)
               lparen
               (v-concat/s (apply-infix ", " (map format-term args)))
               rparen
               (text " = ")
               lparen
               (v-concat/s (apply-infix ", " (map format-term anss)))
               rparen)]))
(define format-question
  (match-lambda
    [(? literal? l)
     (format-literal l)]
    [(? external? e)
     (format-external e)]))
(define (format-questions ls)
  (v-concat
   (map (lambda (l)
          (h-append (format-question l) dot))
        ls)))
(define (format-clause c)
  (if (empty? (clause-body c))
      (format-literal (clause-head c))
      (nest 4
            (v-concat/s
             (list* (h-append (format-literal (clause-head c)) space (text ":-") space)
                    (apply-infix ", " (map format-literal (clause-body c))))))))
(define (format-assertion a)
  (h-append (format-clause (assertion-clause a))
            dot))
(define (format-retraction r)
  (h-append (format-clause (retraction-clause r))
            (char #\~)))
(define (format-query q)
  (h-append (format-question (query-question q))
            (char #\?)))

(define (format-statement s)
  (cond
    [(assertion? s) (format-assertion s)]
    [(retraction? s) (format-retraction s)]
    [(query? s) (format-query s)]))
(define (format-program p)
  (v-concat (map format-statement p)))

(provide/contract
 [format-datum (datum/c . -> . doc?)]
 [format-variable (variable? . -> . doc?)]
 [format-constant (constant? . -> . doc?)]
 [format-term (term/c . -> . doc?)]
 [format-literal (literal? . -> . doc?)]
 [format-questions ((listof question/c) . -> . doc?)]
 [format-clause (clause? . -> . doc?)]
 [format-assertion (assertion? . -> . doc?)]
 [format-retraction (retraction? . -> . doc?)]
 [format-query (query? . -> . doc?)]
 [format-statement (statement/c . -> . doc?)]
 [format-program (program/c . -> . doc?)])
