#lang racket
(require (planet dherman/pprint:4)
         "ast.rkt")

(define (format-datum s)
  (cond
    [(string? s)
     (text (format "~S" s))]
    [(symbol? s)
     (text (symbol->string s))]))
(define (format-variable v)
  (format-datum (variable-sym v)))
(define (format-constant c)
  (format-datum (constant-datum c)))
(define (format-term t)
  (cond
    [(variable? t)
     (format-variable t)]
    [(constant? t)
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
               (v-concat/s (apply-infix comma (map format-term terms)))
               rparen)]))
(define (format-literals ls)
  (v-concat
   (append (map (lambda (l)
                  (format-assertion (make-assertion #f (make-clause #f l (list)))))
                ls)
           (list line))))
(define (format-clause c)
  (if (empty? (clause-body c))
      (format-literal (clause-head c))
      (nest 4
            (v-concat/s
             (list* (h-append (format-literal (clause-head c)) space (text ":-"))
                    (apply-infix comma (map format-literal (clause-body c))))))))
(define (format-assertion a)
  (h-append (format-clause (assertion-clause a))
            dot))
(define (format-retraction r)
  (h-append (format-clause (retraction-clause r))
            (char #\~)))
(define (format-query q)
  (h-append (format-literal (query-literal q))
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
 [format-literals ((listof literal?) . -> . doc?)]
 [format-clause (clause? . -> . doc?)]
 [format-assertion (assertion? . -> . doc?)]
 [format-retraction (retraction? . -> . doc?)]
 [format-query (query? . -> . doc?)]
 [format-statement (statement/c . -> . doc?)]
 [format-program (program/c . -> . doc?)])