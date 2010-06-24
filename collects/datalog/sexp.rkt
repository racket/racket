#lang scheme
(require "ast.ss")

(define sexpr? any/c)

(define (sexp-wrap s->)
  (lambda (sexp)
    (s-> (datum->syntax #f sexp #f))))

(define (stx->program stx)
  (syntax-case stx (begin)
    [(begin s ...)
     (map stx->statement (syntax->list #'(s ...)))]))
(define sexp->program (sexp-wrap stx->program))

(define (stx->statement stx)
  (syntax-case stx (! ~ ?)
    [(! c)
     (make-assertion stx (stx->clause #'c))]
    [(~ c)
     (make-retraction stx (stx->clause #'c))]
    [(? l)
     (make-query stx (stx->literal #'l))]))
(define sexp->statement (sexp-wrap stx->statement))

(define (stx->clause stx)
  (syntax-case stx (:-)
    [(:- l body ...)
     (make-clause stx (stx->literal #'l)
                  (map stx->literal (syntax->list #'(body ...))))]
    [l
     (make-clause stx (stx->literal #'l) empty)]))
(define sexp->clause (sexp-wrap stx->clause))

(define (stx->literal stx)
  (syntax-case stx ()
    [(p t ...)
     (make-literal
      stx (stx->datum #'p)
      (map stx->term (syntax->list #'(t ...))))]))
(define sexp->literal (sexp-wrap stx->literal))

(define (stx->term stx)
  (syntax-case stx (unquote)
    [(unquote d)
     (identifier? #'d)
     (make-variable stx (syntax->datum #'d))]
    [d
     (datum-syntax? #'d)
     (make-constant stx (stx->datum #'d))]))
(define sexp->term (sexp-wrap stx->term))

(define (datum-syntax? stx)
  (define d (syntax->datum stx))
  (or (symbol? d) (string? d)))
(define (stx->datum stx)
  (syntax-case stx ()
    [d
     (datum-syntax? #'d)
     (syntax->datum #'d)]))

(provide/contract
 [stx->program (syntax? . -> . program/c)]
 [stx->statement (syntax? . -> . statement/c)]
 [stx->clause (syntax? . -> . clause?)]
 [stx->literal (syntax? . -> . literal?)]
 [stx->term (syntax? . -> . term/c)]
 [sexp->program (sexpr? . -> . program/c)]
 [sexp->statement (sexpr? . -> . statement/c)]
 [sexp->clause (sexpr? . -> . clause?)]
 [sexp->literal (sexpr? . -> . literal?)]
 [sexp->term (sexpr? . -> . term/c)])