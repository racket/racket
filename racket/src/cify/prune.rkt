#lang racket/base
(require "match.rkt")

(provide prune-unused)

;; Inlining by schemify can make function and constant definitions unused

(define (prune-unused e exports)
  (define candidate-ids (make-hasheq))

  (define (find-candidate-ids! e)
    (match e
      [`(define ,id ,rhs)
       (when (immediate? rhs)
         (hash-set! candidate-ids id #t))]
      [`(begin . ,es)
       (for ([e (in-list es)])
         (find-candidate-ids! e))]
      [`,_ (void)]))

  (define (prune e)
    (match e
      [`(define ,id ,rhs)
       (if (hash-ref candidate-ids id #f)
           `(begin)
           e)]
      [`(begin . ,es)
       `(begin . ,(for/list ([e (in-list es)])
                    (prune e)))]
      [`,_ e]))

  (define (immediate? rhs)
    (match rhs
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`(quote . ,_) #t]
      [`,_ (symbol? rhs)]))

  (define (find! e)
    (match e
      [`(define ,id ,rhs)
       (find! rhs)]
      [`(define-values ,ids ,rhs)
       (find! rhs)]
      [`(begin . ,body)
       (find-body! body)]
      [`(begin0 . ,body)
       (find-body! body)]
      [`(lambda ,_ . ,body)
       (find-body! body)]
      [`(case-lambda [,_ . ,bodys] ...)
       (for ([body (in-list bodys)])
         (find-body! body))]
      [`(if ,tst ,thn ,els)
       (find! tst)
       (find! thn)
       (find! els)]
      [`(with-continuation-mark ,key ,val ,body)
       (find! key)
       (find! val)
       (find! body)]
      [`(let . ,_) (find-let! e)]
      [`(letrec . ,_) (find-let! e)]
      [`(letrec* . ,_) (find-let! e)]
      [`(set! ,id ,rhs)
       (find! id)
       (find! rhs)]
      [`(,rator ,rands ...)
       (find-body! e)]
      [`,_ (hash-remove! candidate-ids e)]))

  (define (find-body! body)
    (for ([e (in-list body)])
      (find! e)))
  
  (define (find-let! e)
    (match e
      [`(,let-id ([,_ ,rhss] ...) . ,body)
       (for ([rhs (in-list rhss)])
         (find! rhs))
       (find-body! body)]))

  (find-candidate-ids! e)
  (for ([ex (in-list exports)])
    (hash-remove! candidate-ids (car ex)))
  (find! e)
  (prune e))
