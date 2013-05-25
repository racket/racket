#lang racket/base
(require racket/contract
         racket/list
         "../ast.rkt")

(define env/c
  (and/c hash? immutable?))
(define (empty-env)
  (make-immutable-hash empty))
(define (lookup env var [def #f])
  (hash-ref env var def))
(define (extend env var val)
  (hash-set env var val))

(provide/contract
 [env/c contract?]
 [empty-env (-> env/c)]
 [lookup ((env/c symbol?) (term/c) . ->* . (or/c false/c term/c))]
 [extend (env/c symbol? term/c . -> . void)])
