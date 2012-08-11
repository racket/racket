#lang racket/base

(require
 "../utils/utils.rkt"
 (types numeric-tower) (env init-envs)
 "base-env-indexing-abs.rkt")

(define e (indexing -Integer))
(define (initialize-indexing) (initialize-type-env e))
(provide initialize-indexing)


