#lang racket/base

(require
 (rename-in "../utils/utils.rkt" [infer r:infer])
 (types abbrev numeric-tower) (env init-envs) (r:infer infer-dummy infer)
 "base-env-indexing-abs.rkt")

(define e (parameterize ([infer-param infer]) (indexing -Integer)))
(define (initialize-indexing) (initialize-type-env e))
(provide initialize-indexing)


