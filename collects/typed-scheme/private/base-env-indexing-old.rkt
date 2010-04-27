#lang scheme

(require
 (rename-in "../utils/utils.ss" [infer r:infer])
 (for-syntax (types abbrev) (env init-envs) (r:infer infer-dummy infer)
             "base-env-indexing-abs.ss"))

(define-for-syntax e (parameterize ([infer-param infer]) (indexing -Integer)))
(begin-for-syntax (initialize-type-env e))


