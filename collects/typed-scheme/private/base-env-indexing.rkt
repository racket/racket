#lang scheme

(require
 (rename-in "../utils/utils.rkt" [infer r:infer])
 (for-syntax (types abbrev) (env init-envs) (r:infer infer-dummy infer)
             "base-env-indexing-abs.rkt"))

(define-for-syntax e (parameterize ([infer-param infer]) (indexing -Nat)))
(begin-for-syntax (initialize-type-env e))


