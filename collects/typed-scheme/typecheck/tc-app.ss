#lang scheme/unit

(require (rename-in "../utils/utils.ss" [infer r:infer])
         "signatures.ss"
         stxclass
         (for-syntax stxclass)
         (rep type-rep filter-rep object-rep))

(import tc-expr^ tc-lambda^ tc-dots^ tc-let^)
(export tc-app^)


(define (tc/app . args)
  (error "tc/app NYI"))

(define (tc/app/check . args)
  (error "tc/app/check NYI"))

(define (tc/funapp . args)
  (error "tc/funapp NYI"))
