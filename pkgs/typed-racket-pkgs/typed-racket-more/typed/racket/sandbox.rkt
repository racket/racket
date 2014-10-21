#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides a base type environment for
;; racket/sandbox

(require racket/sandbox
         (for-syntax (only-in typed-racket/rep/type-rep make-ValuesDots)))

(provide exn:fail:resource?
         exn:fail:resource-resource)

(type-environment
 ;; 14.12 Sandboxed Evaluation
 ;; 14.12.1 Customizing Evaluators
 ;; 14.12.1 Interacting with Evaluators
 ;; 14.12.3 Miscellaneous
 [gui? -Boolean]
 [call-with-limits
  (-polydots (a)
    (-> (-opt -Integer) (-opt -Integer)
        (-> (make-ValuesDots null a 'a))
        (make-ValuesDots null a 'a)))]
 [call-with-deep-time-limit
  (-polydots (a)
    (-> (-opt -Integer)
        (-> (make-ValuesDots null a 'a))
        (make-ValuesDots null a 'a)))]
 [#:struct (exn:fail:resource exn:fail)
           ([resource : (one-of/c 'time 'memory 'deep-time)])
           (-String -Cont-Mark-Set)
           #:no-provide])
