#lang scheme/base
(require racket/sandbox
         scheme/gui/dynamic)

(provide (except-out (all-from-out racket/sandbox)
                     sandbox-namespace-specs
                     make-evaluator
                     make-module-evaluator)
         (rename-out
          [scheme:sandbox-namespace-specs sandbox-namespace-specs]
          [scheme:make-evaluator make-evaluator]
          [scheme:make-module-evaluator make-module-evaluator]))

;; copied from racket/sandbox  :(
(define-syntax mz/mr ; use a value for plain racket, or pull a gui binding
  (syntax-rules ()
    [(mz/mr mzval mrsym)
     (if gui? (gui-dynamic-require 'mrsym) mzval)]))


(define scheme:sandbox-namespace-specs
  (make-parameter `(,(mz/mr make-base-namespace make-gui-namespace))))

(define (scheme:make-evaluator language
                               #:requires [requires null] #:allow-read [allow null]
                               . input-program)
  (parameterize ([sandbox-namespace-specs (scheme:sandbox-namespace-specs)])
    (apply make-evaluator 
           language #:requires requires #:allow-read allow
           input-program)))

(define (scheme:make-module-evaluator
         input-program #:allow-read [allow null] #:language [reqlang #f])
  (parameterize ([sandbox-namespace-specs (scheme:sandbox-namespace-specs)])
    (make-module-evaluator
     input-program #:allow-read allow #:language reqlang)))
