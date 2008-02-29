#lang scheme/base

(require (only-in r6rs)
         r6rs/private/parse-ref)

(provide (rename-out [r6rs:eval eval])
         environment)

(define-namespace-anchor anchor)

(define (r6rs:eval expr env)
  (eval #`(#%expression #,expr) env))

(define (environment . specs)
  (let ([mod-paths
         (map (lambda (spec)
                `(lib ,(parse-library-reference
                        spec
                        (lambda (msg)
                          (error 'environment "~a: ~e" msg spec)))))
              specs)])
    (let ([ns (namespace-anchor->empty-namespace anchor)])
      ;; Make sure all modules are instantiated here:
      (parameterize ([current-namespace ns])
        (for-each namespace-require mod-paths))
      ns)))

