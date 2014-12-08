#lang racket/base
(require racket/place)

(define (go)
  (place ch
         (printf "running\n")
         (parameterize ([current-namespace (make-base-namespace)])
           (eval '(module with-contract-#%app-app racket/kernel
                    (#%require '#%network)
                    (#%declare #:cross-phase-persistent))))
         (printf "done\n")))

(module+ main
  (printf "creating place\n")
  (define p (go))
  (printf "waiting\n")
  (place-wait p))
