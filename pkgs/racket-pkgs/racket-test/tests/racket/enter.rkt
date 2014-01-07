#lang racket
(require racket/enter)

;; Make sure that when a module has a syntax error, we still
;; switch into the module's namespace:
(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module f racket/base (define f 'yes) (raise-user-error 'oops "broken")))
  (with-handlers ([exn:fail:user? void]) (enter! 'f))
  (unless (eq? 'yes (eval 'f))
    (error "not in f?")))

;; Make sure that `enter!' can work on lots of modules:
(enter! pict)
(enter! #f)
(dynamic-enter! 'pict)
(dynamic-enter! #f)
