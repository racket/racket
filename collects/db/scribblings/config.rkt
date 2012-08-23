#lang racket/base
(require scribble/manual
         scribble/eval
         unstable/sandbox
         racket/runtime-path
         (for-label racket/base
                    racket/contract))
(provide (all-defined-out)
         (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)))

(define (tech/reference . pre-flows)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") pre-flows))

(define (parheading . pre-flows)
  (elem (apply bold pre-flows) (hspace 1)))

(define (wplink path . pre-flows)
  (apply hyperlink (string-append "http://en.wikipedia.org/wiki/" path) pre-flows))

;; ----

#|
The log-based-eval should be run in an environment that defines
the DSN 'db-scribble-env as a PostgreSQL data source.
|#

(define-runtime-path example-log "example-log.rktd")
(define the-eval (make-log-based-eval example-log 'replay))

(the-eval '(require racket/class
                    db
                    db/util/postgresql
                    db/util/datetime))

#|
The fake eval is for eg connection examples
|#

(define fake-eval (make-base-eval))
(fake-eval '(begin (require racket/class)
                   (define connection% (class object% (super-new)))))

(define-syntax-rule (fake-examples [example result] ...)
  (examples #:eval fake-eval (eval:alts example result) ...))
