#lang racket/base
(require compiler/find-exe
         racket/system)

(define racket (find-exe))

(define result 0)

(define (run/get-stderr . args)
  (define o (open-output-bytes))
  (parameterize ([current-error-port o])
    (apply system* racket args))
  (get-output-bytes o))

(define (check expect got)
  (unless (equal? expect got)
    (eprintf "Expected ~s,\n     got ~s\n" expect got)
    (set! result 1)))

(check #"finalize: created\nfinalize: exiting\nfinalize: finalizing\n"
       (run/get-stderr "-W" "info@finalize"
                       "-e"
                       "(dynamic-require '(submod \"custodian-finalize-help.rkt\" check-exit) #f)"))

(check #"finalize: created\nfinalize: finalizing\nfinalize: exiting\n"
       (run/get-stderr "-W" "info@finalize"
                       "-e"
                       "(dynamic-require '(submod \"custodian-finalize-help.rkt\" check-gc) #f)"))

(exit result)
