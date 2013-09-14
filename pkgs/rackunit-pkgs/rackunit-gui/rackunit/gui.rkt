#lang racket/base
(require racket/contract/base
         racket/gui/base
         rackunit/private/base
         rackunit/private/gui/gui)

(define (test/gui #:wait? [wait? #f]
                  . tests)
  (let* ([es (make-eventspace)]
         [runner
          (parameterize ((current-eventspace es))
            (make-gui-runner))])
    (sleep/yield 0.1) ;; give the gui a chance to initialize
    (apply runner tests)
    (when wait? (void (sync es)))))

(define test/c (or/c rackunit-test-case? rackunit-test-suite?))

(provide/contract
 [test/gui
  (->* ()
       (#:wait? any/c)
       #:rest (listof test/c)
       any)]
 [make-gui-runner
  (->* ()
       ()
       (->* () () #:rest (listof test/c)
            any))])
