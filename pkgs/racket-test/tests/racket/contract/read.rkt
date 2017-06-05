#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace (make-basic-contract-namespace
                                             'racket/contract
                                             'racket/port)])

  (test/spec-passed
    'read/contract-1
    '(with-input-from-string "1"
       (lambda () (read/contract integer?))))

  (test/spec-passed
    'read/contract-2
    '(with-input-from-string "(1 2 3)"
       (lambda ()
         (read/contract (listof (integer-in 1 3))))))


  (test/spec-failed
    'read/contract-3
    '(with-input-from-string "1"
       (lambda () (read/contract symbol?)))
    "#<input-port:string>")

  (test/spec-failed
    'read/contract-4
    '(with-input-from-string "(1 2 three)"
       (lambda () (read/contract (listof integer?))))
    "#<input-port:string>")

  (test/spec-failed
    'read/contract-5
    '(with-input-from-string "#(a b c)"
       (lambda ()
         (let ([v (read/contract (vectorof symbol?))])
           (vector-set! v 0 0))))
    "top-level")
)
