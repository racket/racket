#lang typed-scheme

(require scheme/promise)

;((plambda: (a) ([x : a]) x) (error 'foo))

(force (delay 3))

(call-with-values (lambda () 3) list)
