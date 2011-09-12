#lang racket/load

(module m typed/racket (define x 1))

(require 'm)

(current-namespace (module->namespace ''m))

(eval 'x)
(eval 5)
