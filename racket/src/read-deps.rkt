#lang racket/base
(require compiler/depend)

(for-each
 (lambda (p) (writeln (path->bytes p)))
 (module-recorded-dependencies (vector-ref (current-command-line-arguments) 0)))
