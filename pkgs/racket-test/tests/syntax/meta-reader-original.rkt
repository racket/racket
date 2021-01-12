#lang racket/base
(define p (open-input-string "#lang at-exp racket/base\n"))
(port-count-lines! p)
(define stx
  (parameterize ([read-accept-reader #t]
                 [current-namespace (make-base-namespace)])
    (expand (read-syntax "x.rkt" p))))
(define r/b
  (syntax-case stx ()
    [(_1 _2 r/b _3 ...) #'r/b]))
(unless (syntax-original? r/b)
  (error "module name after `at-exp` is not `syntax-original?`"))

