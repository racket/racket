#lang racket/base
(require "../unsafe.rkt"
         (submod "../unsafe.rkt" static))


(provide (except-out (all-from-out "../unsafe.rkt")
                     _fun)
         (rename-out [_fun/static _fun]))

