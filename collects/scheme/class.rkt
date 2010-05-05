#lang scheme/base
(require racket/class)
(provide (except-out (all-from-out racket/class)
                     printable<%>)
         (rename-out [writable<%> printable<%>]))
