#lang racket/base

(require (except-in racket/class class)
         typed-racket/base-env/class-prims)

(provide class
         (all-from-out racket/class))
