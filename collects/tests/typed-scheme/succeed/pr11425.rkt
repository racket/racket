#lang racket/load

(module sgn-exporter typed/racket/base
  (require/typed
   racket/math
   [sgn  (Integer -> Fixnum)])
  (provide (all-defined-out)))

(module sgn-importer typed/racket/base
  (require racket/math 'sgn-exporter))

(require 'sgn-exporter)
