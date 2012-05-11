#lang racket/base
(require "srcdoc.rkt")

(provide define-provide/doc-transformer
         (for-syntax
          provide/doc-transformer?
          provide/doc-transformer-proc))
