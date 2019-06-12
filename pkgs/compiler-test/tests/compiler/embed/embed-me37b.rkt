#lang racket/base

(list 'b
      (variable-reference->module-source
       (#%variable-reference)))
