#lang racket/base

(provide (struct-out merged))

(struct merged (body
                used-import-names
                any-syntax-literals?
                any-transformer-registers?
                defined-names)
  #:transparent)
