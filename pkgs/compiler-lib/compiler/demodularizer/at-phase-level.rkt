#lang racket/base

(provide (struct-out at-phase-level))

(struct at-phase-level (phase+submod phase)
  #:prefab)
