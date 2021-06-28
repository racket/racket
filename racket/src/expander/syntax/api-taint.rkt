#lang racket/base
(require "syntax.rkt"
         "to-list.rkt"
         "scope.rkt"
         (rename-in "taint.rkt"
                    [syntax-tainted? raw:syntax-tainted?]
                    [syntax-taint raw:syntax-taint])
         (only-in "../expand/syntax-local.rkt" syntax-local-phase-level)
         "../namespace/core.rkt"
         "../common/contract.rkt"
         "../expand/log.rkt")

;; Provides public versions of taint-related syntax functions

(provide syntax-tainted?
         syntax-arm
         syntax-disarm
         syntax-rearm
         syntax-taint)

(define/who (syntax-tainted? s)
  (check who syntax? s)
  (raw:syntax-tainted? s))

(define/who (syntax-arm s [maybe-insp #f] [use-mode? #f])
  (check who syntax? s)
  (unless (or (not maybe-insp)
              (inspector? maybe-insp))
    (raise-argument-error who "(or/c inspector? #f)" maybe-insp))
  s)

(define/who (syntax-disarm s maybe-insp)
  (check who syntax? s)
  (unless (or (not maybe-insp)
              (inspector? maybe-insp))
    (raise-argument-error who "(or/c inspector? #f)" maybe-insp))
  s)

(define/who (syntax-rearm s from-s [use-mode? #f])
  (check who syntax? s)
  (check who syntax? from-s)
  s)

(define/who (syntax-taint s)
  (check who syntax? s)
  (raw:syntax-taint s))
