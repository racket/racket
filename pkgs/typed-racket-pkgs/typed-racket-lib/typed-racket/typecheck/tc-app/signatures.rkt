#lang racket/base
(require racket/unit
         "../../utils/utils.rkt"
         (contract-req)
         (utils unit-utils))
(require-for-cond-contract syntax/parse/experimental/reflect)

(provide (all-defined-out))

(define-for-cond-contract checker/c reified-syntax-class?)

(define-signature tc-app-hetero^
  ([cond-contracted tc/app-hetero checker/c]))

(define-signature tc-app-list^
  ([cond-contracted tc/app-list checker/c]))

(define-signature tc-app-apply^
  ([cond-contracted tc/app-apply checker/c]))

(define-signature tc-app-values^
  ([cond-contracted tc/app-values checker/c]))

(define-signature tc-app-keywords^
  ([cond-contracted tc/app-keywords checker/c]))

(define-signature tc-app-objects^
  ([cond-contracted tc/app-objects checker/c]))

(define-signature tc-app-eq^
  ([cond-contracted tc/app-eq checker/c]))

(define-signature tc-app-lambda^
  ([cond-contracted tc/app-lambda checker/c]))

(define-signature tc-app-special^
  ([cond-contracted tc/app-special checker/c]))

