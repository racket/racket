#lang racket/base

(require stepper/private/model-settings)

(provide (all-defined-out))

;; DEFINING A LANGUAGE FOR THE PURPOSES OF TESTING

;; ll-model : a representation of the behavior of a language level w.r.t. the stepper
(define-struct ll-model (namespace-spec render-settings enable-testing?))

;; the built-in ll-models:
(define mz
  (make-ll-model 'mzscheme fake-mz-render-settings #f))

(define beginner
  (make-ll-model `(lib "htdp-beginner.ss" "lang") fake-beginner-render-settings #t))

(define beginner-wla
  (make-ll-model `(lib "htdp-beginner-abbr.ss" "lang") fake-beginner-wla-render-settings #t))

(define intermediate
  (make-ll-model `(lib "htdp-intermediate.ss" "lang") fake-intermediate-render-settings #t))

(define intermediate-lambda
  (make-ll-model `(lib "htdp-intermediate-lambda.ss" "lang") fake-intermediate/lambda-render-settings #t))

(define advanced
  (make-ll-model `(lib "htdp-advanced.ss" "lang") fake-advanced-render-settings #t))

(define lazy
  (make-ll-model `(lib "lazy.rkt" "lazy") fake-lazy-render-settings #f))


;; unsure about the render-settings, here: 
(define dmda-a
  (make-ll-model `(lib "DMdA-beginner.ss" "deinprogramm") fake-beginner-render-settings #t))


;; SUPPORT FOR TESTING A BUNCH OF LANGUAGES AT ONCE:

;; built-in multi-language bundles:
(define upto-int/lam
  (list beginner
        beginner-wla
        intermediate
        intermediate-lambda))

(define upto-int
  (list beginner
        beginner-wla
        intermediate))

(define bwla-to-int/lam
  (list beginner-wla
        intermediate
        intermediate-lambda))

(define both-intermediates
  (list intermediate
        intermediate-lambda))



