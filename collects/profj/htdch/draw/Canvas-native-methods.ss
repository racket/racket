#lang scheme

(require (lib "profj/htdch/draw/support.scm")
         mzlib/unit)

(define void-or-true #t)
(define (imperative w@t+1 w@t) w@t+1)

(define-values/invoke-unit/infer canvas-native@)

(provide-signature-elements canvas-native^)


