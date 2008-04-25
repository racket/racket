#lang scheme 

(require (lib "support.scm" "htdch" "draw")
         mzlib/unit)

(define void-or-true #t)
(define (imperative w@t+1 w@t) w@t+1)

(define-values/invoke-unit/infer canvas-native@)

(provide-signature-elements canvas-native^)


