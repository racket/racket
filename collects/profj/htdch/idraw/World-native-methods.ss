#lang scheme 
  (require (lib "profj/htdch/draw/support.scm")
           mzlib/unit)
  
(provide endOfTime-java.lang.String-native endOfWorld-java.lang.String-native bigBangO-double-native)

(define void-or-true void)
(define (imperative world@t+1 world@t) world@t)
  
(define-values/invoke-unit/infer world-native@)
