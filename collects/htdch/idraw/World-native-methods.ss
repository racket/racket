#cs
(module World-native-methods mzscheme
  (require "support.scm" (lib "unitsig.ss"))
  
  (provide endOfTime-native endOfWorld-native bigBangO-double-native)

  (define void-or-true void)
  (define (world-return w) w)
  
  (define-values/invoke-unit/sig world-native^ world-native@ #f support^))
