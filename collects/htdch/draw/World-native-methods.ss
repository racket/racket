#cs
(module World-native-methods mzscheme
  (require (lib "support.scm" "htdch" "draw") (lib "unitsig.ss"))
  
  (provide endOfTime-native endOfWorld-native bigBangO-double-native)

  (define void-or-true #t)
  (define (imperative world@t+1 world@t) world@t+1)
  
  (define-values/invoke-unit/sig world-native^ world-native@ #f support^))


