#cs
(module World-native-methods mzscheme
  (require (lib "support.scm" "htdch" "draw") (lib "unitsig.ss"))
  
  (provide endOfTime-java.lang.String-native endOfWorld-java.lang.String-native bigBangO-double-native)

  (define void-or-true void)
  (define (imperative world@t+1 world@t) world@t)
  
  (define-values/invoke-unit/sig world-native^ world-native@ #f support^))
