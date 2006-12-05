#cs
(module World-native-methods mzscheme
  (require (lib "support.scm" "htdch" "draw") (lib "unit.ss"))
  
  (provide endOfTime-java.lang.String-native endOfWorld-java.lang.String-native bigBangO-double-native)

  (define void-or-true #t)
  (define (imperative world@t+1 world@t) world@t+1)
  
  (define-values/invoke-unit/infer world-native@))
