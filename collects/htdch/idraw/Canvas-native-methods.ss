#cs
(module Canvas-native-methods mzscheme
  (require (lib "support.scm" "htdch" "draw") (lib "unitsig.ss"))

  (define void-or-true (void))
  (define (imperative w@t+1 w@t) w@t+1)
  
  (define-values/invoke-unit/sig canvas-native^ canvas-native@ #f support^)

  (provide-signature-elements canvas-native^))
