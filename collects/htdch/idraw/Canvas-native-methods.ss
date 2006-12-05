#cs
(module Canvas-native-methods mzscheme
  (require (lib "support.scm" "htdch" "draw") (lib "unit.ss"))

  (define void-or-true (void))
  (define (imperative w@t+1 w@t) w@t+1)
  
  (define-values/invoke-unit/infer canvas-native@)

  (provide-signature-elements canvas-native^))
