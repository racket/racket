(module browser mzscheme
  (require (lib "unit.ss")
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")
           (lib "tcp-sig.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url.ss" "net")
           "browser-sig.ss"
           "browser-unit.ss")
  
  (provide-signature-elements browser^)
  
  (define-values/invoke-unit/infer browser@))
