(module browser mzscheme
  (require mzlib/unit
           mred
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")
           net/tcp-sig
           net/url-sig
           net/url
           "browser-sig.ss"
           "browser-unit.ss")
  
  (provide-signature-elements browser^)
  
  (define-values/invoke-unit/infer browser@))
