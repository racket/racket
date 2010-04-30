(module browser mzscheme
  (require mzlib/unit
           mred
           mred/mred-sig
           setup/plt-installer-sig
           setup/plt-installer
           net/tcp-sig
           net/url-sig
           net/url
           "browser-sig.ss"
           "browser-unit.ss")
  
  (provide-signature-elements browser^)
  
  (define-values/invoke-unit/infer browser@))
