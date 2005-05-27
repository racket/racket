(module uri-codec-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide net:uri-codec^)

  (define-signature net:uri-codec^
    (uri-encode
     uri-decode
     form-urlencoded-encode
     form-urlencoded-decode
     alist->form-urlencoded
     form-urlencoded->alist
     current-alist-separator-mode)))