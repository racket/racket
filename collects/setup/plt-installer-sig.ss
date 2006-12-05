(module plt-installer-sig mzscheme
  (require (lib "unit.ss"))
  (provide setup:plt-installer^)
  (define-signature setup:plt-installer^
    (run-installer
     with-installer-window
     run-single-installer
     on-installer-run)))
