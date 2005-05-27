(module checksigs mzscheme
  (require (lib "unitsig.ss"))
  (provide empty^ extra-params^ defs^)
  (define-signature empty^
    ())
  (define-signature extra-params^
    (check-frame
     sync?))
  (define-signature defs^
    (run-thunk
     show-ok
     show-error-ok
     make-wait-dialog
     show-wait-dialog
     hide-wait-dialog)))
