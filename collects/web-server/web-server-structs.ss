(module web-server-structs mzscheme
  (provide (all-defined))
  
  (define current-server-custodian (make-parameter #f))
  
  ;; make-servlet-custodian: -> custodian
  ;; create a custodian for the dynamic extent of a servlet continuation
  (define (make-servlet-custodian)
    (make-custodian (current-server-custodian))))