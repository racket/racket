(module web-server-structs mzscheme
  (require (lib "contract.ss"))
  
  (define current-server-custodian (make-parameter #f))
  (provide current-server-custodian) ; parameter
  
  ;; make-servlet-custodian: -> custodian
  ;; create a custodian for the dynamic extent of a servlet continuation
  (define (make-servlet-custodian)
    (make-custodian (current-server-custodian)))
  
  (provide/contract
   [make-servlet-custodian (-> custodian?)]))