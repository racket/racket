(module cust mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define servlet-cust (current-custodian))
  (define timeout 30)
  (define interface-version 'v1)
  
  (define (start ir)
    `(html
      (head (title "Custodian test"))
      (body 
       (p ,(if (eq? (current-custodian) servlet-cust)
               "It didn't work."
               "It did work."))))))
