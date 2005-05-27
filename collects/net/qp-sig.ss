(module qp-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide net:qp^)
  (define-signature net:qp^
    (
     ;; -- exceptions raised --
     (struct qp-error () -setters (- make-qp-error))
     (struct qp-wrong-input () -setters (- make-qp-wrong-input))
     (struct qp-wrong-line-size (size) -setters (- make-qp-wrong-line-size))
     
     ;; -- qp methods --
     qp-encode
     qp-decode
     qp-encode-stream
     qp-decode-stream
     )))
