(module qp-sig (lib "a-signature.ss")
  ;; -- exceptions raised --
  (struct qp-error () -setters -constructor)
  (struct qp-wrong-input () -setters -constructor)
  (struct qp-wrong-line-size (size) -setters -constructor)

  ;; -- qp methods --
  qp-encode
  qp-decode
  qp-encode-stream
  qp-decode-stream
  )
