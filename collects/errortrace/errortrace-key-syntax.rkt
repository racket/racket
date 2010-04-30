(module errortrace-key-syntax mzscheme
  (require errortrace/errortrace-key)
  (require-for-syntax errortrace/errortrace-key)
  (define errortrace-key-syntax #'errortrace-key)
  (provide errortrace-key-syntax))
