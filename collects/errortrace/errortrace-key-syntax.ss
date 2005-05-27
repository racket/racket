(module errortrace-key-syntax mzscheme
  (require (lib "errortrace-key.ss" "errortrace"))
  (require-for-syntax (lib "errortrace-key.ss"  "errortrace"))
  (define errortrace-key-syntax #'errortrace-key)
  (provide errortrace-key-syntax))


