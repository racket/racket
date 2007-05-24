
(module exn mzscheme
  (require (lib "serialize.ss"))

  (define-serializable-struct mr-exn (message))

  (provide (struct mr-exn (message))))


