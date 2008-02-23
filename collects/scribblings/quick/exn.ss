
(module exn mzscheme
  (require mzlib/serialize)

  (define-serializable-struct mr-exn (message))

  (provide (struct mr-exn (message))))


