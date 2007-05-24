
(module exn mzscheme
  (require (lib "serialize.ss"))

  (define-serializable-struct mr-exn (message))

  ;; Design to print the same as the real canvas:
  (define-struct canvas-super ())
  (define-serializable-struct (object:canvas% canvas-super) () #f)

  (provide (struct mr-exn (message))
           make-object:canvas%))

