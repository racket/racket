
(module debug mzscheme
  (require (lib "pretty.ss")
           "debug-format.ss"
           "view.ss")
  (provide debug-file)

  (define (debug-file file)
    (let-values ([(events msg ctx) (load-debug-file file)])
      (pretty-print msg)
      (pretty-print ctx)
      (go/trace events)))
  
  )
