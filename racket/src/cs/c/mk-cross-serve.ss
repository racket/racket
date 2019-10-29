(let ([args (command-line-arguments)])
  (load (cadr args))
  (compile-file (car args) "cross-serve.so"))
