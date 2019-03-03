(let ([args (command-line-arguments)])
  (compile-file (car args) "cross-serve.so"))
