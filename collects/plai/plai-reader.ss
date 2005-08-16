(module plai-reader mzscheme
  (require (lib "etc.ss"))

  (provide (rename plai-read-syntax read-syntax))

  (define (read-syntax/namespace-introduce . args)
    (let ([v (apply read-syntax args)])
      (if (syntax? v)
          (namespace-syntax-introduce v)
          v)))
  
  (define (plai-read-syntax . args)
    (parameterize ([read-case-sensitive #t])
      (apply read-syntax/namespace-introduce args)))


  )