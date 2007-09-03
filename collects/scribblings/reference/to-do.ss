
(module to-do mzscheme

  (define make-resolved-module-path #f)
  (define resolved-module-path? #f)
  (define resolved-module-path->path #f)
  (define module-path? #f)

  (define call-with-input-file* #f)
  (define call-with-output-file* #f)

  (define free-transformer-identifier=? #f)
  (define free-template-identifier=? #f)
  (define free-label-identifier=? #f)

  (define syntax->datum #f)
  (define datum->syntax #f)

  (define-syntax else #f)
  (define-syntax _ #f)

  (define list-mutableof #t)
  (define list-mutable/c #t)
  (define cons-mutable/c #t)

  (define cons? #f)
  (define first #f)
  (define rest #f)
  (define empty? #f)
  (define empty #f)

  (define arity? #f)

  (provide (all-defined)))
