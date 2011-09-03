#lang scheme/load

(module source mzscheme

    (require mzlib/contract)

    (define-struct ast (loc))

    (provide/contract (struct ast ([loc srcloc?])))
  )

(module client typed-scheme

  (require-typed-struct ast ([loc : Any]) 'source))
