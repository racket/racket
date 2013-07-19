(module datum '#%kernel
  (#%require racket/private/stxcase-scheme
             racket/private/qqstx)
  (#%provide datum datum-case with-datum
             quasidatum undatum undatum-splicing))
