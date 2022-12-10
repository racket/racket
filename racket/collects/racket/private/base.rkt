(module base "pre-base.rkt"

  (#%require "hash.rkt"
             "list.rkt" ; shadows `reverse', `mem{q,v,ber}'
             "string.rkt"
             "stxcase-scheme.rkt"
             "qqstx.rkt"
             "stx.rkt"
             "kw-file.rkt"
             "namespace.rkt"
             "struct.rkt"
             "cert.rkt"
             "submodule.rkt"
             "generic-interfaces.rkt"
             "print-value-columns.rkt"
             "kw-syntax-binding.rkt" ; shadows `syntax-binding-set-extend`
             "kw-syntax-serialize.rkt" ; shadows `syntax-serialize` and `syntax-deserialize
             (for-syntax "stxcase-scheme.rkt"))

  (#%provide (all-from-except "pre-base.rkt"
                              open-input-file
                              open-output-file
                              open-input-output-file
                              call-with-input-file
                              call-with-output-file
                              with-input-from-file
                              with-output-to-file
                              directory-list
                              copy-file
                              regexp-replace*
                              new-apply-proc
                              do-raise-syntax-error
                              raise-syntax-error)
             struct
             (all-from-except "hash.rkt" paired-fold)
             (all-from "list.rkt")
             (all-from-except "string.rkt"
                              -regexp-replace*)
             (rename -regexp-replace* regexp-replace*)
             identifier?
             (all-from-except "stxcase-scheme.rkt" datum datum-case with-datum)
             (all-from-except "qqstx.rkt" quasidatum undatum undatum-splicing)
             (all-from "namespace.rkt")
             (all-from "cert.rkt")
             (all-from "submodule.rkt")
             (all-from "generic-interfaces.rkt")
             (all-from "print-value-columns.rkt")
             (all-from "kw-syntax-binding.rkt")
             (all-from "kw-syntax-serialize.rkt")
             (for-syntax syntax-rules syntax-id-rules ... _)
             (rename -open-input-file open-input-file)
             (rename -open-output-file open-output-file)
             (rename -open-input-output-file open-input-output-file)
             (rename -call-with-input-file call-with-input-file)
             (rename -call-with-output-file call-with-output-file)
             (rename -with-input-from-file with-input-from-file)
             (rename -with-output-to-file with-output-to-file)
             (rename -directory-list directory-list)
             (rename -copy-file copy-file)
             (rename -raise-syntax-error raise-syntax-error)
             call-with-input-file*
             call-with-output-file*))
