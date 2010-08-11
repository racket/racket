(module base "pre-base.rkt"
  
  (#%require "hash.rkt"
             "list.rkt"
             "string.rkt"
             "stxcase-scheme.rkt"
             "qqstx.rkt"
             "stx.rkt"
             "kw-file.rkt"
             "namespace.rkt"
             "struct.rkt"
             (for-syntax "stxcase-scheme.rkt"))

  (#%provide (all-from-except "pre-base.rkt"
                              open-input-file
                              open-output-file
                              open-input-output-file
                              call-with-input-file
                              call-with-output-file
                              with-input-from-file
                              with-output-to-file
                              regexp-replace*
                              new-apply-proc)
             struct
             (all-from "hash.rkt")
             (all-from "list.rkt")
             (all-from-except "string.rkt" 
                              -regexp-replace*)
             (rename -regexp-replace* regexp-replace*)
             identifier?
             (all-from "stxcase-scheme.rkt")
             (all-from "qqstx.rkt")
             (all-from "namespace.rkt")
             (for-syntax syntax-rules syntax-id-rules ... _)
             (rename -open-input-file open-input-file)
             (rename -open-output-file open-output-file)
             (rename -open-input-output-file open-input-output-file)
             (rename -call-with-input-file call-with-input-file)
             (rename -call-with-output-file call-with-output-file)
             (rename -with-input-from-file with-input-from-file)
             (rename -with-output-to-file with-output-to-file)
             call-with-input-file*
             call-with-output-file*))

