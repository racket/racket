
(module base "private/pre-base.ss"
  (#%require "private/list.ss"
             "private/string.ss"
             "private/stxcase-scheme.ss"
             "private/qqstx.ss"
             "private/stx.ss"
             "private/kw-file.ss"
             "private/namespace.ss"
             (for-syntax "private/stxcase-scheme.ss"))

  (#%provide (all-from-except "private/pre-base.ss"
                              open-input-file
                              open-output-file
                              open-input-output-file
                              call-with-input-file
                              call-with-output-file
                              with-input-from-file
                              with-output-to-file
                              regexp-replace*)
             (all-from "private/list.ss")
             (all-from-except "private/string.ss" 
                              -regexp-replace*)
             (rename -regexp-replace* regexp-replace*)
             identifier?
             (all-from "private/stxcase-scheme.ss")
             (all-from "private/qqstx.ss")
             (all-from "private/namespace.ss")
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



             

