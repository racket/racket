;; module loader for SRFI-69
(module |69| mzscheme
  (require (lib "hash.ss" "srfi" "69"))
  (provide (all-from-except (lib "hash.ss" "srfi" "69")
                            s:make-hash-table
                            s:hash-table?
                            s:hash-table-copy)
           (rename s:make-hash-table make-hash-table)
           (rename s:hash-table? hash-table?)
           (rename s:hash-table-copy hash-table-copy)))
