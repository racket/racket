;; module loader for SRFI-69
(module |69| mzscheme
  (require srfi/69/hash)
  (provide (all-from-except srfi/69/hash
                            s:make-hash-table
                            s:hash-table?
                            s:hash-table-copy)
           (rename s:make-hash-table make-hash-table)
           (rename s:hash-table? hash-table?)
           (rename s:hash-table-copy hash-table-copy)))
