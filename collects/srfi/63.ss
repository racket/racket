(module |63| mzscheme
  (require srfi/63/63)
  (provide (all-from-except srfi/63/63 s:equal?)
           (rename s:equal? equal?)))
