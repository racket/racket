;; module loader for SRFI-43
(module |43| mzscheme
  (require srfi/43/vector-lib)
  (provide (all-from-except srfi/43/vector-lib
                            s:vector-fill!
                            s:vector->list)
           (rename s:vector-fill! vector-fill!)
           (rename s:vector->list vector->list)))
