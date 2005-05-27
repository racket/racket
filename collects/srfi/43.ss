;; module loader for SRFI-43
(module |43| mzscheme
  (require (lib "vector-lib.ss" "srfi" "43"))
  (provide (all-from-except (lib "vector-lib.ss" "srfi" "43")
                            s:vector-fill!
                            s:vector->list)
           (rename s:vector-fill! vector-fill!)
           (rename s:vector->list vector->list)))
