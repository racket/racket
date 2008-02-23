;; module loader for SRFI-45
(module |45| mzscheme
  (require srfi/45/lazy)
  (provide (all-from-except srfi/45/lazy
                            s:delay
                            s:force
                            srfi-45-promise?)
           (rename s:delay delay)
           (rename s:force force)
           (rename srfi-45-promise? promise?)))
