;; module loader for SRFI-45
(module |45| mzscheme
  (require (lib "lazy.ss" "srfi" "45"))
  (provide (all-from-except (lib "lazy.ss" "srfi" "45")
                            s:delay
                            s:force
                            srfi-45-promise?)
           (rename s:delay delay)
           (rename s:force force)
           (rename srfi-45-promise? promise?)))
