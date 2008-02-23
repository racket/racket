;; module loader for SRFI-71
(module |71| mzscheme
  (require srfi/71/letvalues)
  (provide (all-from-except srfi/71/letvalues
                            let
                            let*
                            letrec
                            srfi-let
                            srfi-let*
                            srfi-letrec))
  (provide (rename srfi-let    let)
           (rename srfi-let*   let*)
           (rename srfi-letrec letrec)
           (rename srfi-letrec letrec*)))
  