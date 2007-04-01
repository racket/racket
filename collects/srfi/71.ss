;; module loader for SRFI-71
(module |71| mzscheme
  (require (lib "letvalues.ss" "srfi" "71"))
  (provide (all-from-except (lib "letvalues.ss" "srfi" "71")
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
  