(module |63| mzscheme
  (require (lib "63.ss" "srfi" "63"))
  (provide (all-from-except (lib "63.ss" "srfi" "63") s:equal?)
           (rename s:equal? equal?)))