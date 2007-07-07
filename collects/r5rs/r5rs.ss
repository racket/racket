(module r5rs mzscheme
  (define-syntax provide-r5rs
    (syntax-rules ()
      [(_) (begin (require (lib "lang.ss" "r5rs"))
                  (provide (all-from (lib "lang.ss" "r5rs"))))]))
  (provide-r5rs)
  (read-case-sensitive #f)
  (read-square-bracket-as-paren #f)
  (read-curly-brace-as-paren #f)
  (read-accept-infix-dot #f)
  (current-namespace (scheme-report-environment 5)))
