
(module init scheme/base

  (read-case-sensitive #f)
  (read-curly-brace-as-paren #f)
  (read-accept-infix-dot #f)
  (read-curly-brace-as-paren #f)
  (read-square-bracket-as-paren #f)

  (print-vector-length #f)
  (print-pair-curly-braces #t)
  (print-mpair-curly-braces #f)
  
  (define-syntax out
    (syntax-rules ()
      [(_) (begin
             (require "main.ss")
             (provide (all-from-out "main.ss")))]))
  (out))

