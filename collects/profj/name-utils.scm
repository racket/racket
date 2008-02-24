(module name-utils mzscheme
  
  (provide (all-defined-except getter))
  
  (define (getter match-pattern replace-pattern)
    (lambda (name)
      (cond
        [(symbol? name) name]
        [(regexp-match match-pattern name) (regexp-replace replace-pattern name "")]
        [else name])))
  
  (define get-leading-name (getter "\\." "\\.(.)*"))
  (define get-last-name (getter "\\." "^(.)*\\."))
  
  )
