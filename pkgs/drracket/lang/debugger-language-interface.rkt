
(module debugger-language-interface mzscheme
  (require mzlib/class)
  (provide debugger-language<%>)

  (define debugger-language<%>
    (interface () debugger:supported?)))
