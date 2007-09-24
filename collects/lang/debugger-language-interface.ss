
(module debugger-language-interface mzscheme
  (require (lib "class.ss"))
  (provide debugger-language<%>)

  (define debugger-language<%>
    (interface () debugger:supported?)))

  