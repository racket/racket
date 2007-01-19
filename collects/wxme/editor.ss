
(module editor mzscheme
  (require (lib "class.ss")
           "private/class-help.ss")

  (provide editor%)

  (define editor%
    (class object%
      (init-accessible content-port)
      (super-new))))
