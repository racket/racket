(module editor mzscheme
  (require mzlib/class
           "private/class-help.rkt")

  (provide editor%)

  (define editor%
    (class object%
      (init-accessible content-port)
      (super-new))))
