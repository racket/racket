
(module image mzscheme
  (require mzlib/class
           "private/class-help.ss")

  (provide image%)

  (define image%
    (class object%
      (init-accessible filename data w h dx dy)
      (super-new))))
