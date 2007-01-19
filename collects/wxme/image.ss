
(module image mzscheme
  (require (lib "class.ss")
           "private/class-help.ss")

  (provide image%)

  (define image%
    (class object%
      (init-accessible filename data w h dx dy)
      (super-new))))
