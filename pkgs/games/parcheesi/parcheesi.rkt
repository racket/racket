(module parcheesi racket
  (require racket/unit
           racket/class
           "admin-gui.rkt")

  (provide game@)
  (define game@
    (unit (import)
          (export)
          (new gui-game%))))
