
(module parcheesi mzscheme
  (require mzlib/unit
           mzlib/class
           "admin-gui.ss")
  
  (provide game@)
  (define game@
    (unit (import)
          (export)
          (new gui-game%))))


