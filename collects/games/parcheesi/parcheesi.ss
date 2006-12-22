
(module parcheesi mzscheme
  (require (lib "unit.ss")
           (lib "class.ss")
           "admin-gui.ss")
  
  (provide game@)
  (define game@
    (unit (import)
          (export)
          (new gui-game%))))


