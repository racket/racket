
(module parcheesi mzscheme
  (require (lib "unit.ss")
           (lib "class.ss"))
  
  (provide game-unit)
  (define game-unit
    (unit (import)
          (export)
          (new (dynamic-require '(lib "admin-gui.ss" "games" "parcheesi") 'gui-game%)))))

