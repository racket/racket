(module turtle-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide turtle^)

  (define-signature turtle^ 
    (turtles
     clear
     turn turn/radians
     move move-offset
     draw draw-offset
     erase erase-offset
     
     save-turtle-bitmap
     
     splitfn split*fn tpromptfn
     turtle-window-size
     
     display-lines-in-drawing)))
