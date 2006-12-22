(module browser-sig mzscheme
  (require (lib "unit.ss")
           "private/sig.ss")

  (provide browser^)
  
  (define-signature browser^
    ((open hyper^)
     (open html-export^)
     (open bullet-export^))))

