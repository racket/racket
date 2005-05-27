(module browser-sig mzscheme
  (require (lib "unitsig.ss")
           "private/sig.ss")
  
  (provide browser^)

  (define-signature browser^
    ((open hyper^)
     (open html-export^)
     (open bullet-export^))))

