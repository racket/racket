(module browser-sig mzscheme
  (require mzlib/unit
           "private/sig.ss")

  (provide browser^)
  
  (define-signature browser^
    ((open hyper^)
     (open html-export^)
     (open bullet-export^))))

