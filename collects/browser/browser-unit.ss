(module browser-unit mzscheme
  (require (lib "unit.ss")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "tcp-sig.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url-unit.ss" "net")
           "browser-sig.ss"
           "private/sig.ss"
           "private/bullet.ss"
           "private/html.ss"
           "private/hyper.ss")
  
  (provide browser@)
  
  (define-unit-from-context bullet@ bullet-export^)
  
  (define-compound-unit/infer pre-browser@
    (import setup:plt-installer^
            mred^
            url^)
    (export hyper^ html-export^ bullet-export^)
    (link html@ hyper@ bullet@))

  (define-unit/new-import-export browser@
    (import setup:plt-installer^
            mred^
            url^)
    (export browser^)
    ((hyper^ html-export^ bullet-export^) 
     pre-browser@ 
     setup:plt-installer^
     mred^
     url^)))


  
  
