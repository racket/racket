(module browser-unit mzscheme
  (require mzlib/unit
           mred/mred-sig
           setup/plt-installer-sig
           net/tcp-sig
           "browser-sig.rkt"
           "private/sig.rkt"
           "private/bullet.rkt"
           "private/html.rkt"
           "private/hyper.rkt")
  
  (provide browser@)
  
  (define-unit-from-context bullet@ bullet-export^)
  
  (define-compound-unit/infer pre-browser@
    (import setup:plt-installer^
            mred^)
    (export hyper^ html-export^ bullet-export^)
    (link html@ hyper@ bullet@))

  (define-unit/new-import-export browser@
    (import setup:plt-installer^
            mred^)
    (export browser^)
    ((hyper^ html-export^ bullet-export^) 
     pre-browser@ 
     setup:plt-installer^
     mred^)))


  
  
