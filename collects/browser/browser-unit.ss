(module browser-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "tcp-sig.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url-unit.ss" "net")
           "browser-sig.ss"
           "private/bullet.ss"
           "private/html.ss"
           "private/hyper.ss"
           "private/sig.ss")
  
  (provide browser@)
  
  (define pre-browser@
    (compound-unit/sig
      (import (plt-installer : setup:plt-installer^)
              (mred : mred^)
              (tcp : net:tcp^)
              (url : net:url^))
      (link [html : html^ (html@ mred url)]
            [hyper : hyper^ (hyper@ html mred plt-installer url)]
	    [bullet-size : bullet-export^ ((unit/sig bullet-export^
					     (import)
					     (rename (html:bullet-size bullet-size))
					     (define html:bullet-size bullet-size)))])
      (export (open hyper)
	      (open bullet-size)
              (open (html : html-export^)))))
  
  
  ;; this extra layer of wrapper here is only to
  ;; ensure that the browser^ signature actually matches
  ;; the export of the pre-browser@ unit.
  ;; (it didn't before, so now we check.)
  (define browser@
    (compound-unit/sig
      (import  (plt-installer : setup:plt-installer^)
               (mred : mred^)
               (tcp : net:tcp^)
               (url : net:url^))
      (link [pre-browser : browser^ (pre-browser@ plt-installer mred tcp url)])
      (export (open pre-browser)))))
