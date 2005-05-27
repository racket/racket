(module link mzscheme 
  (require (lib "web-server-unit.ss" "web-server")
           (lib "sig.ss" "web-server")
           
           (lib "unitsig.ss")
           
           (lib "tcp-sig.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url-unit.ss" "net")
           
           (lib "browser-sig.ss" "browser")
           (lib "browser-unit.ss" "browser")
           
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")   

           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
           
           "tcp-intercept.ss"
           "sig.ss"
           
           "gui.ss"
           "main.ss"
           "config.ss")
  
  (define help-desk@
    (compound-unit/sig
      (import [plt-installer : setup:plt-installer^]
              [mred : mred^]
              [real-tcp : net:tcp^])
      (link
       [config : web-config^ (config)]
       [real-url : net:url^ (url@ real-tcp)]
       [web-server : web-server^ (web-server@ real-tcp config)]
       
       [ic-tcp : net:tcp^ (tcp-intercept@ web-server)]
       [pre-ic-url : net:url^ (url@ ic-tcp)]
       [ic-url : net:url^ (url-intercept@ pre-ic-url)]
       [browser : browser^ (browser@ plt-installer mred ic-tcp ic-url)]
       [gui : gui^ (gui@ browser ic-url)]

       [m : () (main@)])
      (export (open gui)
              (open web-server))))
  
  (define-values/invoke-unit/sig ((open gui^) (open web-server^))
                                 help-desk@
                                 #f
                                 setup:plt-installer^
                                 mred^
                                 net:tcp^)
  
  (provide-signature-elements gui^)
  (provide-signature-elements web-server^))