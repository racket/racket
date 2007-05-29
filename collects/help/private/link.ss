(module link mzscheme 
  (require (lib "web-server-unit.ss" "web-server")
           (lib "web-server-sig.ss" "web-server")
           (lib "web-config-sig.ss" "web-server")
           
           (lib "unit.ss")
           
           (lib "tcp-sig.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url-unit.ss" "net")
           
           (lib "browser-sig.ss" "browser")
           (lib "browser-unit.ss" "browser")
           
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")   

           (lib "mred-unit.ss" "mred")
           (lib "mred-sig.ss" "mred")
           
           "tcp-intercept.ss"
           "sig.ss"
           
           "gui.ss"
           "main.ss"
           "config.ss")
  
  (define-unit-from-context inst@ setup:plt-installer^)
  (define-unit-from-context real-tcp@ tcp^)
  (define-unit-binding config@ config (import) (export web-config^))
  
  (define-compound-unit/infer help-desk@
    (import)
    (export gui^ main^ web-server^)
    (link inst@
          standard-mred@
          (((real-tcp : tcp^)) real-tcp@)
          config@
          (((real-url : url^)) url@ real-tcp)
          (() web-server@ real-tcp)
          (((ic-tcp : tcp^)) tcp-intercept@)
          (((pre-ic-url : url^)) url@ ic-tcp)
          (((ic-url : url^)) url-intercept@ pre-ic-url)
          (() browser@ ic-tcp ic-url)
          (() gui@ ic-url)
          main@))

  (define-values/invoke-unit/infer help-desk@)

  (provide-signature-elements gui^ main^ web-server^))
