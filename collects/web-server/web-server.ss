(module web-server mzscheme
  (require (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "tcp-sig.ss" "net")
           (prefix raw: (lib "tcp-unit.ss" "net"))
           (lib "unit.ss")
           (lib "contract.ss")
           "private/dispatch-server-sig.ss"
           "private/dispatch-server-unit.ss"
           "web-config-sig.ss"
           "web-server-sig.ss"
           "web-server-unit.ss"
           (prefix http: "private/request.ss"))
  (provide
   serve
   serve/ports
   serve/ips+ports)
  (provide/contract
   [do-not-return (-> void)]
   [serve/web-config@ (unit? . -> . (-> void?))])
  
  (define (do-not-return)
    (semaphore-wait (make-semaphore 0)))
  
  (define/kw (serve
              #:key
              dispatch
              [tcp@ raw:tcp@]
              [port 80]
              [listen-ip #f]
              [max-waiting 40]
              [initial-connection-timeout 60]
              [read-request http:read-request])
    (define-unit-binding a-tcp@
      tcp@ (import) (export tcp^))
    (define-compound-unit/infer dispatch-server@/tcp@
      (import dispatch-server-config^)
      (link a-tcp@ dispatch-server@)
      (export dispatch-server^))
    (define-values/invoke-unit
      dispatch-server@/tcp@
      (import dispatch-server-config^)
      (export dispatch-server^))
    
    (serve))
  
  (define/kw (serve/ports
              #:key
              dispatch
              [tcp@ raw:tcp@]
              [ports (list 80)]
              [listen-ip #f]
              [max-waiting 40]
              [initial-connection-timeout 60]
              [read-request http:read-request])
    (define shutdowns
      (map (lambda (port)
             (serve #:dispatch dispatch
                    #:tcp@ tcp@
                    #:port port
                    #:listen-ip listen-ip
                    #:max-waiting max-waiting
                    #:initial-connection-timeout initial-connection-timeout
                    #:read-request read-request))
           ports))
    (lambda ()
      (for-each apply shutdowns)))
  
  (define/kw (serve/ips+ports
              #:key
              dispatch
              [tcp@ raw:tcp@]
              [ips+ports (list (cons #f (list 80)))]
              [max-waiting 40]
              [initial-connection-timeout 60]
              [read-request http:read-request])
    (define shutdowns
      (map (match-lambda
             [(list-rest listen-ip ports)
              (serve/ports #:dispatch dispatch
                           #:tcp@ tcp@
                           #:ports ports
                           #:listen-ip listen-ip
                           #:max-waiting max-waiting
                           #:initial-connection-timeout initial-connection-timeout
                           #:read-request read-request)])
           ips+ports))
    (lambda ()
      (for-each apply shutdowns)))
  
  ; serve/config@ : configuration -> (-> void)
  (define (serve/web-config@ config@)
    (define-unit m@ (import web-server^) (export)
      (init-depend web-server^)
      (serve))
    (define-unit-binding c@ config@ (import) (export web-config^))
    (invoke-unit
     (compound-unit/infer
      (import)
      (link raw:tcp@ c@ web-server@ m@)
      (export)))))