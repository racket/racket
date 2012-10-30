#lang racket/base
(require racket/match
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         racket/unit
         racket/async-channel
         racket/contract
         unstable/contract
         web-server/dispatchers/dispatch
         web-server/private/dispatch-server-sig
         web-server/private/dispatch-server-unit
         web-server/web-config-sig
         web-server/web-server-sig
         web-server/web-server-unit
         (prefix-in http: web-server/http/request))
(provide/contract
 [serve
  (->* (#:dispatch dispatcher/c)
       (#:confirmation-channel (or/c false/c async-channel?)
                               #:connection-close? boolean?
                               #:tcp@ (unit/c (import) (export tcp^))
                               #:port tcp-listen-port?
                               #:listen-ip (or/c false/c string?)
                               #:max-waiting number?
                               #:initial-connection-timeout number?)
       (-> void))]
 [serve/ports
  (->* (#:dispatch dispatcher/c)
       (#:confirmation-channel (or/c false/c async-channel?)
                               #:connection-close? boolean?
                               #:tcp@ (unit/c (import) (export tcp^))
                               #:ports (listof tcp-listen-port?)
                               #:listen-ip (or/c false/c string?)
                               #:max-waiting number?
                               #:initial-connection-timeout number?)
       (-> void))]
 [serve/ips+ports
  (->* (#:dispatch dispatcher/c)
       (#:confirmation-channel (or/c false/c async-channel?)
                               #:connection-close? boolean?
                               #:tcp@ (unit/c (import) (export tcp^))
                               #:ips+ports (listof (cons/c (or/c false/c string?) (listof tcp-listen-port?)))
                               #:max-waiting number?
                               #:initial-connection-timeout number?)
       (-> void))]
 [do-not-return (-> void)]
 [serve/web-config@ (((unit/c (import) (export web-config^))) (#:tcp@ (unit/c (import) (export tcp^))) . ->* . (-> void?))])

(define (do-not-return)
  (semaphore-wait (make-semaphore 0)))

(define (serve
         #:dispatch dispatch
         #:confirmation-channel [confirmation-channel #f]
         #:connection-close? [connection-close? #f]
         #:tcp@ [tcp@ raw:tcp@]
         #:port [port 80]
         #:listen-ip [listen-ip #f]
         #:max-waiting [max-waiting 40]
         #:initial-connection-timeout [initial-connection-timeout 60])
  (define read-request 
    (http:make-read-request
     #:connection-close? connection-close?))
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
  
  (serve #:confirmation-channel confirmation-channel))

(define (serve/ports
         #:dispatch dispatch
         #:confirmation-channel [confirmation-channel #f]
         #:connection-close? [connection-close? #f]
         #:tcp@ [tcp@ raw:tcp@]
         #:ports [ports (list 80)]
         #:listen-ip [listen-ip #f]
         #:max-waiting [max-waiting 40]
         #:initial-connection-timeout [initial-connection-timeout 60])
  (define shutdowns
    (map (lambda (port)
           (serve 
            #:dispatch dispatch
            #:confirmation-channel confirmation-channel
            #:connection-close? connection-close?
            #:tcp@ tcp@
            #:port port
            #:listen-ip listen-ip
            #:max-waiting max-waiting
            #:initial-connection-timeout initial-connection-timeout))
         ports))
  (lambda ()
    (for-each apply shutdowns)))

(define (serve/ips+ports
         #:dispatch dispatch
         #:confirmation-channel [confirmation-channel #f]
         #:connection-close? [connection-close? #f]
         #:tcp@ [tcp@ raw:tcp@]
         #:ips+ports [ips+ports (list (cons #f (list 80)))]
         #:max-waiting [max-waiting 40]
         #:initial-connection-timeout [initial-connection-timeout 60])
  (define shutdowns
    (map (match-lambda
           [(list-rest listen-ip ports)
            (serve/ports
             #:dispatch dispatch
             #:confirmation-channel confirmation-channel
             #:connection-close? connection-close?
             #:tcp@ tcp@
             #:ports ports
             #:listen-ip listen-ip
             #:max-waiting max-waiting
             #:initial-connection-timeout initial-connection-timeout)])
         ips+ports))
  (lambda ()
    (for-each apply shutdowns)))

; serve/config@ : configuration -> (-> void)
(define (serve/web-config@ config@ #:tcp@ [tcp@ raw:tcp@])
  (define-unit-binding a-tcp@
    tcp@ (import) (export tcp^))
  (define-unit m@ (import web-server^) (export)
    (init-depend web-server^)
    (serve))
  (define-unit-binding c@ config@ (import) (export web-config^))
  (invoke-unit
   (compound-unit/infer
    (import)
    (link a-tcp@ c@ web-server@ m@)
    (export))))
