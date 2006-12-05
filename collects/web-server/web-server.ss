(module web-server mzscheme
  (require (lib "tcp-sig.ss" "net")
           (lib "unit.ss")
           (lib "contract.ss")
           "sig.ss"
           "web-server-unit.ss"
	   "configuration.ss"
           "private/configuration-structures.ss")
  (provide/contract
   [serve (case-> [configuration? . -> . (-> void?)]
                  [configuration? natural-number/c . -> . (-> void?)]
                  [configuration? natural-number/c string? . -> . (-> void?)])])
  
  ; : configuration [nat] [(U str #f)] -> -> void
  (define serve
    (case-lambda
      [(config)
       (run-the-server config)]
      [(config port)
       (run-the-server (update-configuration config `((port . ,port))))]
      [(config port listen-ip)
       (run-the-server (update-configuration config `((port . ,port) (ip-address . ,listen-ip))))]))
  
  (define-unit-from-context tcp@ tcp^)
  
  (define-unit m@ (import web-server^) (export)
    (init-depend web-server^)
    (serve))
  
  ; : configuration -> -> void
  (define (run-the-server config)
    (define-unit-binding c@ config (import) (export web-config^))
    (invoke-unit
     (compound-unit/infer
       (import)
       (link tcp@ c@ web-server@ m@)
       (export)))))