(module web-server mzscheme
  (require (lib "tcp-sig.ss" "net")
           (lib "unit.ss")
           (lib "contract.ss")
           "web-config-sig.ss"
           "web-server-sig.ss"
           "web-server-unit.ss"
	   "configuration.ss"
           "private/configuration-structures.ss")
  (provide/contract
   [serve/web-config@ (configuration? . -> . (-> void?))])
  
  ; serve/config@ : configuration -> (-> void)
  (define (serve/web-config@ config@)
    (define-unit-from-context tcp@ tcp^)    
    (define-unit m@ (import web-server^) (export)
      (init-depend web-server^)
      (serve))
    (define-unit-binding c@ config@ (import) (export web-config^))
    (invoke-unit
     (compound-unit/infer
      (import)
      (link tcp@ c@ web-server@ m@)
      (export)))))