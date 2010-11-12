#lang racket/base
(require syntax/modresolve)
(provide module-db)

;; A ModuleDB = hash[path/symbol => (U 'no-drop 'no-bypass)]
;;   'no-drop = must not be dropped or bypassed because of, eg, side effects
;;   'no-bypass = don't bypass in favor of private component modules
;;                but if the module is unused, can drop it
;;                (FIXME: replace with component module calculation and checking)

(define (make-module-db mod+config-list)
  (for/hash ([mod+config (in-list mod+config-list)])
    (values (resolve-module-path (car mod+config) #f) (cadr mod+config))))

;; module-db : ModuleDB
(define module-db
  (make-module-db
   '([racket/base                            no-bypass]
     [racket/contract/base                   no-bypass]
     [racket/gui                             no-bypass]
     [racket/match                           no-bypass]
     ['#%builtin                             no-drop]

     [typed-scheme/private/base-env          no-drop]
     [typed-scheme/private/base-special-env  no-drop]
     [typed-scheme/private/base-env-numeric  no-drop]
     [typed-scheme/private/base-env-indexing no-drop])))
