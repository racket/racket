#;
(exn-pred exn:fail:contract? ".*interface for bad-map.*")
#lang scheme/load

(module bad-map scheme
  (provide bad-map)
  (define (bad-map f l)
    (list (f 'quux))))

(module use-bad-map typed-scheme
  (require/typed 'bad-map
                 [bad-map (All (A B) ((A -> B) (Listof A) -> (Listof B)))])
  (bad-map add1 (list 12 13 14)))

(require 'use-bad-map)
