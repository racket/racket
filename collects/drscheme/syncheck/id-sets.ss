#lang typed-scheme

(provide (rename-out [make-module-identifier-mapping make-id-set]
		     [module-identifier-mapping? id-set?])
         add-id get-idss get-ids for-each-ids
         Id-Set)

(require/opaque-type Id-Set module-identifier-mapping? syntax/boundmap)

;; FIXME - need polymorphic imports
(require/typed [module-identifier-mapping-get module-identifier-mapping-get/f]
  (Id-Set Identifier (-> #f) -> (U (Listof Identifier) #f))
  syntax/boundmap)

(require/typed syntax/boundmap
  [make-module-identifier-mapping (-> Id-Set)]
  [module-identifier-mapping-get 
     (Id-Set Identifier (-> '()) -> (Listof Identifier))]
  [module-identifier-mapping-put! (Id-Set Identifier (Listof Identifier) -> Void)]
  [module-identifier-mapping-for-each (Id-Set (Identifier (Listof Identifier) -> Void) -> Void)]
  [module-identifier-mapping-map 
   (Id-Set (Identifier (Listof Identifier) -> (Listof Identifier)) -> (Listof (Listof Identifier)))])                    


(: add-id (Id-Set Identifier -> Void))
(define (add-id mapping id)
  (let* ([old (module-identifier-mapping-get mapping id (λ () '()))]
         [new (cons id old)])
    (module-identifier-mapping-put! mapping id new)))

(: get-idss (Id-Set -> (Listof (Listof Identifier))))
(define (get-idss mapping)
  (module-identifier-mapping-map mapping (λ: ([x : Identifier] [y : (Listof Identifier)]) y)))

(: get-ids (Id-Set Identifier -> (U (Listof Identifier) #f)))
(define (get-ids mapping var)
  (module-identifier-mapping-get/f mapping var (λ () #f)))

(: for-each-ids (Id-Set ((Listof Identifier) -> Void) -> Void))
(define (for-each-ids mapping f)
  (module-identifier-mapping-for-each mapping (λ: ([x : Identifier] [y : (Listof Identifier)]) (f y))))
