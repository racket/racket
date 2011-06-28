#lang racket/base
(require racket/match
         racket/list
         racket/serialize
         racket/contract
         racket/local
         unstable/bytes
         (planet jaymccarthy/mongodb))

(struct db (m d heap-mc tree-mc))

(define (ensure-mongo-collection d c #:init? init?)
  (if init?
      (local [(define mc (mongo-db-create-collection! d c #:capped? #f #:size 10000))]
        (mongo-collection-index! mc (hasheq 'key 1) #:name "key")
        mc)
      (mongo-collection d c)))

(define (db-connect spec #:init? [init? #f])
  (match-define (regexp #rx"^([a-zA-Z]+):([0-9]+):([a-zA-Z]+)$"
                        (list _ host (app string->number port) db-name))
                spec)
  (define m (create-mongo #:host host #:port port))
  (define d (mongo-db m db-name))
  (define h (ensure-mongo-collection d "heap" #:init? init?))
  (define t (ensure-mongo-collection d "tree" #:init? init?))
  (db m d h t))

(define (db-ref the-db . path)
  (define e
    (seqn-first
     (mongo-collection-find 
      (db-heap-mc the-db)
      (hasheq 'key path)
      #:selector (hasheq 'value 1)
      #:limit 1)))
  (read/bytes (hash-ref e 'value)))

(define (db-set! the-db value . path)
  (mongo-collection-repsert!
   (db-heap-mc the-db)
   (hasheq 'key path)
   (hasheq 'key path
           'value (write/bytes value)))
  (define-values (dir entry-l) (split-at path (sub1 (length path))))
  (define entry (first entry-l))
  (mongo-collection-repsert!
   (db-tree-mc the-db)
   (hasheq 'key dir)
   (hasheq '$addToSet (hasheq 'entries entry))))

(define (db-list the-db . path)
  (vector->list
   (hash-ref
    (seqn-first
     (mongo-collection-find 
      (db-tree-mc the-db)
      (hasheq 'key path)
      #:selector (hasheq 'entries 1)
      #:limit 1))
    'entries)))

(define (db-close! db)
  (close-mongo! (db-m db)))

(provide/contract
 [db? (any/c . -> . boolean?)]
 [db-connect ((string?) (#:init? boolean?) . ->* . db?)]
 [db-ref ((db?) () #:rest (non-empty-listof string?) . ->* . serializable?)] 
 [db-list ((db?) () #:rest (listof string?) . ->* . (listof string?))] 
 [db-set! ((db? serializable?) () #:rest (non-empty-listof string?) . ->* . void)]
 [db-close! (db? . -> . void)])
