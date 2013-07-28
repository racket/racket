#lang scheme/load

(module before typed/scheme

  (provide (all-defined-out))

  (define-struct: Sigil ())

  (: list->english ((Listof String) -> String))
  (define (list->english strs) (error 'fail))

  (define-type-alias (Set X) (HashTable X '()))

  (: empty-set (All (T) (-> (Set T))))
  (define (empty-set) (error 'fail))

  (: set->list (All (T) ((Set T) -> (Listof T))))
  (define (set->list set) (error 'fail))
  )

(module after typed/scheme
  (require 'before)

  (: f ((Set Sigil) -> Any))
  (define (f x1)
    (let* ([x2 (set->list x1)])
      (list->english x2)
      (error 'NO! "Way!"))))

