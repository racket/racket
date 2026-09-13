#lang racket/base
(require racket/serialize
         racket/treelist
         racket/mutable-treelist)

(and
 (treelist? (deserialize (serialize (treelist 1 2 3))))
 (mutable-treelist? (deserialize (serialize (mutable-treelist 1 2 3))))
 'ok-41)
