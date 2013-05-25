#lang racket

(struct result (phase) #:prefab)
(struct failure result (serious? msg) #:prefab)
(struct success result () #:prefab)

(provide/contract
 [struct result ([phase symbol?])]
 [struct failure ([phase symbol?]
                  [serious? boolean?]
                  [msg string?])]
 [struct success ([phase symbol?])])
