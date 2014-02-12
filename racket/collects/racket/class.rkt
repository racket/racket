#lang racket/base

(require "contract/private/object.rkt")
(provide (all-from-out "contract/private/object.rkt"))
  
;; All of the implementation is actually in private/class-internal.rkt,
;;  which provides extra (private) functionality to contract.rkt.
(require "private/class-internal.rkt"
         "private/class-c-old.rkt"
         "private/class-c-new.rkt")
  
(provide-public-names)
(provide generic?)
