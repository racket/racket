#lang racket/base
(require "../common/contract.rkt"
         (rename-in "../host/linklet.rkt"
                    [linklet? raw:linklet?]
                    [recompile-linklet raw:recompile-linklet]
                    [eval-linklet raw:eval-linklet]
                    [instantiate-linklet raw:instantiate-linklet]
                    [linklet-import-variables raw:linklet-import-variables]
                    [linklet-export-variables raw:linklet-export-variables])
         (rename-in "linklet.rkt"
                    [linklet-directory? raw:linklet-directory?]
                    [linklet-directory->hash raw:linklet-directory->hash]
                    [linklet-bundle? raw:linklet-bundle?]
                    [linklet-bundle->hash raw:linklet-bundle->hash])
         "compiled-in-memory.rkt"
         "correlated-linklet.rkt"
         "built-in-symbol.rkt")

(provide linklet-directory?
         linklet-directory->hash
         linklet-bundle?
         linklet-bundle->hash
         (except-out (all-from-out "linklet.rkt")
                     raw:linklet-directory?
                     raw:linklet-directory->hash
                     raw:linklet-bundle?
                     raw:linklet-bundle->hash)
         linklet?
         recompile-linklet
         decompile-linklet
         eval-linklet
         instantiate-linklet
         linklet-import-variables
         linklet-export-variables
         linklet-body-reserved-symbol?)

(define (linklet-directory? v)
  (or (raw:linklet-directory? v)
      (and (compiled-in-memory? v)
           (raw:linklet-directory?
            (compiled-in-memory-linklet-directory v)))))

(define (linklet-bundle? v)
  (or (raw:linklet-bundle? v)
      (and (compiled-in-memory? v)
           (raw:linklet-bundle?
            (compiled-in-memory-linklet-directory v)))))

(define (linklet-directory->hash ld)
  (raw:linklet-directory->hash (if (compiled-in-memory? ld)
                                   (compiled-in-memory-linklet-directory ld)
                                   ld)))

(define (linklet-bundle->hash lb)
  (raw:linklet-bundle->hash (if (compiled-in-memory? lb)
                                (compiled-in-memory-linklet-directory lb)
                                lb)))

(define (linklet? l)
  (or (raw:linklet? l)
      (correlated-linklet? l)))

(define recompile-linklet
  (case-lambda
    [(lnk) (raw:recompile-linklet (force-compile-linklet lnk))]
    [(lnk info) (raw:recompile-linklet (force-compile-linklet lnk) info)]
    [(lnk info import-keys) (raw:recompile-linklet (force-compile-linklet lnk) info import-keys)]
    [(lnk info import-keys get-import) (raw:recompile-linklet (force-compile-linklet lnk) info import-keys get-import)]
    [(lnk info import-keys get-import options) (raw:recompile-linklet (force-compile-linklet lnk) info import-keys get-import options)]))

(define (eval-linklet lnk)
  (raw:eval-linklet (force-compile-linklet lnk)))

(define (instantiate-linklet lnk import-instances [target-instance #f] [use-prompt? #f])
  (raw:instantiate-linklet (force-compile-linklet lnk) import-instances target-instance use-prompt?))

(define (linklet-import-variables lnk)
  (raw:linklet-import-variables (force-compile-linklet lnk)))

(define (linklet-export-variables lnk)
  (raw:linklet-export-variables (force-compile-linklet lnk)))

(define/who (decompile-linklet lnk)
  (check who linklet? lnk)
  (and (correlated-linklet? lnk)
       (correlated-linklet-expr lnk)))

(define/who (linklet-body-reserved-symbol? s)
  (check who symbol? s)
  (built-in-symbol? s))
