#lang racket/base
(require racket/contract/base
         "private/modcode-noctc.rkt")

(provide (except-out (all-from-out "private/modcode-noctc.rkt")
                     get-module-code
                     get-module-path
                     get-metadata-path))

(provide/contract
 [get-module-code
  (->* (path-string?)
       (#:roots (listof (or/c path-string? 'same))
        #:submodule-path (listof symbol?)
        #:sub-path (and/c path-string? relative-path?)
        (and/c path-string? relative-path?)
        #:compile (-> any/c any)
        (-> any/c any)
        #:extension-handler (or/c false/c (path? boolean? . -> . any))
        (or/c false/c (path? boolean? . -> . any))
        #:choose (path? path? path? . -> . (or/c 'src 'zo 'so #f))
        #:notify  (any/c . -> . any)
        #:source-reader (any/c input-port? . -> . (or/c syntax? eof-object?))
        #:rkt-try-ss? boolean?)
       any)]
 [get-module-path
  (->* (path-string?)
       (#:roots (listof (or/c path-string? 'same))
        #:submodule? boolean?
        #:sub-path (and/c path-string? relative-path?)
        (and/c path-string? relative-path?)
        #:choose (path? path? path? . -> . (or/c 'src 'zo 'so #f))
        #:rkt-try-ss? boolean?)
       (values path? (or/c 'src 'zo 'so)))]
 [get-metadata-path
  (->* (path-string?)
       (#:roots (listof (or/c path-string? 'same)))
       #:rest (listof (or/c path-string? 'same))
       path?)])
