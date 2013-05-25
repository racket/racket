#lang scheme/base
(require "core.rkt"
         "private/provide-structs.rkt"
         racket/contract/base)

(provide-structs
 [module-path-index-desc ()]
 [exported-index-desc ([name symbol?]
                       [from-libs (listof module-path?)])]
 [(method-index-desc exported-index-desc) ([method-name symbol?]
                                           [class-tag tag?])]
 [(constructor-index-desc exported-index-desc) ([class-tag tag?])]
 [(procedure-index-desc exported-index-desc) ()]
 [(thing-index-desc exported-index-desc) ()]
 [(struct-index-desc exported-index-desc) ()]
 [(form-index-desc exported-index-desc) ()]
 [(class-index-desc exported-index-desc) ()]
 [(interface-index-desc exported-index-desc) ()]
 [(mixin-index-desc exported-index-desc) ()])
