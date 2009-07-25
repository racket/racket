#lang scheme/base

(require "core.ss"
         "private/provide-structs.ss"
         scheme/contract)

(provide-structs
 [module-path-index-desc ()]
 [exported-index-desc ([name symbol?]
                       [from-libs (listof module-path?)])]
 [(method-index-desc exported-index-desc) ([method-name symbol?]
                                           [class-tag tag?])]
 [(procedure-index-desc exported-index-desc) ()]
 [(thing-index-desc exported-index-desc) ()]
 [(struct-index-desc exported-index-desc) ()]
 [(form-index-desc exported-index-desc) ()]
 [(class-index-desc exported-index-desc) ()]
 [(interface-index-desc exported-index-desc) ()]
 [(mixin-index-desc exported-index-desc) ()])
