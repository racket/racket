#lang racket/base
(require "snip/private/snip.rkt"
         "snip/private/snip-admin.rkt"
         "snip/private/style.rkt")
(provide mult-color<%>
         add-color<%>
         style-delta%
         style<%>
         style-list%
         the-style-list
         (all-from-out "snip/private/snip.rkt"
                       "snip/private/snip-admin.rkt"))
