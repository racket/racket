#lang racket/base
(require racket/contract/base
         "snip/private/contract.rkt"
         "snip/private/snip.rkt"
	 "snip/private/snip-admin.rkt"
	 "snip/private/style.rkt")

(provide mult-color<%>
         add-color<%>
         style<%>
         the-style-list

         (except-out (all-from-out "snip/private/snip.rkt")
                     snip%
                     snip-class%
                     string-snip%
                     tab-snip%
                     image-snip%))

(provide/contract [style-delta%  style-delta%/c]
                  [style-list%   style-list%/c]
                  [snip%         snip%/c]
                  [snip-class%   snip-class%/c]
                  [string-snip%  string-snip%/c]
                  [tab-snip%     tab-snip%/c]
                  [image-snip%   image-snip%/c]
                  [snip-admin%   snip-admin%/c])
