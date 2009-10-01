#lang scheme/base
(require "private/image-core.ss"
         "private/image-more.ss")

(provide overlay
         overlay/places
         overlay/xy
         
         beside
         beside/places

         ;above
         ;above/places
         
         rotate
         frame
         
         ellipse
         rectangle
         
         x-place?
         y-place?
         image?
         mode?
         angle?)