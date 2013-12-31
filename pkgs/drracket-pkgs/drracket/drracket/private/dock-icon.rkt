#lang racket/base
(provide set-dock-tile-bitmap)
(require ffi/unsafe/objc
         ffi/unsafe/atomic
         mred/private/wx/cocoa/image
         mred/private/wx/cocoa/utils)

(module test racket/base)

(import-class NSApplication
              NSImageView)

(define (set-dock-tile-bitmap bm)
  (unless old-cocoa?
    (define dock-tile (tell (tell NSApplication sharedApplication) dockTile))
    (start-atomic)
    (define view (tell (tell NSImageView alloc) init))
    (tellv view setImage: (bitmap->image bm))
    
    (tellv dock-tile setContentView: view)
    (tellv dock-tile display)
    (tellv view release)
    (end-atomic)))
