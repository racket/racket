#lang racket
(require racket/unit)

(provide relative-btree^
         bullet-export^
         hyper^
         html-export^
         html^)

(define-signature html-export^
  (html-img-ok
   html-eval-ok
   image-map-snip%))

(define-signature html^ extends html-export^
  (html-convert
   html-status-handler))

(define-signature bullet-export^
  (bullet-size))

(define-signature hyper^
  (open-url
   (struct exn:file-saved-instead (pathname) #:omit-constructor)
   (struct exn:cancelled () #:omit-constructor)
   
   hyper-text<%>
   hyper-text-mixin
   hyper-text%
   
   hyper-canvas-mixin
   hyper-canvas%
   
   hyper-panel<%>
   hyper-panel-mixin
   hyper-panel%
   
   hyper-frame<%>
   hyper-frame-mixin
   hyper-frame%
   
   hyper-no-show-frame-mixin
   hyper-no-show-frame%
   
   editor->page
   page->editor))

(define-signature relative-btree^
  (make-btree
   
   btree-get
   btree-put!
   
   btree-shift!
   
   btree-for-each
   btree-map))
