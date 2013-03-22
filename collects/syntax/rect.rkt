#lang racket/base
(provide (struct-out exn:fail:syntax/rects)
         (struct-out exn:fail:read/rects)
         (struct-out exn:fail:read:eof/rects)
         (struct-out exn:fail:read:non-char/rects)
         (struct-out srcloc-rect)
         prop:exn:srcloc-rects 
         exn:srcloc-rects? 
         exn:srcloc-rects-accessor)

(define-values (prop:exn:srcloc-rects exn:srcloc-rects? exn:srcloc-rects-accessor)
  (make-struct-type-property 'exn:srcloc-rects))

(struct exn:fail:syntax/rects exn:fail:syntax (rects)
  #:property prop:exn:srcloc-rects (λ (x) (exn:fail:syntax/rects-rects x)))

(struct exn:fail:read/rects exn:fail:read (rects)
  #:property prop:exn:srcloc-rects (λ (x) (exn:fail:read/rects-rects x)))
(struct exn:fail:read:eof/rects exn:fail:read:eof (rects)
  #:property prop:exn:srcloc-rects (λ (x) (exn:fail:read:eof/rects-rects x)))
(struct exn:fail:read:non-char/rects exn:fail:read:non-char (rects)
  #:property prop:exn:srcloc-rects (λ (x) (exn:fail:read:non-char/rects-rects x)))

(struct srcloc-rect (source pos width height) #:transparent)
