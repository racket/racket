#lang racket/gui

(require sgl/gl
         rackunit)

;; Test 1: Make sure creating multiple GL bitmaps doesn't crash

(for ([_  (in-range 5)])
  (define bm (make-gl-bitmap 32 32 (new gl-config%)))
  (send (send (make-object bitmap-dc% bm) get-gl-context)
        call-as-current
        (λ ()
          (glClearColor 0.0 0.0 0.0 0.0)
          (glClear GL_COLOR_BUFFER_BIT)
          (glBegin GL_TRIANGLES)
          (glVertex2f -1.0 -1.0)
          (glVertex2f +1.0 -1.0)
          (glVertex2f -1.0 +1.0)
          (glEnd)
          (glFinish)))
  (set! bm #f)
  (collect-garbage))

;; Test 2: make sure `get-current-gl-context` returns non-#f only within the dynamic extent of
;; `call-as-current` - in particular, that it returns #f on other threads

(define bm (make-gl-bitmap 32 32 (new gl-config%)))
(define ctxt (send (make-object bitmap-dc% bm) get-gl-context))

(define outside-context #f)
(define inside-context #f)

(define ch (make-channel))

(define th
  (thread
   (λ ()
     (channel-get ch)
     (set! outside-context (get-current-gl-context))
     (channel-put ch #t))))

(send ctxt call-as-current
      (λ ()
        (set! inside-context (get-current-gl-context))
        (channel-put ch #t)
        (channel-get ch)
        (void)))

(check-false (get-current-gl-context))
(check-false outside-context)
(check-eq? inside-context ctxt)
