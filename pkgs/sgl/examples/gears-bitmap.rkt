#lang racket/base
(require racket/draw
         racket/gui/base
         racket/class
         "gears.rkt")

(define w 400)
(define h 400)

(define bm (make-gl-bitmap w h (new gl-config%)))
(define gears (new gears%
                   [with-gl-context
                    (lambda (thunk)
                      (send (send (send bm make-dc) get-gl-context)
                            call-as-current
                            thunk))]
                   [swap-gl-buffers void]
                   [refresh void]
                   [verbose? #f]))
(send gears set-size w h)
(void (send gears draw))

(define dest (build-path (find-system-path 'temp-dir) "gears.png"))
(when (send bm save-file dest 'png)
  (printf "wrote to ~a\n" dest))
