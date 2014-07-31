#lang racket/base
;; $Id: gears.rkt,v 1.8 2005/01/12 12:49:10 mflatt Exp $
;;
;; This is a version of the venerable "gears" demo for PLT Scheme 200 using
;; Scott Owens' SGL OpenGL bindings.  It was ported from "glxgears.c" 1.3 from
;; XFree86, which had the following notices:
;;
;;     Copyright (C) 1999-2001  Brian Paul   All Rights Reserved.
;;
;;     Permission is hereby granted, free of charge, to any person obtaining a
;;     copy of this software and associated documentation files (the
;;     "Software"), to deal in the Software without restriction, including
;;     without limitation the rights to use, copy, modify, merge, publish,
;;     distribute, sublicense, and/or sell copies of the Software, and to
;;     permit persons to whom the Software is furnished to do so, subject to
;;     the following conditions:
;;
;;     The above copyright notice and this permission notice shall be included
;;     in all copies or substantial portions of the Software.
;;
;;     XFree86: xc/programs/glxgears/glxgears.c,v 1.3 2001/11/03 17:29:20 dawes
;;
;;     This is a port of the infamous "gears" demo to straight GLX (i.e. no
;;     GLUT).  Port by Brian Paul 23 March 2001.
;;
;; To run, evaluate this file in DrRacket in the "module" language level,
;; or execute "mred -qu gears.rkt" from your OS shell.
;;
;; Scheme port by Neil W. Van Dyke <neil@neilvandyke.org>, 23 November 2002.
;; Originally called glxgears.rkt.  Minor modifications since.
;; See "http://www.neilvandyke.org/opengl-plt/" for more information.
;;
;; Updated to newer sgl interface by Scott Owens

(require racket/draw
         racket/class
         racket/math
         sgl
         sgl/gl-vectors)

(provide gears%)

(define controls? #t)

(define gears%
  (class object%
    (init-field with-gl-context
                swap-gl-buffers
                refresh
                verbose?)

    (super-new)

    (define rotation 0.0)

    (define view-rotx 20.0)
    (define view-roty 30.0)
    (define view-rotz 0.0)

    (define gear1 #f)
    (define gear2 #f)
    (define gear3 #f)

    (define step? #f)

    (define/public (ready?)
      (and gear1 #t))

    (define/public (run)
      (set! step? #t)
      (refresh))

    (define/public (move-left)
      (set! view-roty (+ view-roty 5.0))
      (refresh))

    (define/public (move-right)
      (set! view-roty (- view-roty 5.0))
      (refresh))

    (define/public (move-up)
      (set! view-rotx (+ view-rotx 5.0))
      (refresh))

    (define/public (move-down)
      (set! view-rotx (- view-rotx 5.0))
      (refresh))

    (define (build-gear inner-radius    ; radius of hole at center
                        outer-radius    ; radius at center of teeth
                        width           ; width of gear
                        teeth           ; number of teeth
                        tooth-depth)    ; depth of tooth
      (let* ((r0             inner-radius)
             (r1             (- outer-radius (/ tooth-depth 2.0)))
             (r2             (+ outer-radius (/ tooth-depth 2.0)))
             (da             (/ (* 2.0 pi) teeth 4.0))
             (da2            (* da 2))
             (da3            (* da 3))
             (half-width     (* width 0.5))
             (neg-half-width (- half-width)))

        ;; TODO: Generalize away some more redundant program text.

        (gl-shade-model 'flat)

        (gl-normal 0.0 0.0 1.0)

	;; Draw front face.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) half-width)
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) half-width)
            (when (< i teeth)
              (gl-vertex (* r0 cos-angle)
                            (* r0 sin-angle)
                            (* half-width))
              (gl-vertex (* r1 (cos (+ angle da3)))
                            (* r1 (sin (+ angle da3)))
                            half-width))))
        (gl-end)

        ;; Draw front sides of teeth.
        (gl-begin 'quads)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (gl-vertex (* r1 (cos angle))
                       (* r1 (sin angle))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)))
        (gl-end)

        (gl-normal 0.0 0.0 -1.0)

        ;; Draw back face.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) neg-half-width)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (when (< i teeth)
              (gl-vertex (* r1 (cos (+ angle da3)))
                         (* r1 (sin (+ angle da3)))
                         neg-half-width)
              (gl-vertex (* r0 cos-angle)
                         (* r0 sin-angle)
                         neg-half-width))))
        (gl-end)

        ;; Draw back sides of teeth.
        (gl-begin 'quads)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (gl-vertex (* r1 (cos angle))
                       (* r1 (sin angle))
                       neg-half-width)))
        (gl-end)

        ;; Draw outward faces of teeth.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))

            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) half-width)
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) neg-half-width)

            (let* ((u   (- (* r2 (cos (+ angle da))) (* r1 cos-angle)))
                   (v   (- (* r2 (sin (+ angle da))) (* r1 sin-angle)))
                   (len (sqrt (+ (* u u) (* v v)))))
              (gl-normal (/ v len) (- (/ u len)) 0.0))

            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)

            (let ((u (- (* r1 (cos (+ angle da3)))
                        (* r2 (cos (+ angle da2)))))
                  (v (- (* r1 (sin (+ angle da3)))
                        (* r2 (sin (+ angle da2))))))
              (gl-normal v (- u) 0.0))

            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)))

        (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) half-width)
        (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) neg-half-width)
        (gl-end)

        (gl-shade-model 'smooth)

        ;; Draw inside radius cylinder.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-normal (- cos-angle) (- sin-angle) 0.0)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) half-width)))
        (gl-end)))

    (define/public (set-size width height)
      (with-gl-context
       (lambda ()

         (when verbose?
           (unless gear1
             (printf "  RENDERER:   ~A\n" (gl-get-string 'renderer))
             (printf "  VERSION:    ~A\n" (gl-get-string 'version))
             (printf "  VENDOR:     ~A\n" (gl-get-string 'vendor))
             (printf "  EXTENSIONS: ~A\n" (gl-get-string 'extensions))))

         (gl-viewport 0 0 width height)
         (gl-matrix-mode 'projection)
         (gl-load-identity)
         (let ((h (/ height width)))
           (gl-frustum -1.0 1.0 (- h) h 5.0 60.0))
         (gl-matrix-mode 'modelview)
         (gl-load-identity)
         (gl-translate 0.0 0.0 -40.0)

         (gl-light-v 'light0 'position (vector->gl-float-vector
                                        (vector 5.0 5.0 10.0 0.0)))
         (gl-enable 'cull-face)
         (gl-enable 'lighting)
         (gl-enable 'light0)
         (gl-enable 'depth-test)

         (unless gear1

           (set! gear1 (gl-gen-lists 1))
           (gl-new-list gear1 'compile)
           (gl-material-v 'front
                           'ambient-and-diffuse
                           (vector->gl-float-vector (vector 0.8 0.1 0.0 1.0)))
           (build-gear 1.0 4.0 1.0 20 0.7)
           (gl-end-list)

           (set! gear2 (gl-gen-lists 1))
           (gl-new-list gear2 'compile)
           (gl-material-v 'front
                           'ambient-and-diffuse
                           (vector->gl-float-vector (vector 0.0 0.8 0.2 1.0)))
           (build-gear 0.5 2.0 2.0 10 0.7)
           (gl-end-list)

           (set! gear3 (gl-gen-lists 1))
           (gl-new-list gear3 'compile)
           (gl-material-v 'front
                           'ambient-and-diffuse
                           (vector->gl-float-vector (vector 0.2 0.2 1.0 1.0)))
           (build-gear 1.3 2.0 0.5 10 0.7)
           (gl-end-list)

           (gl-enable 'normalize))))
      (refresh))
    
    (define/public (draw)
      (when gear1
	(when step?
	  ;; TODO: Don't increment this infinitely.
	  (set! rotation (+ 2.0 rotation)))
	(with-gl-context
	 (lambda ()

	   (gl-clear-color 0.0 0.0 0.0 0.0)
	   (gl-clear 'color-buffer-bit 'depth-buffer-bit)

	   (gl-push-matrix)
	   (gl-rotate view-rotx 1.0 0.0 0.0)
	   (gl-rotate view-roty 0.0 1.0 0.0)
	   (gl-rotate view-rotz 0.0 0.0 1.0)

	   (gl-push-matrix)
	   (gl-translate -3.0 -2.0 0.0)
	   (gl-rotate rotation 0.0 0.0 1.0)
	   (gl-call-list gear1)
	   (gl-pop-matrix)

	   (gl-push-matrix)
	   (gl-translate 3.1 -2.0 0.0)
	   (gl-rotate (- (* -2.0 rotation) 9.0) 0.0 0.0 1.0)
	   (gl-call-list gear2)
	   (gl-pop-matrix)

	   (gl-push-matrix)
	   (gl-translate -3.1 4.2 0.0)
	   (gl-rotate (- (* -2.0 rotation) 25.0) 0.0 0.0 1.0)
	   (gl-call-list gear3)
	   (gl-pop-matrix)

	   (gl-pop-matrix)

	   (swap-gl-buffers)
	   (gl-flush))))
      (cond
       [step?
        (set! step? #f)
        #t]
       [else #f]))))

(module+ main
  (require racket/gui/base)
  
  (define gears-canvas%
    (class* canvas% ()
      (inherit refresh with-gl-context swap-gl-buffers get-parent
               get-top-level-window)

      (define gears (new gears%
                         [with-gl-context
                          (lambda (thunk)
                            (with-gl-context
                             #:fail (lambda () (report-no-gl))
                             thunk))]
                         [swap-gl-buffers
                          (lambda () (swap-gl-buffers))]
                         [refresh
                          (lambda () (refresh))]
                         [verbose? #t]))

      (define/public (get-gears) gears)
      
      (super-new [style '(gl no-autoclear)])

      (define/private (report-no-gl)
        (message-box "Gears"
                     (string-append
                      "There was an error initializing OpenGL. "
                      "Maybe OpenGL is not supported on the current platform.")
                     (get-top-level-window)
                     '(ok stop))
        (exit 1))

      (define/override (on-size width height)
        (send gears set-size width height))

      (define sec (current-seconds))
      (define frames 0)
      
      (define/override (on-paint)
        (when (send gears ready?)
          (when (>= (- (current-seconds) sec) 5)
            (send (get-parent) set-status-text (format "~a fps" (/ (exact->inexact frames) 5)))
            (set! sec (current-seconds))
            (set! frames 0))
          (set! frames (add1 frames))
          
          (when (send gears draw)
            (queue-callback (lambda x (send gears run)) #f))))))

  (let* ((f (new frame% [label "gears.rkt"]))
         (c (new gears-canvas% (parent f) (min-width 300) (min-height 300))))
    (define g (send c get-gears))
    (send f create-status-line)
    (when controls?
      (let ((h (instantiate horizontal-panel% (f)
                 (alignment '(center center)) (stretchable-height #f))))
        (instantiate button%
          ("Start" h (lambda (b e) (send b enable #f) (send g run)))
          (stretchable-width #t) (stretchable-height #t))
        (let ((h (instantiate horizontal-panel% (h)
                   (alignment '(center center)))))
          (instantiate button% ("Left" h (lambda x (send g move-left)))
            (stretchable-width #t))
          (let ((v (instantiate vertical-panel% (h)
                     (alignment '(center center)) (stretchable-width #f))))
            (instantiate button% ("Up" v (lambda x (send g move-up)))
              (stretchable-width #t))
            (instantiate button% ("Down" v (lambda x (send g move-down)))
              (stretchable-width #t)))
          (instantiate button% ("Right" h (lambda x (send g move-right)))
            (stretchable-width #t)))))
    (send f show #t)))
