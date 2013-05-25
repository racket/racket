#lang racket/base

(require racket/class sgl/gl sgl/gl-vectors)

(provide string-init string-draw)

;; HERSHEY fonts
(define hershey-fonts
  '#( (#\A "MWRMNV RRMVV RPSTS")
      (#\B "MWOMOV ROMSMUNUPSQ ROQSQURUUSVOV")
      (#\C "MXVNTMRMPNOPOSPURVTVVU")
      (#\D "MWOMOV ROMRMTNUPUSTURVOV")
      (#\E "MWOMOV ROMUM ROQSQ ROVUV")
      (#\F "MVOMOV ROMUM ROQSQ")
      (#\G "MXVNTMRMPNOPOSPURVTVVUVR RSRVR")
      (#\H "MWOMOV RUMUV ROQUQ")
      (#\I "MTRMRV") ; modified
      (#\J "NUSMSTRVPVOTOS")
      (#\K "MWOMOV RUMOS RQQUV")
      (#\L "MVOMOV ROVUV")
      (#\M "LXNMNV RNMRV RVMRV RVMVV")
      (#\N "MWOMOV ROMUV RUMUV")
      (#\O "MXRMPNOPOSPURVSVUUVSVPUNSMRM")
      (#\P "MWOMOV ROMSMUNUQSROR")
      (#\Q "MXRMPNOPOSPURVSVUUVSVPUNSMRM RSTVW")
      (#\R "MWOMOV ROMSMUNUQSROR RRRUV")
      (#\S "MWUNSMQMONOOPPTRUSUUSVQVOU")
      (#\T "MWRMRV RNMVM")
      (#\U "MXOMOSPURVSVUUVSVM")
      (#\V "MWNMRV RVMRV")
      (#\W "LXNMPV RRMPV RRMTV RVMTV")
      (#\X "MWOMUV RUMOV")
      (#\Y "MWNMRQRV RVMRQ")
      (#\Z "MWUMOV ROMUM ROVUV")
      (#\space "LX")
      ;; numbers
      (#\0 "MWRMPNOPOSPURVTUUSUPTNRM")
      (#\1 "MWPORMRV")
      (#\2 "MWONQMSMUNUPTROVUV")
      (#\3 "MWONQMSMUNUPSQ RRQSQURUUSVQVOU")
      (#\4 "MWSMSV RSMNSVS")
      (#\5 "MWPMOQQPRPTQUSTURVQVOU RPMTM")
      (#\6 "MWTMRMPNOPOSPURVTUUSTQRPPQOS")
      (#\7 "MWUMQV ROMUM")
      (#\8 "MWQMONOPQQSQUPUNSMQM RQQOROUQVSVUUURSQ")
      (#\9 "MWUPTRRSPROPPNRMTNUPUSTURVPV")
      ;; signs
      (#\- "LXNRVR")
      (#\+ "LXRNRV RNRVR")
      ;; !!!!! this must exist !!!!!
      (#\* "MWRORU ROPUT RUPOT")
      ))

;; font database is a hash table
(define font-db    (make-hash))
(define font-gen   #f)
(define font-scale #f)

(define (real->int val)
  (inexact->exact (round val)))

;; interpret a hershey font
(define (interpret-hershey str scale)
  (let* ([nc (/ (string-length str) 2)]
         [cx #f] [cy #f] [x #f] [y #f]
         [left  (char->integer (string-ref str 0))]
         [right (char->integer (string-ref str 1))]
         [rchar (char->integer #\R)])
    (set! left  (- left rchar))
    (set! right (- right rchar))

    (glBegin GL_LINE_STRIP)
    (for ([i (in-range 1 nc)])
      (set! cx (string-ref str (+ (* i 2) 0)))
      (set! cy (string-ref str (+ (* i 2) 1)))
      (if (and (char=? cx #\space)
               (char=? cy #\R))
        (begin (glEnd)
               (glBegin GL_LINE_STRIP))
        (begin (set! x (* (- (char->integer cx) rchar) scale))
               (set! y (* (- (char->integer cy) rchar) scale))
               (glVertex2f x (- y)))))
    (glEnd)

    ;; width of the font
    (- right left)))


;; initialise the font database
(define (string-init scale)
  (let* ([n     (vector-length hershey-fonts)]
         [elem  #f]
         [width #f])

    (set! font-scale scale)
    (set! font-gen (glGenLists n))
    (glLineWidth 2.0)

    (for ([i (in-range n)])
      (set! elem (vector-ref hershey-fonts i))
      (glNewList (+ font-gen i) GL_COMPILE)
      (set! width (interpret-hershey (cadr elem) scale))
      (glEndList)

      (hash-set! font-db (car elem) (cons i width)))))

;; draw the text
(define (string-draw str)
  (let* ([n (string-length str)]
         [c #f] [e #f]
         [star (hash-ref font-db #\*)])
    (glPushMatrix)
    (glNormal3f 0.0 0.0 1.0)
    (for ([i (in-range n)])
      (set! c (string-ref str i))
      (set! e (hash-ref font-db c (lambda () star)))
      (glCallList (+ font-gen (car e)))
      (glTranslatef (* font-scale (cdr e)) 0.0 0.0))
    (glPopMatrix)))


;; -------------------------------------------------------
;; Testing

#|
(define *GL_VIEWPORT_WIDTH*  #f)
(define *GL_VIEWPORT_HEIGHT* #f)
(define scale 1.5)
(define bit '#(1 1 1 1  1 1 1 1
               1 1 1 1  1 1 1 1
               1 1 1 1  1 1 1 1
               1 1 1 1  1 1 1 1
               1 1 1 1  1 1 1 1
               1 1 1 1  1 1 1 1
               1 1 1 1  1 1 1 1
               1 1 1 1  1 1 1 1))

(define (my-display)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0 *GL_VIEWPORT_WIDTH* 0 *GL_VIEWPORT_HEIGHT* -1 1)

  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glTranslatef (/ *GL_VIEWPORT_WIDTH*  2)
                (/ *GL_VIEWPORT_HEIGHT* 2)
                0.0)
  (string-draw "+12" scale)
  ;; (glRasterPos2i 50 50)
  ;; (glBitmap 8 8 0.0 0.0 8.0 0.0 (vector->gl-ubyte-vector bit))
)

(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (glClearColor 0.0 0.0 0.0 0.0)
          (glClear GL_COLOR_BUFFER_BIT)
          (glClear GL_DEPTH_BUFFER_BIT)

          (my-display)
          (swap-gl-buffers))))

    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (set! *GL_VIEWPORT_WIDTH* width)
          (set! *GL_VIEWPORT_HEIGHT* height)

          (string-init scale))))

    (super-instantiate () (style '(gl)))))

;; initialise fonts

(let* ([f (make-object frame% "Font test" #f)]
       [w (instantiate my-canvas% (f)
                       (min-width  300)
                       (min-height 100))])

  (send f show #t))
|#
