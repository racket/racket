#lang racket/base
#|
;; This is how "file.png" and "folder.png" were generated, but it's commented
;; out here to avoid a dependency on `racket/draw`.
(require racket/class
         racket/math
         racket/draw)

(define S 32)

(define (file)
  (define bm (make-bitmap S S))
  (define dc (send bm make-dc))

  (define I 3)

  (send dc set-smoothing 'smoothed)

  (send dc set-pen "black" 2 'solid)
  (send dc set-brush (make-color 200 200 200) 'solid)
  (send dc draw-polygon (list
                         (cons 6 I)
                         (cons 6 (- S I))
                         (cons (- S 6) (- S I))
                         (cons (- S 6) (+ 8 I))
                         (cons (- S 14) I))
        0 1)

  (send dc set-brush (make-color 150 150 150) 'solid)
  (send dc draw-polygon (list
                         (cons (- S 14) (+ 8 I))
                         (cons (- S 6) (+ 8 I))
                         (cons (- S 14) I))
        0 1)

  (send bm save-file "/tmp/file.png" 'png #:unscaled? #t))

(define (folder)
  (define bm (make-bitmap S S))
  (define dc (send bm make-dc))

  (define D 8)
  (define H 12)
  (define T (/ D 2))

  (send dc set-smoothing 'smoothed)

  (send dc set-pen "black" 2 'solid)
  (send dc set-brush (make-color 200 200 200) 'solid)

  (define p (new dc-path%))
  (send p arc (+ 1 T) (- S D 1 H T) D D 0 (* 1/2 pi) #t)
  (send p arc 1 (- S D 1 H T) D D (* 1/2 pi) (* 1 pi) #t)
  (send p line-to 1 (- S 1))
  ;; (send p arc 1 (- S D 1) D D (* -1 pi) (* -1/2 pi) #t)
  (send p line-to (- S 1) (- S 1))
  ;; (send p arc (- S D 1) (- S D 1) D D (* -1/2 pi) 0 #t)
  (send p arc (- S D 1) (- S D 1 H) D D 0 (* 1/2 pi) #t)
  (send p close)

  (send dc draw-path p 0 0)
  
  (send bm save-file "/tmp/folder.png" 'png #:unscaled? #t))

(file)
(folder)
|#

