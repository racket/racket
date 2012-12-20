#lang racket/base
(require racket/class
         racket/draw
         racket/file)

;; Check memory-management in the JPEG library by reading and writing
;; in many threads at the same time.

(define ts
  (for/list ([i (in-range 40)])
    (thread
     (lambda()
       (for ([i (in-range 10)])
         (define bm (read-bitmap (collection-file-path "plt.jpg" "icons")))
         (define t (make-temporary-file))
         (send bm save-file t 'jpeg)
         (define bm2 (read-bitmap t))
         (define w (send bm get-width))
         (define h (send bm get-width))
         (define s1 (make-bytes (* w h 4)))
         (define s2 (make-bytes (* w h 4)))
         (send bm get-argb-pixels 0 0 w h s1)
         (send bm2 get-argb-pixels 0 0 w h s2)
         ;; JPEG is lossy, so use a fuzzy compare:
         (define diff (for/sum ([b1 (in-bytes s1)]
                                [b2 (in-bytes s2)])
                               (- b2 b1)))
         (unless ((abs diff) . < . (* w h 1))
           (error 'jpeg-stree "mismatch ~s ~s ~e" w h diff))
         (delete-file t))))))

(for ([t (in-list ts)]) (sync t))

       