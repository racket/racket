#lang racket/base

;; A library for creating gifs out of bitmap%s

(require racket/gui/base
         racket/class
         racket/list
         net/gifwrite
         racket/contract)

(provide write-gif)

(define (force-bm bm) (if (procedure? bm) (bm) bm))

(define (split-bytes b len offset)
  (if (= offset (bytes-length b))
      null
      (cons (subbytes b offset (+ offset len))
            (split-bytes b len (+ offset len)))))

(define (write-gifs bms delay filename one-at-a-time? last-frame-delay loop?)
  (let* ([init (force-bm (car bms))]
         [w (send init get-width)]
         [h (send init get-height)])
    (let ([argb-thunks
           (map (lambda (bm)
                  (lambda ()
                    (let ([bm (force-bm bm)]
                          [argb (make-bytes (* w h 4) 255)])
                      (send bm get-argb-pixels 0 0 w h argb)
                      (let ([mask (send bm get-loaded-mask)])
                        (when mask
                          (send mask get-argb-pixels 0 0 w h argb #t)))
                      argb)))
                (cons init (cdr bms)))])
      (if one-at-a-time?
          ;; Quantize individually, and stream the images through
          (call-with-output-file*
           filename
           (lambda (p)
             (let* ([gif (gif-start p w h 0 #f)])
               (when loop?
                 (gif-add-loop-control gif 0))
               (let ([last-argb-thunk (last argb-thunks)])
                 (for-each (lambda (argb-thunk)
                             (let-values ([(pixels colormap transparent)
                                           (quantize (argb-thunk))])
                               (when (or transparent delay)
                                 (gif-add-control gif 'any #f (or delay 0) transparent))
                               (gif-add-image gif 0 0 w h #f colormap pixels)
                               (when (and last-frame-delay (eq? argb-thunk last-argb-thunk))
                                 (gif-add-control gif 'any #f last-frame-delay transparent)
                                 (gif-add-image gif 0 0 w h #f colormap pixels))))
                           argb-thunks))
               (gif-end gif))))
          ;; Build images and quantize all at once:
          (let-values ([(pixels colormap transparent)
                        (quantize (apply bytes-append (map (lambda (t) (t)) argb-thunks)))])
            (call-with-output-file*
             filename
             (lambda (p)
               (let* ([gif (gif-start p w h 0 colormap)])
                 (when delay
                   (gif-add-loop-control gif 0))
                 (let* ([pixelss (split-bytes pixels (* w h) 0)]
                        [last-pixels (last pixelss)])
                   (for-each (lambda (pixels)
                               (when (or transparent delay)
                                 (gif-add-control gif 'any #f (or delay 0) transparent))
                               (gif-add-image gif 0 0 w h #f #f pixels)
                               (when (and last-frame-delay (eq? pixels last-pixels))
                                 (gif-add-control gif 'any #f last-frame-delay transparent)
                                 (gif-add-image gif 0 0 w h #f colormap pixels)))
                             pixelss))
                 (gif-end gif)))))))))

(define (write-gif bm filename)
  (write-gifs (list bm) #f filename #f #f #f))

(provide/contract
 [write-animated-gif
  (->i ((bms (and/c (listof (or/c (is-a?/c bitmap%) (-> (is-a?/c bitmap%)))) pair?))
        (delay (integer-in 0 4294967295))
        (filename (or/c path? string?)))
       (#:one-at-a-time? (one-at-a-time? any/c)
        #:last-frame-delay (last-frame-delay (or/c (integer-in 0 4294967295) false/c))
        #:loop? (Loop? (delay) (lambda (x) (and delay #t))))
       any)])

(define (write-animated-gif bms delay filename
                            #:one-at-a-time? [one-at-a-time? #f]
                            #:last-frame-delay [last-frame-delay #f]
                            #:loop? [loop? (and delay #t)])
  (write-gifs bms delay filename one-at-a-time? last-frame-delay loop?))

