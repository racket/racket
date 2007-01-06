
(module gif mzscheme
  (require (lib "class.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "gifwrite.ss" "net"))

  (provide write-gif
           write-animated-gif)

  (define (split-bytes b len offset)
    (if (= offset (bytes-length b))
        null
        (cons (subbytes b offset (+ offset len))
              (split-bytes b len (+ offset len)))))

  (define (write-gifs bms delay filename)
    (let ([w (send (car bms) get-width)]
          [h (send (car bms) get-height)])
      (let ([argbs
             (map (lambda (bm)
                    (let ([argb (make-bytes (* w h 4) 255)])
                      (send bm get-argb-pixels 0 0 w h argb)
                      (let ([mask (send bm get-loaded-mask)])
                        (when mask
                          (send mask get-argb-pixels 0 0 w h argb #t)))
                      argb))
                  bms)])
        (let-values ([(pixels colormap transparent)
                      (quantize (apply bytes-append argbs))])
          (call-with-output-file*
           filename
           (lambda (p)
             (let* ([gif (gif-start p w h 0 colormap)])
               (when delay
                 (gif-add-loop-control gif 0))
               (for-each (lambda (pixels)
                           (when (or transparent delay)
                             (gif-add-control gif 'any #f (or delay 0) transparent))
                           (gif-add-image gif 0 0 w h #f #f pixels))
                         (split-bytes pixels (* w h) 0))
               (gif-end gif))))))))

  (define (write-gif bm filename)
    (write-gifs (list bm) #f filename))

  (define (write-animated-gif bms delay filename)
    (write-gifs bms delay filename))

  )

               