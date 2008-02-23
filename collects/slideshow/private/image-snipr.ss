(module image-snipr mzscheme
  (require mred
           mzlib/class)

  (provide snipclass
           image-snip/r%)
  
  (define image-snip/r%
    (class image-snip%
      (init bitmap)
      (init-field orig-snip)
      (define/public (get-orig-snip) orig-snip)
      
      (inherit get-bitmap)
      (define/override (copy) (make-object image-snip/r% (get-bitmap) orig-snip))
      
      (super-make-object bitmap)
      
      (inherit set-snipclass set-bitmap)
      (set-snipclass snipclass)
      
      (define/override (write stream-out)
        (super write stream-out)
        (let* ([sc (send orig-snip get-snipclass)]
               [cn-bytes (string->bytes/utf-8 (send sc get-classname))])
          (send stream-out put (+ (bytes-length cn-bytes) 1) cn-bytes)
          (send orig-snip write stream-out)))))
  
  (define image-snip/r-snipclass%
    (class snip-class%
      (define/override (read stream-in)
        (let* ([is-sc (send (get-the-snip-class-list) find "wximage")]
               [bs (send is-sc read stream-in)]
               [bm (send bs get-bitmap)])
          (send bs set-bitmap (make-object bitmap% 1 1)) ;; ugh
          (let* ([name (bytes->string/utf-8 (send stream-in get-bytes))]
                 [sc (send (get-the-snip-class-list) find name)])
            (unless sc
              (error 'ack! "did not find a snipclass ~s, so cannot continue parsing stream" name))
            (let* ([hidden-snip (send sc read stream-in)])
              (make-object image-snip/r% bm hidden-snip)))))
      (super-new)))
  
  (define snipclass (new image-snip/r-snipclass%))
  (send snipclass set-classname (format "~s" '(lib "image-snipr.ss" "slideshow" "private")))
  (send snipclass set-version 1)
  (send (get-the-snip-class-list) add snipclass))
