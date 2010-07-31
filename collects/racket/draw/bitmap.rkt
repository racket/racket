#lang scheme/base
(require scheme/class
         scheme/unsafe/ops
         mred/private/syntax
         "hold.ss"
         "bstr.ss"
         "cairo.ss"
         "png.ss"
         "jpeg.ss"
         "xbm.ss"
         "xpm.ss"
         "bmp.ss"
         "gif.rkt"
         "local.ss"
         "color.ss")

(provide bitmap%)

;; FIXME: there must be some way to abstract over all many of the
;; ARGB/RGBA/BGRA iterations.

(define-local-member-name
  get-alphas-as-mask
  set-alphas-as-mask)

(define (kind-symbol? s)
  (memq s '(unknown unknown/mask unknown/alpha
                    gif gif/mask gif/alpha 
                    jpeg jpeg/alpha
                    png png/mask png/alpha
                    xbm xbm/alpha 
                    xpm xpm/alpha 
                    bmp bmp/alpha
                    pict)))

(define (save-kind-symbol? s)
  (memq s '(png jpeg gif xbm xpm bmp)))

(define (quality-integer? i)
  (and (exact-nonnegative-integer? i) (i . <= . 100)))

(define (destroy s)
  (cairo_surface_destroy s))

(define (argb-indices)
  (if (system-big-endian?)
      (values 0 1 2 3)
      (values 3 2 1 0)))

(define (a-index)
  (if (system-big-endian?) 0 3))

(define (b-index)
  (if (system-big-endian?) 3 0))

(define fx+ unsafe-fx+)
(define fx* unsafe-fx*)

(define bitmap%
  (class object%

    ;; We support three kinds of bitmaps:
    ;;  * Color with alpha channel; 
    ;;    when used as a mask, alpha channel is used;
    ;;    this is the sensible one that works nicely with Cairo
    ;;  * Black and white; alpha channel is opposite
    ;;    of value, so either value or alpha can be
    ;;    considered as mask;
    ;;    we have to play some tricks to keep the value and mask
    ;;    inverted, and to keep everything black & white (no gray)
    ;;  * Color without alpha channel; when used as a mask,
    ;;    value channel is used (i.e., inverted RGB average
    ;;    is used as an alpha);
    ;;    we have to play even worse tricks when this kind of bitmap
    ;;    is used as a mask

    (init-rest args)
    (super-new)

    (define-values (width height b&w? alpha-channel? s loaded-mask)
      (case-args 
       args
       [() (void)]
       [([exact-nonnegative-integer? w]
         [exact-nonnegative-integer? h]
         [any? [b&w? #f]]
         [any? [alpha? #f]])
        (values
         w
         h
         (and b&w? #t)
         (and alpha? (not b&w?))
         (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)])
           (cairo_surface_flush s)
           (cond
            [alpha? 
             ;; Init transparent:
             (bytes-fill! (cairo_image_surface_get_data s) 0)]
            [b&w?
             ;; Init transparent white:
             (transparent-white! s w h)]
            [else
             ;; Init all white, 255 alpha:
             (bytes-fill! (cairo_image_surface_get_data s) 255)])
           s)
         #f)]
       [([path-string? filename]
         [kind-symbol? [kind 'unknown]]
         [(make-or-false color%) [bg-color #f]])
        (let-values ([(s b&w?) (do-load-bitmap filename kind bg-color)]
                     [(alpha?) (memq kind '(unknown/alpha gif/alpha jpeg/alpha
                                                          png/alpha xbm/alpha xpm/alpha 
                                                          bmp/alpha))]
                     [(mask?) (memq kind '(unknown/mask gif/mask png/mask))])
          (let ([mask-bm
                 (and s
                      (not alpha?)
                      (not b&w?)
                      (let ([w (cairo_image_surface_get_width s)]
                            [h (cairo_image_surface_get_height s)]
                            [row-width (cairo_image_surface_get_stride s)]
                            [bstr (cairo_image_surface_get_data s)]
                            [A (a-index)])
                        (begin0
                         (and mask?
                              ;; Move alpha channel to a separate mask bitmap
                              (let ([b&w? (for*/and ([j (in-range h)]
                                                     [i (in-range w)])
                                                    (let ([v (bytes-ref bstr (+ A (* 4 i) (* j row-width)))])
                                                      (or (= v 0) (= v 255))))])
                                (let ([mask-bm (make-object bitmap% w h b&w?)])
                                  (send mask-bm set-alphas-as-mask 0 0 w h bstr row-width A)
                                  mask-bm)))
                         ;; Force all alpha values to 255
                         (for* ([j (in-range h)]
                                [i (in-range w)])
                           (bytes-set! bstr (+ A (* 4 i) (* j row-width)) 255))
                         (cairo_surface_mark_dirty s))))])
            (if s
                (values (cairo_image_surface_get_width s)
                        (cairo_image_surface_get_height s)
                        b&w?
                        alpha?
                        s
                        mask-bm)
                (values 0 0 #f #f #f #f))))]
       [([bytes? bstr]
         [exact-nonnegative-integer? w]
         [exact-nonnegative-integer? h])
        (let ([bw (quotient (+ w 7) 8)])
          (unless ((bytes-length bstr) . >= . (* h bw))
            (error (init-name 'bitmap%)
                   "given byte string is too small for dimensions: ~s"
                   bstr))
          (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)])
            (let ([rows (list->vector
                         (for/list ([i (in-range h)])
                           (let ([s (* i bw)])
                             (subbytes bstr s (+ s bw)))))])
              (install-bytes-rows s w h rows #t #f #f #t))
            (values w h #t #f s #f)))]
       (init-name 'bitmap%)))

    ;; Use for non-alpha color bitmaps when they are used as a mask:
    (define alpha-s #f)
    (define alpha-s-up-to-date? #f)
    (define/private (drop-alpha-s)
      (set! alpha-s-up-to-date? #f)
      (when alpha-s
        (let ([s2 alpha-s])
          (set! alpha-s #f)
          (destroy s2))))

    ;; Allocate memory proportional to the size of the bitmap, which
    ;; helps the GC see that we're using that much memory.
    (define shadow (make-bytes (* width height (if b&w? 1 4))))

    (def/public (get-width) width)
    (def/public (get-height) height)
    (def/public (get-depth) (if b&w? 1 32))
    (def/public (is-color?) (not b&w?))
    (def/public (has-alpha-channel?) alpha-channel?)

    (def/public (get-loaded-mask) loaded-mask)
    (def/public (set-loaded-mask [(make-or-false bitmap%) m]) (set! loaded-mask m))

    (define/private (release-s)
      (drop-alpha-s)
      (when s
        (let ([s2 s])
          (set! s #f)
          (destroy s2))))

    (define/private (check-ok who)
      (unless s
        (error (method-name 'bitmap% who) "bitmap is not ok")))

    (define locked 0)
    (define/public (adjust-lock delta) (set! locked (+ locked delta)))

    (def/public (load-bitmap [(make-alts path-string? input-port?) in]
                             [kind-symbol? [kind 'unknown]]
                             [(make-or-false color%) [bg #f]])
      (release-s)
      (set!-values (s b&w?) (do-load-bitmap in kind bg))
      (set! width (if s (cairo_image_surface_get_width s) 0))
      (set! height (if s (cairo_image_surface_get_height s) 0)))

    (define/private (do-load-bitmap in kind bg)
      (if (path-string? in)
          (call-with-input-file*
           in
           (lambda (in) (do-load-bitmap in kind bg)))
          (case kind
           [(unknown unknown/mask unknown/alpha)
            (let ([starts? (lambda (s)
                             (equal? (peek-bytes (bytes-length s) 0 in) s))])
              (cond
               [(starts? #"\211PNG\r\n")
                (do-load-bitmap in 
                                (if (eq? kind 'unknown/alpha) 
                                    'png/alpha 
                                    (if (eq? kind 'unknown/mask) 
                                        'png/mask
                                        'png))
                                bg)]
               [(starts? #"\xFF\xD8\xFF")
                (do-load-bitmap in 'jpeg bg)]
               [(starts? #"GIF8")
                (do-load-bitmap in 'gif bg)]
               [(starts? #"BM")
                (do-load-bitmap in 'bmp bg)]
               [(starts? #"#define")
                (do-load-bitmap in 'xbm bg)]
               [(starts? #"/* XPM */")
                (do-load-bitmap in 'xpm bg)]
               [else
                ;; unrecognized file type; try to parse as XBM
                (do-load-bitmap in 'xbm bg)]))]
           [(png png/mask png/alpha)
            ;; Using the Cairo PNG support is about twice as fast, but we have
            ;; less control, and there are problems making deallocation reliable
            ;; (in case of exceptions or termination):
            #;
            (let ([proc (lambda (ignored bstr len)
                          (read-bytes! (scheme_make_sized_byte_string bstr len 0) in)
                          CAIRO_STATUS_SUCCESS)])
              (with-holding
               proc
               (values (cairo_image_surface_create_from_png_stream proc) #f)))
            ;; Using libpng directly:
            (let-values ([(r w h b&w? alpha?) (create-png-reader 
                                               in
                                               (and bg
                                                    (list (send bg red)
                                                          (send bg green)
                                                          (send bg blue))))])
              (let ([rows (read-png r)])
                (destroy-png-reader r)
                (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                       [pre? (and alpha? (memq kind '(png/alpha png/mask)))])
                  (install-bytes-rows s w h rows b&w? alpha? pre? #f)
                  (values s b&w?))))]
           [(jpeg jpeg/alpha)
            (let ([d (create-decompress in)])
              (dynamic-wind
                  void
                  (lambda ()
                    (jpeg_read_header d #t)
                    (jpeg_start_decompress d)
                    (let ([w (jpeg_decompress_struct-output_width d)]
                          [h (jpeg_decompress_struct-output_height d)]
                          [c (jpeg_decompress_struct-output_components d)])
                      (let-values ([(samps bstr) (create-jpeg-sample-array d (* w c))]
                                   [(A R G B) (argb-indices)])
                        (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                               [dest (begin
                                       (cairo_surface_flush s)
                                       (cairo_image_surface_get_data s))]
                               [dest-row-width (cairo_image_surface_get_stride s)])
                          (for ([j (in-range h)])
                            (jpeg_read_scanlines d samps 1)
                            (let ([row (* dest-row-width j)])
                              (for ([i (in-range w)])
                                (let ([4i (fx+ row (fx* 4 i))]
                                      [ci (fx* c i)])
                                  (unsafe-bytes-set! dest (fx+ 4i A) 255)
                                  (if (= c 1)
                                      (let ([v (unsafe-bytes-ref bstr ci)])
                                        (unsafe-bytes-set! dest (fx+ 4i R) v)
                                        (unsafe-bytes-set! dest (fx+ 4i G) v)
                                        (unsafe-bytes-set! dest (fx+ 4i B) v))
                                      (begin
                                        (unsafe-bytes-set! dest (fx+ 4i R) (unsafe-bytes-ref bstr ci))
                                        (unsafe-bytes-set! dest (fx+ 4i G) (unsafe-bytes-ref bstr (fx+ ci 1)))
                                        (unsafe-bytes-set! dest (fx+ 4i B) (unsafe-bytes-ref bstr (fx+ ci 2)))))))))
                          (cairo_surface_mark_dirty s)
                          (jpeg_finish_decompress d)
                          (values s #f)))))
                  (lambda ()
                    (destroy-decompress d))))]
           [(gif gif/mask gif/alpha)
            (let-values ([(w h rows) (gif->rgba-rows in)])
              (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                     [alpha? #t]
                     [pre? #f]
                     [b&w? #f])
                (install-bytes-rows s w h rows b&w? alpha? pre? #f)
                (values s b&w?)))]
           [(xbm xbm/alpha)
            (let-values ([(w h rows) (read-xbm in)])
              (if rows
                  (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)])
                    (install-bytes-rows s w h rows #t #f #f #t)
                    (values s #t))
                  (values #f #f)))]
           [(xpm xpm/alpha)
            (let-values ([(w h rows) (read-xpm in)])
              (if rows
                  (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                        [alpha? #t])
                    (install-bytes-rows s w h rows #f alpha? #t #f)
                    (values s #f))
                  (values #f #f)))]
           [(bmp bmp/alpha)
            (let-values ([(w h rows) (read-bmp in)])
              (if rows
                  (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                        [alpha? #t])
                    (install-bytes-rows s w h rows #f alpha? #t #f)
                    (values s #f))
                  (values #f #f)))]
           [else (values #f #f)])))
    
    ;; s : Cairo bitmap surface
    ;; w, h : width and height in pixels
    ;; rows : a vector of `h' byte strings
    ;; b&w? : each bit in a byte string is a pixel (so each byte
    ;;        string in `rows' is `(/ w 8)' bytes long)
    ;;        if not `backward?': low bit is first and 0 is black
    ;;        if `backward?': high bit is first and 1 is black
    ;; alpha? : relevant only if not `b&w?';
    ;;          if true: each byte string has 4 bytes per pixel, RGBA
    ;;          if false: each byte string has 3 bytes er pixel, RGB
    ;; pre? : should be #f if not `alpha?', otherwise a #t value
    ;;        means that the RGB values should be multiplied by A value
    ;;        (i.e., the values are not already pre-multiplied)
    ;; backward? : affects byte interpretation in `b&w?' mode; see above
    (define/private (install-bytes-rows s w h rows b&w? alpha? pre? backward?)
      (let* ([dest (begin
                     (cairo_surface_flush s)
                     (cairo_image_surface_get_data s))]
             [dest-row-width (cairo_image_surface_get_stride s)])
        (let-values ([(A R G B) (argb-indices)])
          (for ([r (in-vector rows)]
                [j (in-naturals)])
            (let ([row (* dest-row-width j)])
              (if b&w?
                  (for ([i (in-range w)])
                    (let ([b (unsafe-fxquotient i 8)]
                          [bit (if backward?
                                   (unsafe-fxlshift 1 (unsafe-fxand i 7))
                                   (unsafe-fxrshift 128 (unsafe-fxand i 7)))]
                          [pos (fx+ row (fx* 4 i))])
                      (let* ([v (if (zero? (unsafe-fxand bit (unsafe-bytes-ref r b)))
                                    0 
                                    255)]
                             [v (if backward? (- 255 v) v)])
                        (unsafe-bytes-set! dest (fx+ pos A) (- 255 v))
                        (unsafe-bytes-set! dest (fx+ pos 1) v)
                        (unsafe-bytes-set! dest (fx+ pos 2) v)
                        (unsafe-bytes-set! dest (fx+ pos B) v))))
                  (for ([i (in-range w)])
                    (let* ([4i (fx* 4 i)]
                           [pos (fx+ row 4i)]
                           [spos (if alpha?
                                     (fx* 4 i)
                                     (fx* 3 i))]
                           [al (if alpha?
                                   (unsafe-bytes-ref r (fx+ spos 3))
                                   255)]
                           [a (if pre?
                                  al
                                  255)]
                           [premult (lambda (al v)
                                      (if pre?
                                          (unsafe-fxquotient (fx* al v) 255)
                                          (if alpha?
                                              (unsafe-fxquotient 
                                               (+ (* 255 (- 255 al))
                                                  (* v al))
                                               255)
                                              v)))])
                      (unsafe-bytes-set! dest (fx+ pos A) a)
                      (unsafe-bytes-set! dest (fx+ pos R) (premult al (unsafe-bytes-ref r spos)))
                      (unsafe-bytes-set! dest (fx+ pos G) (premult al (unsafe-bytes-ref r (fx+ spos 1))))
                      (unsafe-bytes-set! dest (fx+ pos B) (premult al (unsafe-bytes-ref r (fx+ spos 2))))))))))
        (cairo_surface_mark_dirty s)))

    (def/public (save-file [(make-alts path-string? output-port?) out]
                           [save-kind-symbol? [kind 'unknown]]
                           [quality-integer? [quality 75]])
      (check-ok 'save-file)
      (do-save-file out kind quality))

    (define/private (do-save-file out kind quality)
      (if (path-string? out)
          (call-with-output-file*
           out
           #:exists 'truncate/replace
           (lambda (out) (do-save-file out kind quality)))
          (case kind
            [(png)
             (cond
              [b&w?
               ;; Write a 1-bit png
               (let* ([b (ceiling (/ width 8))]
                      [rows (build-vector height (lambda (i) (make-bytes b)))]
                      [data (begin (cairo_surface_flush s)
                                   (cairo_image_surface_get_data s))]
                      [row-width (cairo_image_surface_get_stride s)])
                 (for ([j (in-range height)])
                   (let ([row (vector-ref rows j)])
                     (for ([bi (in-range b)])
                       (bytes-set! 
                        row
                        bi
                        (let ([src (+ (* j row-width) (* (* bi 8) 4))])
                          (for/fold ([v 0]) 
                              ([k (in-range 8)])
                            (if ((+ (* 8 bi) k) . < . width)
                                (if (zero? (bytes-ref data (+ src (* 4 k))))
                                    v
                                    (bitwise-ior v (unsafe-fxrshift 128 k)))
                                v)))))))
                 (let ([w (create-png-writer out width height #t #f)])
                   (write-png w rows)
                   (destroy-png-writer w)))]
              [(and (not alpha-channel?) 
                    loaded-mask
                    (= width (send loaded-mask get-width))
                    (= height (send loaded-mask get-height)))
               (let ([bstr (make-bytes (* width height 4))])
                 (get-argb-pixels 0 0 width height bstr)
                 (get-argb-pixels 0 0 width height bstr #t)
                 ;; PNG wants RGBA instead of ARGB...
                 (let ([rows (build-vector height (lambda (i) (make-bytes (* 4 width))))])
                   (for ([j (in-range height)]
                         [dest-row (in-vector rows)])
                     (let ([src-row (* j (* 4 width))])
                       (for ([i (in-range width)])
                         (let* ([4i (* 4 i)]
                                [ri (+ src-row 4i)])
                           (bytes-set! dest-row 4i (bytes-ref bstr (+ 1 ri)))
                           (bytes-set! dest-row (+ 4i 1) (bytes-ref bstr (+ 2 ri)))
                           (bytes-set! dest-row (+ 4i 2) (bytes-ref bstr (+ 3 ri)))
                           (bytes-set! dest-row (+ 4i 3) (bytes-ref bstr ri))))))
                   (let ([w (create-png-writer out width height #f #t)])
                     (write-png w rows)
                     (destroy-png-writer w))))]
              [else
               ;; Use Cairo built-in support:
               (let ([proc (lambda (ignored bstr len)
                             (write-bytes (scheme_make_sized_byte_string bstr len 0) out)
                             CAIRO_STATUS_SUCCESS)])
                 (with-holding
                  proc
                  (cairo_surface_write_to_png_stream s proc)))])]
            [(jpeg)
             (let ([c (create-compress out)])
               (dynamic-wind
                   void
                   (lambda ()
                     (set-jpeg_compress_struct-image_width! c width)
                     (set-jpeg_compress_struct-image_height! c height)
                     (set-jpeg_compress_struct-input_components! c 3)
                     (set-jpeg_compress_struct-in_color_space! c JCS_RGB)
                     (jpeg_set_defaults c)
                     (jpeg_set_quality c quality #t)
                     (jpeg_start_compress c #t)
                     (let-values ([(samps bstr) (create-jpeg-sample-array c (* width height 3))]
                                  [(A R G B) (argb-indices)])
                       (cairo_surface_flush s)
                       (let* ([dest (cairo_image_surface_get_data s)]
                              [dest-row-width (cairo_image_surface_get_stride s)]
                              [h height]
                              [w width])
                         (for ([j (in-range h)])
                           (let ([row (* dest-row-width j)])
                             (for ([i (in-range w)])
                               (let ([4i (* 4 i)]
                                     [ci (* 3 i)])
                                 (bytes-set! bstr ci (bytes-ref dest (+ row (+ 4i R))))
                                 (bytes-set! bstr (+ ci 1) (bytes-ref dest (+ row (+ 4i G))))
                                 (bytes-set! bstr (+ ci 2) (bytes-ref dest (+ row (+ 4i B)))))))
                           (jpeg_write_scanlines c samps 1))))
                     (jpeg_finish_compress c))
                   (lambda () (destroy-compress c))))]
            [else (error (method-name 'bitmap% 'save-file)
                         "kind saving not yet implemented: ~e"
                         kind)])))

    (def/public (ok?) (and s #t))

    (define/public (get-cairo-surface) s)
    (define/public (get-cairo-alpha-surface)
      (if (or b&w? alpha-channel?)
          s
          (begin
            (prep-alpha)
            alpha-s)))

    (def/public (get-argb-pixels [exact-nonnegative-integer? x]
                                 [exact-nonnegative-integer? y]
                                 [exact-nonnegative-integer? w]
                                 [exact-nonnegative-integer? h]
                                 [bytes? bstr]
                                 [any? [get-alpha? #f]])
      (unless ((bytes-length bstr) . >  . (* w h))
        (raise-mismatch-error (method-name 'bitmap% 'get-argb-pixels)
                              "byte string is too short: "
                              bstr))
      ;; Fill range that is beyond edge of picture:
      (if get-alpha?
          (for* ([i (in-range width (+ x w))]
                 [j (in-range height (+ y h))])
            (bytes-set! bstr (* 4 (+ i (* j w))) 255))
          (for* ([i (in-range width (+ x w))]
                 [j (in-range height (+ y h))])
            (let ([p (* 4 (+ i (* j w)))])
              (bytes-set! bstr p 255)
              (bytes-set! bstr (+ p 1) 0)
              (bytes-set! bstr (+ p 2) 0)
              (bytes-set! bstr (+ p 3) 0))))
      ;; Get pixels:
      (let-values ([(A R G B) (argb-indices)])
        (when (not get-alpha?)
          (cairo_surface_flush s)
          (let ([data (cairo_image_surface_get_data s)]
                [row-width (cairo_image_surface_get_stride s)])
            (let ([w2 (+ x (min (- width x) w))])
              (for* ([j (in-range y (min (+ y h) height))])
                (let ([row (* j row-width)]
                      [p (* 4 (* (- j y) w))])
                  (for ([i (in-range x w2)])
                    (let* ([4i (* 4 i)]
                           [pi (+ p (* 4 (- i x)))]
                           [ri (+ row 4i)]
                           [a (bytes-ref data (+ ri A))]
                           [unmult (lambda (a v)
                                     (if alpha-channel?
                                         (if (zero? a)
                                             255
                                             (unsafe-fxquotient (fx* v 255) a))
                                         v))])
                      (when alpha-channel?
                        (bytes-set! bstr pi a))
                      (bytes-set! bstr (+ pi 1) (unmult a (bytes-ref data (+ ri R))))
                      (bytes-set! bstr (+ pi 2) (unmult a (bytes-ref data (+ ri G))))
                      (bytes-set! bstr (+ pi 3) (unmult a (bytes-ref data (+ ri B))))))))))))
      (cond
       [(and get-alpha?
             (not alpha-channel?)
             loaded-mask
             (= width (send loaded-mask get-width))
             (= height (send loaded-mask get-height)))
        ;; Get alpha from mask bitmap:
        (send loaded-mask get-alphas-as-mask x y w h bstr)]
       [(and get-alpha? alpha-channel?)
        (get-alphas-as-mask x y w h bstr)]
       [(and (not get-alpha?) (not alpha-channel?))
        ;; For non-alpha mode or no mask; fill in 255s:
        (for ([j (in-range 0 (min h (- height y)))])
          (let ([row (* j (* 4 w))])
            (for ([i (in-range 0 (min w (- width x)))])
              (let ([p (+ (* 4 i) row)])
                (bytes-set! bstr p 255)))))]))

    (def/public (set-argb-pixels [exact-nonnegative-integer? x]
                                 [exact-nonnegative-integer? y]
                                 [exact-nonnegative-integer? w]
                                 [exact-nonnegative-integer? h]
                                 [bytes? bstr]
                                 [any? [set-alpha? #f]])
      (unless ((bytes-length bstr) . >  . (* w h))
        (raise-mismatch-error (method-name 'bitmap% 'set-argb-pixels)
                              "byte string is too short: "
                              bstr))
      ;; Set pixels:
      (let-values ([(A R G B) (argb-indices)])
        (when (not set-alpha?)
          (cairo_surface_flush s)
          (let ([data (cairo_image_surface_get_data s)]
                [row-width (cairo_image_surface_get_stride s)])
            (let ([w2 (+ x (min (- width x) w))])
              (for ([j (in-range y (min (+ y h) height))]
                    [dj (in-naturals)])
                (let ([row (* j row-width)]
                      [p (* 4 (* dj w))])
                  (for ([i (in-range x w2)])
                    (let* ([4i (* 4 i)]
                           [pi (+ p (* 4 (- i x)))]
                           [ri (+ row 4i)])
                      (if b&w?
                          (let ([v (if (and (= (bytes-ref bstr (+ pi 1)) 255)
                                            (= (bytes-ref bstr (+ pi 2)) 255)
                                            (= (bytes-ref bstr (+ pi 3)) 255))
                                       255
                                       0)])
                            (bytes-set! data (+ ri A) (- 255 v))
                            (bytes-set! data (+ ri 1) v)
                            (bytes-set! data (+ ri 2) v)
                            (bytes-set! data (+ ri B) v))
                          (begin
                            (when alpha-channel? 
                              (bytes-set! data (+ ri A) (bytes-ref bstr pi)))
                            (bytes-set! data (+ ri R) (bytes-ref bstr (+ pi 1)))
                            (bytes-set! data (+ ri G) (bytes-ref bstr (+ pi 2)))
                            (bytes-set! data (+ ri B) (bytes-ref bstr (+ pi 3)))))))))))
          (cairo_surface_mark_dirty s)))
      (cond
       [(and set-alpha?
             (not alpha-channel?)
             loaded-mask
             (= width (send loaded-mask get-width))
             (= height (send loaded-mask get-height)))
        ;; Set alphas in mask bitmap:
        (send loaded-mask set-alphas-as-mask x y w h bstr (* 4 w) 0)]))
    
    (define/public (get-alphas-as-mask x y w h bstr)
      (let ([data (cairo_image_surface_get_data (if (or b&w? alpha-channel?)
                                                    (begin
                                                      (cairo_surface_flush s)
                                                      s)
                                                    (begin
                                                      (prep-alpha)
                                                      (cairo_surface_flush alpha-s)
                                                      alpha-s)))]
            [row-width (cairo_image_surface_get_stride s)]
            [A (a-index)])
        (for ([j (in-range y (min (+ y h) height))])
          (let ([row (* j row-width)])
            (for ([i (in-range x (min (+ x w) width))])
              (let ([p (* 4 (+ i (* j w)))]
                    [q (+ row (* i 4))])
                (bytes-set! bstr p (bytes-ref data (+ q A)))))))))
    
    (define/public (prep-alpha)
      (when (and (not b&w?)
                 (not alpha-channel?))
        (unless alpha-s-up-to-date?
          (unless alpha-s
            (set! alpha-s (cairo_image_surface_create CAIRO_FORMAT_ARGB32
                                                      width height)))
          (cairo_surface_flush s)
          (cairo_surface_flush alpha-s)
          (let ([data (cairo_image_surface_get_data s)]
                [alpha-data (cairo_image_surface_get_data alpha-s)]
                [row-width (cairo_image_surface_get_stride s)]
                [A (a-index)]
                [B (b-index)])
            (for ([j (in-range height)])
              (let ([row (* j row-width)])
                (for ([i (in-range width)])
                  (let ([q (+ row (* i 4))])
                    (let ([v (quotient
                              (+ (+ (bytes-ref data (+ q 1))
                                    (bytes-ref data (+ q 2)))
                                 (bytes-ref data (+ q B)))
                              3)])
                      (bytes-set! alpha-data (+ q A) (- 255 v))))))))
          (cairo_surface_mark_dirty alpha-s)
          (set! alpha-s-up-to-date? #t))))

    (define/public (transparent-white! s width height)
      (let ([bstr (cairo_image_surface_get_data s)]
            [row-width (cairo_image_surface_get_stride s)]
            [A (a-index)])
        (bytes-fill! bstr 255)
        (for ([j (in-range height)])
          (let ([row (* j row-width)])
            (for ([i (in-range width)])
              (bytes-set! bstr (+ A (+ row (* i 4))) 0))))))

    (define/public (set-alphas-as-mask x y w h bstr src-w src-A)
      (when (or b&w? (and (not b&w?) (not alpha-channel?)))
        (let ([data (cairo_image_surface_get_data s)]
              [row-width (cairo_image_surface_get_stride s)]
              [A (a-index)]
              [B (b-index)])
          (cairo_surface_flush s)
          (for ([j (in-range y (min (+ y h) height))])
            (let ([row (* j row-width)]
                  [src-row (* (- j y) src-w)])
              (for ([i (in-range x (min (+ x w) width))])
                (let* ([p (+ (* 4 (- i x)) src-row)]
                       [q (+ (* 4 i) row)])
                  (let* ([v (bytes-ref bstr (+ p src-A))]
                         [vv (- 255 v)])
                    (bytes-set! data (+ q B) vv)
                    (bytes-set! data (+ q 1) vv)
                    (bytes-set! data (+ q 2) vv)
                    (bytes-set! data (+ q A) (if b&w? v 255)))))))
          (cairo_surface_mark_dirty s))))

    ))
