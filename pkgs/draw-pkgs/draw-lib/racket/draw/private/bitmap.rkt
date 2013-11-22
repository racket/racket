#lang racket/base
(require racket/class
         racket/unsafe/ops
         file/convertible
         (for-syntax racket/base)
         "syntax.rkt"
         "hold.rkt"
         "../unsafe/bstr.rkt"
         "../unsafe/cairo.rkt"
         "../unsafe/png.rkt"
         "../unsafe/jpeg.rkt"
         "../xbm.rkt"
         "../xpm.rkt"
         "../bmp.rkt"
         "../gif.rkt"
         "local.rkt"
         "color.rkt"
         "lock.rkt")

(provide bitmap%
         make-bitmap
         make-platform-bitmap
         read-bitmap
         make-monochrome-bitmap
         -bitmap-dc%
         (protect-out make-alternate-bitmap-kind
                      build-cairo-surface
                      quartz-bitmap%
                      win32-no-hwnd-bitmap%
                      install-bitmap-dc-class!))

(define -bitmap-dc% #f)
(define (install-bitmap-dc-class! v) (set! -bitmap-dc% v))

;; FIXME: there must be some way to abstract over all many of the
;; ARGB/RGBA/BGRA iterations.

(define-struct alternate-bitmap-kind (width height))

(define-local-member-name
  get-alphas-as-mask
  set-alphas-as-mask)

(define (bitmap-file-kind-symbol? s)
  (memq s '(unknown unknown/mask unknown/alpha
                    gif gif/mask gif/alpha
                    jpeg jpeg/alpha
                    png png/mask png/alpha
                    xbm xbm/alpha
                    xpm xpm/alpha
                    bmp bmp/alpha
                    pict)))

(define (bitmap-save-kind-symbol? s)
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

(define mult-table #f)
(define unmult-table #f)

(define (get-mult-table)
  (atomically
   (unless mult-table
     (set! mult-table (make-bytes (* 256 256)))
     (for ([a (in-range 256)])
       (for ([v (in-range 256)])
         (bytes-set! mult-table
                     (fx+ (fx* a 256) v)
                     (unsafe-fl->fx
                      (unsafe-flround
                       (unsafe-fl/
                        (unsafe-fx->fl (fx* a v))
                        255.0))))))))
  mult-table)

(define (get-unmult-table)
  (atomically
   (unless unmult-table
     (set! unmult-table (make-bytes (* 256 256)))
     (for ([a (in-range 256)])
       (for ([v (in-range 256)])
         (bytes-set! unmult-table
                     (fx+ (fx* a 256) v)
                     (if (unsafe-fx<= a v)
                         255
                         (unsafe-fl->fx
                          (unsafe-flround
                           (unsafe-fl/
                            (unsafe-fx->fl (fx* 255 v))
                            (unsafe-fx->fl a))))))))))
  unmult-table)

(define (alpha-mult al v)
  (unsafe-fl->fx
   (unsafe-flround
    (unsafe-fl/
     (unsafe-fx->fl (fx* al v))
     255.0))))

(define (alpha-unmult al v)
  (if (zero? al)
      255
      (unsafe-fxmin 255
                    (unsafe-fl->fx
                     (unsafe-flround
                      (unsafe-fl/
                       (unsafe-fx->fl (fx* 255 v))
                       (unsafe-fx->fl al)))))))


(define png-convertible<%>
  (interface* ()
              ([prop:convertible
                (lambda (bm format default)
                  (case format
                    [(png-bytes)
                     (let ([s (open-output-bytes)])
                       (send bm save-file s 'png)
                       (get-output-bytes s))]
                    [else default]))])))

(define (get-empty-surface)
  (cairo_image_surface_create CAIRO_FORMAT_ARGB32 1 1))

(define bitmap%
  (class* object% (png-convertible<%>)

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

    (define-values (alt? width height b&w? alpha-channel? s loaded-mask)
      (case-args
       args
       [([alternate-bitmap-kind? a])
        (values #t
                (alternate-bitmap-kind-width a)
                (alternate-bitmap-kind-height a)
                #f #t #f #f)]
       [([exact-positive-integer? w]
         [exact-positive-integer? h]
         [any? [b&w? #f]]
         [any? [alpha? #f]])
        (values
         #f
         w
         h
         (and b&w? #t)
         (and alpha? (not b&w?))
         (let ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 (max w 1) (max h 1))])
           (cairo_surface_flush s)
           (cond
            [b&w?
             ;; Init transparent white:
             (transparent-white! s w h)]
            [alpha?
             ;; Init transparent:
             (bytes-fill! (cairo_image_surface_get_data s) 0)]
            [else
             ;; Init all white, 255 alpha:
             (bytes-fill! (cairo_image_surface_get_data s) 255)])
           (cairo_surface_mark_dirty s)
           s)
         #f)]
       [([(make-alts path-string? input-port?) filename]
         [bitmap-file-kind-symbol? [kind 'unknown]]
         [(make-or-false color%) [bg-color #f]]
         [any? [complain-on-failure? #f]])
        (let-values ([(s b&w?) (do-load-bitmap filename kind bg-color complain-on-failure?)]
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
                (values #f
                        (cairo_image_surface_get_width s)
                        (cairo_image_surface_get_height s)
                        b&w?
                        (and alpha? (not b&w?))
                        s
                        mask-bm)
                (values #f 0 0 #f #f #f #f))))]
       [([bytes? bstr]
         [exact-positive-integer? w]
         [exact-positive-integer? h])
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
            (values #f w h #t #f s #f)))]
       (init-name 'bitmap%)))

    ;; Use for non-alpha color bitmaps when they are used as a mask:
    (define alpha-s #f)
    (define alpha-s-up-to-date? #f)
    (define/public (drop-alpha-s)
      (set! alpha-s-up-to-date? #f)
      (when alpha-s
        (let ([s2 alpha-s])
          (set! alpha-s #f)
          (destroy s2))))

    ;; Claim memory proportional to the size of the bitmap, which
    ;; helps the GC see that we're using that much memory:
    (define shadow (make-phantom-bytes (* width height 4)))

    (def/public (get-width) (max 1 width))
    (def/public (get-height) (max 1 height))
    (def/public (get-depth) (if b&w? 1 32))
    (def/public (is-color?) (not b&w?))
    (def/public (has-alpha-channel?) (and alpha-channel? #t))

    (define/private (check-alternate who)
      (when alt?
        (raise-mismatch-error (method-name 'bitmap% who)
                              "not available in a canvas-compatible bitmap: "
                              this)))

    (def/public (get-loaded-mask) loaded-mask)
    (def/public (set-loaded-mask [(make-or-false bitmap%) m]) (set! loaded-mask m))

    (define/public (draw-bitmap-to cr sx sy dx dy w h alpha clipping)
      #f)

    (define/public (release-bitmap-storage)
      (drop-alpha-s)
      (when s
        (let ([s2 s])
          (set! s #f)
          (destroy s2)
          (set! shadow #f))))

    (define/public (get-bitmap-gl-context)
      #f)

    (define/public (make-dc) (make-object -bitmap-dc% this))

    (define/public (load-file in
                              [kind 'unknown]
                              [bg #f]
                              [complain-on-failure? #f])
      (check-alternate 'load-file)
      (release-bitmap-storage)
      (set!-values (s b&w?) (do-load-bitmap in kind bg complain-on-failure?))
      (set! width (if s (cairo_image_surface_get_width s) 0))
      (set! height (if s (cairo_image_surface_get_height s) 0))
      (set! shadow (make-phantom-bytes (* width height 4)))
      (and s #t))

    (define/private (do-load-bitmap in kind bg complain-on-failure?)
      (if (path-string? in)
          (with-handlers ([exn:fail? (lambda (exn)
                                       (if complain-on-failure?
                                           (raise exn)
                                           (values #f #f)))])
            (call-with-input-file*
             in
             (lambda (in) (do-load-bitmap in kind bg #f))))
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
                                bg
                                complain-on-failure?)]
               [(starts? #"\xFF\xD8\xFF")
                (do-load-bitmap in 'jpeg bg complain-on-failure?)]
               [(starts? #"GIF8")
                (do-load-bitmap in 'gif bg complain-on-failure?)]
               [(starts? #"BM")
                (do-load-bitmap in 'bmp bg complain-on-failure?)]
               [(starts? #"#define")
                (do-load-bitmap in 'xbm bg complain-on-failure?)]
               [(starts? #"/* XPM */")
                (do-load-bitmap in 'xpm bg complain-on-failure?)]
               [else
                ;; unrecognized file type; try to parse as XBM
                (do-load-bitmap in 'xbm bg complain-on-failure?)]))]
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
                                               (memq kind '(png/mask png/alpha))
                                               (and bg
                                                    (list (send bg red)
                                                          (send bg green)
                                                          (send bg blue))))])
              (let ([rows (read-png r)])
                (destroy-png-reader r)
                (let* ([s (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h)]
                       [pre? (and alpha? (eq? kind 'png/alpha))])
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
                     [pre? (and alpha? (eq? kind 'gif/alpha))]
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
             [dest-row-width (cairo_image_surface_get_stride s)]
             [m (and pre? (get-mult-table))])
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
                           [premult (lambda (al v)
                                      (if m
                                          (unsafe-bytes-ref m (fx+ (fx* al 256) v))
                                          v))])
                      (unsafe-bytes-set! dest (fx+ pos A) al)
                      (unsafe-bytes-set! dest (fx+ pos R) (premult al (unsafe-bytes-ref r spos)))
                      (unsafe-bytes-set! dest (fx+ pos G) (premult al (unsafe-bytes-ref r (fx+ spos 1))))
                      (unsafe-bytes-set! dest (fx+ pos B) (premult al (unsafe-bytes-ref r (fx+ spos 2))))))))))
        (cairo_surface_mark_dirty s)))

    (define/private (call-with-alt-bitmap x y w h proc)
      (let* ([bm (make-object bitmap% w h #f #t)]
             [cr (cairo_create (send bm get-cairo-surface))])
        (let ([p (cairo_get_source cr)])
          (cairo_pattern_reference p)
          (cairo_set_source_surface cr (get-cairo-surface) (- x) (- y))
          (let ([sc (get-cairo-device-scale)])
            (unless (= sc 1)
              (let ([m (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0)])
                (cairo_matrix_init_translate m 0 0)
                (cairo_matrix_scale m sc sc)
                (cairo_matrix_translate m x y)
                (cairo_pattern_set_matrix (cairo_get_source cr) m))))
          (cairo_new_path cr)
          (cairo_rectangle cr 0 0 w h)
          (cairo_fill cr)
          (cairo_set_source cr p)
          (cairo_pattern_destroy p))
        (cairo_destroy cr)
        (proc bm)
        (send bm release-bitmap-storage)))

    (define/public (save-file out [kind 'unknown] [quality 75])
      (and (ok?)
           (begin
             (if alt?
                 (call-with-alt-bitmap
                  0 0 width height
                  (lambda (bm)
                    (send bm save-file out kind quality)))
                 (do-save-file out kind quality))
             #t)))

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
                          (for/fold ([v 0]) ([k (in-range 8)])
                            (if ((+ (* 8 bi) k) . < . width)
                                (if (zero? (bytes-ref data (+ src 3 (* 4 k))))
                                    (bitwise-ior v (unsafe-fxrshift 128 k))
                                    v)
                                v)))))))
                 (let ([w (create-png-writer out width height #t #f)])
                   (write-png w rows)
                   (destroy-png-writer w)))]
              [else #;(and (not alpha-channel?)
                           loaded-mask
                           (= width (send loaded-mask get-width))
                           (= height (send loaded-mask get-height)))
               (let ([bstr (make-bytes (* width height 4))])
                 (get-argb-pixels 0 0 width height bstr)
                 (when loaded-mask
                   (send loaded-mask get-argb-pixels 0 0 width height bstr #t))
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
              #;
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
                     (let-values ([(samps bstr) (create-jpeg-sample-array c (* width 3))]
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

    (define/public (get-cairo-surface) (or s (get-empty-surface)))
    (define/public (get-cairo-target-surface) (get-cairo-surface))
    (define/public (get-cairo-alpha-surface)
      (or (if (or b&w? alpha-channel?)
              s
              (begin
                (prep-alpha)
                alpha-s))
          (get-empty-surface)))

    (define/public (get-cairo-device-scale) 1.0)

    (define/public (get-backing-scale) (get-cairo-device-scale))

    (define/public (get-handle) s)

    (define/public (get-argb-pixels x y w h bstr
                                    [get-alpha? #f]
                                    [pre-mult? #f])
      (unless ((bytes-length bstr) . >=  . (* w h 4))
        (raise-mismatch-error (method-name 'bitmap% 'get-argb-pixels)
                              "byte string is too short: "
                              bstr))
      (when (ok?)
        (if alt?
            (call-with-alt-bitmap
             x y w h
             (lambda (bm) (send bm get-argb-pixels 0 0 w h bstr get-alpha? pre-mult?)))
            (do-get-argb-pixels x y w h bstr get-alpha? pre-mult?))))

    (define/private (do-get-argb-pixels x y w h bstr get-alpha? pre-mult?)
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
      (when (not get-alpha?)
        (let-values ([(A R G B) (argb-indices)])
          (cairo_surface_flush s)
          (let ([data (cairo_image_surface_get_data s)]
                [row-width (cairo_image_surface_get_stride s)]
                [um (and (or (and alpha-channel? (not pre-mult?)) b&w?)
                         (get-unmult-table))]
                [set-alpha? alpha-channel?])
            (let ([w2 (+ x (min (- width x) w))])
              (for* ([j (in-range y (min (+ y h) height))])
                (let* ([row (* j row-width)]
                       [p (* 4 (* (- j y) w))]
                       [ri-start (+ row (* 4 x))]
                       [ri-end (+ row (* 4 w2))]
                       [pi-start p]
                       [pi-end (+ p (* 4 (- w2 x)))])
                  (for ([ri (in-range ri-start ri-end 4)]
                        [pi (in-range pi-start pi-end 4)])
                    (let ([a (unsafe-bytes-ref data (+ ri A))])
                      (let-syntax ([unmult
                                    ;; Defined as a macro to copy the
                                    ;; `unsafe-bytes-ref' to each branch,
                                    ;; instead of binding a local variable
                                    (syntax-rules ()
                                      [(_ v)
                                       (if um
                                           (unsafe-bytes-ref um (fx+ (fx* a 256) v))
                                           v)])])
                        (when set-alpha?
                          (unsafe-bytes-set! bstr pi a))
                        (unsafe-bytes-set! bstr (+ pi 1) (unmult (unsafe-bytes-ref data (+ ri R))))
                        (unsafe-bytes-set! bstr (+ pi 2) (unmult (unsafe-bytes-ref data (+ ri G))))
                        (unsafe-bytes-set! bstr (+ pi 3) (unmult (unsafe-bytes-ref data (+ ri B)))))))))))))
      (cond
       [get-alpha?
        (get-alphas-as-mask x y w h bstr)]
       [(and (not get-alpha?) (not alpha-channel?))
        ;; For non-alpha mode and no alpha channel; fill in 255s for alpha:
        (for ([j (in-range 0 (min h (- height y)))])
          (let ([row (* j (* 4 w))])
            (for ([i (in-range 0 (min w (- width x)))])
              (let ([p (+ (* 4 i) row)])
                (bytes-set! bstr p 255)))))]))

    (define/public (set-argb-pixels x y w h bstr
                                    [set-alpha? #f]
                                    [pre-mult? #f])
      (unless ((bytes-length bstr) . >=  . (* w h 4))
        (raise-mismatch-error (method-name 'bitmap% 'set-argb-pixels)
                              "byte string is too short: "
                              bstr))
      (check-alternate 'set-argb-pixels)
      (when (ok?)
        ;; Set pixels:
        (let-values ([(A R G B) (argb-indices)])
          (when (not set-alpha?)
            (cairo_surface_flush s)
            (let ([data (cairo_image_surface_get_data s)]
                  [row-width (cairo_image_surface_get_stride s)]
                  [m (and (not pre-mult?) (get-mult-table))])
              (define-syntax-rule (set-loop body)
                (let ([w2 (+ x (min (- width x) w))])
                  (for ([j (in-range y (min (+ y h) height))]
                        [dj (in-naturals)])
                    (let ([row (* j row-width)]
                          [p (* 4 (* dj w))])
                      (for ([i (in-range x w2)])
                        (let* ([4i (unsafe-fx* 4 i)]
                               [pi (unsafe-fx+ p (unsafe-fx* 4 (unsafe-fx- i x)))]
                               [ri (unsafe-fx+ row 4i)])
                          (body pi ri)))))))
              (cond
               [b&w?
                (set-loop
                 (lambda (pi ri)
                   (let ([v (if (and (= (unsafe-bytes-ref bstr (+ pi 1)) 255)
                                     (= (unsafe-bytes-ref bstr (+ pi 2)) 255)
                                     (= (unsafe-bytes-ref bstr (+ pi 3)) 255))
                                255
                                0)])
                     (unsafe-bytes-set! data (unsafe-fx+ ri A) (- 255 v))
                     (unsafe-bytes-set! data (unsafe-fx+ ri 1) v)
                     (unsafe-bytes-set! data (unsafe-fx+ ri 2) v)
                     (unsafe-bytes-set! data (unsafe-fx+ ri B) v))))]
               [alpha-channel?
                (define-syntax-rule (alpha-set-loop pm)
                  (set-loop
                   (lambda (pi ri)
                     (let ([a (bytes-ref bstr pi)])
                       (unsafe-bytes-set! data (unsafe-fx+ ri A) a)
                       (unsafe-bytes-set! data (unsafe-fx+ ri R)
                                          (pm a (unsafe-bytes-ref bstr (unsafe-fx+ pi 1))))
                       (unsafe-bytes-set! data (unsafe-fx+ ri G)
                                          (pm a (unsafe-bytes-ref bstr (unsafe-fx+ pi 2))))
                       (unsafe-bytes-set! data (unsafe-fx+ ri B)
                                          (pm a (unsafe-bytes-ref bstr (unsafe-fx+ pi 3))))))))
                (if m
                    (alpha-set-loop (lambda (a v)
                                      (unsafe-bytes-ref m (unsafe-fx+ (unsafe-fx* a 256) v))))
                    (alpha-set-loop (lambda (a v) (unsafe-fxmin a v))))]
               [else
                (set-loop
                 (lambda (pi ri)
                   (unsafe-bytes-set! data (unsafe-fx+ ri R)
                                      (unsafe-bytes-ref bstr (unsafe-fx+ pi 1)))
                   (unsafe-bytes-set! data (unsafe-fx+ ri G)
                                      (unsafe-bytes-ref bstr (unsafe-fx+ pi 2)))
                   (unsafe-bytes-set! data (unsafe-fx+ ri B)
                                      (unsafe-bytes-ref bstr (unsafe-fx+ pi 3)))))]))
            (cairo_surface_mark_dirty s)))
        (cond
         [(and set-alpha?
               (not alpha-channel?))
          ;; Set alphas:
          (set-alphas-as-mask x y w h bstr (* 4 w) 0)])
        (drop-alpha-s)))

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
              (let ([p (* 4 (+ (- i x) (* (- j y) w)))]
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

(define/top (make-bitmap [exact-positive-integer? w]
                         [exact-positive-integer? h]
                         [any? [alpha? #t]])
  (make-object bitmap% w h #f alpha?))

(define/top (read-bitmap [(make-alts path-string? input-port?) filename]
                         [bitmap-file-kind-symbol? [kind 'unknown/alpha]]
                         [(make-or-false color%) [bg-color #f]]
                         [any? [complain-on-failure? #t]])
  (make-object bitmap% filename kind bg-color complain-on-failure?))

(define/top (make-monochrome-bitmap [exact-positive-integer? w]
                                    [exact-positive-integer? h]
                                    [(make-or-false bytes?) [bits #f]])
  (if bits
      (make-object bitmap% bits w h)
      (make-object bitmap% w h #t)))

(define/top (make-platform-bitmap [exact-positive-integer? w]
                                  [exact-positive-integer? h])
  (case (system-type)
    [(macosx) (make-object quartz-bitmap% w h)]
    [(windows) (make-object win32-no-hwnd-bitmap% w h)]
    [(unix) (make-bitmap w h)]))

(define-local-member-name build-cairo-surface)
(define win32-no-hwnd-bitmap%
  (class bitmap%
    (init w h)
    (super-make-object (make-alternate-bitmap-kind w h))

    (define s (build-cairo-surface w h))
    ;; erase the bitmap
    (let ([cr (cairo_create s)])
      (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
      (cairo_paint cr)
      (cairo_destroy cr))

    (define/public (build-cairo-surface w h)
      (cairo_win32_surface_create_with_dib CAIRO_FORMAT_RGB24 w h))

    (define/override (ok?) #t)
    (define/override (is-color?) #t)
    (define/override (has-alpha-channel?) #f)

    (define/override (get-cairo-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (set! s #f)))))

(define quartz-bitmap%
  (class bitmap%
    (init w h [with-alpha? #t] [resolution 1.0] [dest-cg #f])
    (super-make-object (make-alternate-bitmap-kind w h))
    
    (define cocoa-resolution resolution)
    
    (define/override (get-cairo-device-scale)
      cocoa-resolution)
    
    (define s
      (let* ([sw (inexact->exact
                  (ceiling
                   (* cocoa-resolution w)))]
             [sh (inexact->exact
                  (ceiling
                   (* cocoa-resolution h)))]
             [s (if dest-cg
                    (cairo_quartz_surface_create_for_cg_context dest-cg sw sh)
                    (cairo_quartz_surface_create (if with-alpha?
                                                     CAIRO_FORMAT_ARGB32
                                                     CAIRO_FORMAT_RGB24)
                                                 sw
                                                 sh))])
        ;; initialize bitmap to empty - needed?
        (let ([cr (cairo_create s)])
          (cairo_set_operator cr (if with-alpha?
                                     CAIRO_OPERATOR_CLEAR
                                     CAIRO_OPERATOR_SOURCE))
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_paint cr)
          (cairo_destroy cr))
        s))

    (define/override (ok?) (and s #t))

    (define/override (is-color?) #t)

    (define has-alpha? with-alpha?)
    (define/override (has-alpha-channel?) has-alpha?)

    (define/override (get-cairo-surface) s)
    (define/override (get-cairo-alpha-surface)
      (if has-alpha?
          s
          (super get-cairo-alpha-surface)))

    (define/override (release-bitmap-storage)
      (atomically
       (when s
         (cairo_surface_destroy s)
         (set! s #f))))))
