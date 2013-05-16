#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "bstr.rkt"
         "../private/utils.rkt"
         "../private/libs.rkt")

(define-runtime-lib png-lib
  [(unix)
   ;; Most Linux distros supply "libpng12", while other Unix
   ;; variants often have just "libpng", etc.
   (let loop ([alts '(("libpng16" ("16" ""))
                      ("libpng15" ("15" ""))
                      ("libpng12" ("0" "")))])
     (cond
      [(null? alts) (ffi-lib "libpng")]
      [else (apply ffi-lib (car alts)
                   #:fail (lambda ()
                            (loop (cdr alts))))]))]
  [(macosx) (ffi-lib "libpng15.15.dylib")]
  [(windows)
   (ffi-lib "zlib1.dll")
   (ffi-lib "libpng14-14.dll")])

(define-ffi-definer define-png png-lib
  #:provide provide)

(define-png png_access_version_number (_fun -> _uint32))

;; We support version 1.2 and 1.4... and (WARNING!) we'll optimisitically
;;  assume that other versions are also ok
(define PNG_LIBPNG_VER_STRING (string->bytes/latin-1
                               (let ([v (png_access_version_number)])
                                 (format "~s.~s~a"
                                         (quotient v 10000)
                                         (quotient (remainder v 10000) 100)
                                         (if (zero? (remainder v 100))
                                             ""
                                             (format ".~a" (remainder v 100)))))))

(define _png_structp (_cpointer 'png_structp))
(define _png_infop (_cpointer 'png_infop))
(define _png_end_infop (_cpointer 'png_end_infop))
(define _png_size_t _long)

(define-cstruct _png_color_16 ([index _byte]
                               [red _uint16]
                               [green _uint16]
                               [blue _uint16]
                               [gray _uint16]))

(define-png png_create_read_struct
  (_fun _bytes
        _pointer
        (_fun _png_structp _string -> _void)
        (_fun _png_structp _string -> _void)
        -> _png_structp))

(define png_destroy_read_struct1
  (get-ffi-obj 'png_destroy_read_struct
               png-lib
               (_fun (_ptr i _png_structp)
                     (_pointer = #f)
                     (_pointer = #f)
                     -> _void)))
(define png_destroy_read_struct2
  (get-ffi-obj 'png_destroy_read_struct
               png-lib
               (_fun (_ptr i _png_structp)
                     (_ptr i _png_infop)
                     (_pointer = #f)
                     -> _void)))

(define-png png_create_write_struct
  (_fun _bytes
        _pointer
        (_fun _png_structp _string -> _void)
        (_fun _png_structp _string -> _void)
        -> _png_structp))
(define png_destroy_write_struct1
  (get-ffi-obj 'png_destroy_write_struct
               png-lib
               (_fun (_ptr i _png_structp)
                     (_pointer = #f)
                     -> _void)))
(define png_destroy_write_struct2
  (get-ffi-obj 'png_destroy_write_struct
               png-lib
               (_fun (_ptr i _png_structp)
                     (_ptr i _png_infop)
                     -> _void)))

(define-png png_create_info_struct (_fun _png_structp -> _png_infop))
(define-png png_read_info (_fun _png_structp _png_infop -> _void))
(define-png png_read_end (_fun _png_structp _png_infop -> _void))
(define-png png_write_info (_fun _png_structp _png_infop -> _void))

(define-png png_read_update_info (_fun _png_structp _png_infop -> _void))

(define-png png_get_IHDR (_fun _png_structp
                               _png_infop
                               (w : (_ptr o _uint32))
                               (h : (_ptr o _uint32))
                               (depth : (_ptr o _int))
                               (color-type : (_ptr o _int))
                               (interlace-type : (_ptr o _int))
                               (compression-type : (_ptr o _int))
                               (filter-type : (_ptr o _int))
                               -> _void
                               -> (values w h depth
                                          color-type
                                          interlace-type
                                          compression-type
                                          filter-type)))

(define-png png_set_IHDR (_fun _png_structp
                               _png_infop
                               _uint32
                               _uint32
                               _int
                               _int _int _int _int
                               -> _void))

(define current-fun-keep (make-parameter #f))

(define-png png_set_read_fn (_fun _png_structp
                                  _pointer
                                  (_fun #:keep (lambda (v) ((current-fun-keep) v))
                                        _png_structp
                                        _pointer
                                        _png_size_t
                                        -> _void)
                                  -> _void))
(define-png png_set_write_fn (_fun _png_structp
                                   _pointer
                                   (_fun #:keep (lambda (v) ((current-fun-keep) v))
                                         _png_structp
                                         _pointer
                                         _png_size_t
                                         -> _void)
                                   (_fun #:keep (lambda (v) ((current-fun-keep) v))
                                         _png_structp
                                         -> _void)
                                   -> _void))
(define-png png_get_io_ptr (_fun _png_structp -> _pointer))

(define-png png_get_rowbytes (_fun _png_structp _png_infop -> _uint32))
(define-png png_read_rows (_fun _png_structp _pointer #;(_vector i _bytes) _pointer _uint32 -> _void))
(define-png png_write_image (_fun _png_structp _pointer #;(_vector i _bytes) -> _void))

(define-png png_write_end (_fun _png_structp _png_infop -> _void))

(define-png png_get_valid (_fun _png_structp _png_infop _uint32 -> _uint32))
(define-png png_get_bKGD (_fun _png_structp _png_infop (p : (_ptr o _png_color_16)) -> (r : _bool) -> (and r p)))
(define-png png_set_background (_fun _png_structp _png_color_16-pointer _int _int _double* -> _bool))
(define-png png_get_gAMA (_fun _png_structp _png_infop (g : (_ptr o _double))
                               -> (ok? : _bool)
                               -> (and ok? g)))
(define-png png_set_gamma (_fun _png_structp _double* _double* -> _void))
(define-png png_set_filler (_fun _png_structp _uint32 _int -> _void))

(define-png png_set_invert_alpha (_fun _png_structp -> _void))
(define-png png_set_palette_to_rgb (_fun _png_structp -> _void))
(define-png png_set_gray_to_rgb (_fun _png_structp -> _void))
(define-png png_set_tRNS_to_alpha (_fun _png_structp -> _void))
(define-png png_set_strip_16 (_fun _png_structp -> _void))
(define-png png_set_strip_alpha (_fun _png_structp -> _void))
(define-png png_set_gray_1_2_4_to_8 (_fun _png_structp -> _void)
  #:fail (lambda () #f))
(define-png png_set_expand_gray_1_2_4_to_8 (_fun _png_structp -> _void)
  #:fail (lambda () #f))
(define-png png_set_interlace_handling (_fun _png_structp -> _int))

(define/provide PNG_COLOR_MASK_PALETTE    1)
(define/provide PNG_COLOR_MASK_COLOR      2)
(define/provide PNG_COLOR_MASK_ALPHA      4)

(define/provide PNG_COLOR_TYPE_GRAY 0)
(define/provide PNG_COLOR_TYPE_PALETTE  (bitwise-ior PNG_COLOR_MASK_COLOR PNG_COLOR_MASK_PALETTE))
(define/provide PNG_COLOR_TYPE_RGB      PNG_COLOR_MASK_COLOR)
(define/provide PNG_COLOR_TYPE_RGB_ALPHA  (bitwise-ior PNG_COLOR_MASK_COLOR PNG_COLOR_MASK_ALPHA))
(define/provide PNG_COLOR_TYPE_GRAY_ALPHA PNG_COLOR_MASK_ALPHA)

(define/provide PNG_INTERLACE_NONE        0)
(define/provide PNG_INTERLACE_ADAM7       1)

(define/provide PNG_FILTER_TYPE_BASE      0)
(define/provide PNG_INTRAPIXEL_DIFFERENCING 64)
(define/provide PNG_FILTER_TYPE_DEFAULT   PNG_FILTER_TYPE_BASE)

(define/provide PNG_COMPRESSION_TYPE_BASE 0)
(define/provide PNG_COMPRESSION_TYPE_DEFAULT PNG_COMPRESSION_TYPE_BASE)

(define/provide PNG_BACKGROUND_GAMMA_UNKNOWN 0)
(define/provide PNG_BACKGROUND_GAMMA_SCREEN  1)
(define/provide PNG_BACKGROUND_GAMMA_FILE    2)
(define/provide PNG_BACKGROUND_GAMMA_UNIQUE  3)

(define/provide PNG_INFO_tRNS #x0010)

(define PNG_FILLER_BEFORE 0)
(define PNG_FILLER_AFTER  1)

;; ----------------------------------------
;; Reading

(provide create-png-reader
         read-png
         destroy-png-reader)

(define-struct reader ([png #:mutable] info ib num-passes w h))

(define (error-esc v s)
  (error 'png "~a" s))

(define (read-png-bytes png p len)
  (let ([bstr (scheme_make_sized_byte_string p len 0)])
    (read-bytes! bstr (car (ptr-ref (png_get_io_ptr png) _scheme)))))

(define free-cell ((deallocator) free-immobile-cell))
(define make-cell ((allocator free-cell) malloc-immobile-cell))

(define (create-png-reader in keep-alpha? bg-rgb)
  (let* ([png (png_create_read_struct PNG_LIBPNG_VER_STRING #f error-esc void)]
         [info (png_create_info_struct png)]
         [funs (box null)]
         [ib (make-cell (cons in funs))])
    (parameterize ([current-fun-keep (lambda (v)
                                       (set-box! funs (cons v (unbox funs))))])
      (png_set_read_fn png ib read-png-bytes))
    (png_read_info png info)
    (let-values ([(w h depth color-type
                     interlace-type compression-type filter-type)
                  (png_get_IHDR png info)])
      (let* ([tRNS? (positive? (png_get_valid png info PNG_INFO_tRNS))]
             [b&w? (and (= depth 1)
                        (= color-type PNG_COLOR_TYPE_GRAY)
                        (not tRNS?))]
             [alpha? (and keep-alpha?
                          (not b&w?)
                          (or tRNS?
                              (positive? (bitwise-ior color-type PNG_COLOR_MASK_ALPHA))))])
        (unless b&w?
          ;; Normalize formal of returned rows:
          (when (= color-type PNG_COLOR_TYPE_PALETTE)
            (png_set_palette_to_rgb png))
          (when (or (= color-type PNG_COLOR_TYPE_GRAY)
                    (= color-type PNG_COLOR_TYPE_GRAY_ALPHA))
            (png_set_gray_to_rgb png))
          (when tRNS?
            (png_set_tRNS_to_alpha png))
          (when (= depth 16)
            (png_set_strip_16 png))
          ;; Expand grayscale images to the full 8 bits from 1, 2, or 4 bits/pixel
          ((or png_set_gray_1_2_4_to_8 png_set_expand_gray_1_2_4_to_8) png))
        (unless (or alpha? b&w?)
          ;; Set the background color to draw transparent and alpha images over.
          (let* ([deep (lambda (n)
                         (if (= depth 16)
                             (+ n (arithmetic-shift n 8))
                             n))]
                 [bg (make-png_color_16 0 (deep 255) (deep 255) (deep 255) (deep 255))])
            (cond
             [bg-rgb (set-png_color_16-red! bg (deep (car bg-rgb)))
                     (set-png_color_16-green! bg (deep (cadr bg-rgb)))
                     (set-png_color_16-blue! bg (deep (caddr bg-rgb)))
                     (set-png_color_16-gray! bg (deep (floor (/ (apply + bg-rgb) 3))))]
             [else (let ([c (png_get_bKGD png info)])
                     (when c
                       (memcpy bg c (ctype-sizeof _png_color_16))))])
            (png_set_background png bg
                                (if bg-rgb
                                    PNG_BACKGROUND_GAMMA_SCREEN
                                    PNG_BACKGROUND_GAMMA_FILE)
                                0 1.0)))
        (let ([gamma (png_get_gAMA png info)])
          (when gamma
            (let* ([s (getenv "SCREEN_GAMMA")]
                   [screen-gamma (and s (string->number s))])
              (png_set_gamma png (if (and (real? screen-gamma)
                                          (<= 0.0 screen-gamma 10.0))
                                     screen-gamma
                                     (case (system-type)
                                       [(macosx) 1.7]
                                       [else 2.0]))
                             gamma))))
        (cond
         [alpha?
          ;; Make sure there's an alpha or filler byte (before each RGB triplet):
          (png_set_filler png 255 PNG_FILLER_AFTER)]
         [tRNS?
          ;; Make sure there's no alpha channel:
          (png_set_strip_alpha png)])
        (let ([num-passes (png_set_interlace_handling png)])
          (png_read_update_info png info)
          (values (make-reader png info ib num-passes w h)
                  w h
                  b&w?
                  alpha?))))))

(define (malloc-rows h row-bytes)
  (let* ([align (lambda (v) (if (positive? (remainder v 8))
                                (+ v (- 8 (remainder v 8)))
                                v))]
         [table-size (align (* h (ctype-sizeof _pointer)))]
         [row-size (align row-bytes)]
         [memory (malloc (+ table-size (* row-size h))
                         'atomic-interior)]
         [rows memory])
    (for ([i (in-range h)])
      (ptr-set! rows _pointer i (ptr-add memory (+ table-size (* i row-size)))))
    rows))

(define (read-png reader)
  (let* ([row-bytes (png_get_rowbytes (reader-png reader) (reader-info reader))]
         [rows (malloc-rows (reader-h reader) row-bytes)])
    (for ([i (in-range (reader-num-passes reader))])
      (png_read_rows (reader-png reader) rows #f (reader-h reader)))
    (png_read_end (reader-png reader) (reader-info reader))
    (list->vector
     (for/list ([i (in-range (reader-h reader))])
       (let ([p (ptr-ref rows _gcpointer i)])
         (scheme_make_sized_byte_string p row-bytes 1))))))

(define (destroy-png-reader reader)
  (when (reader-png reader)
    (png_destroy_read_struct2 (reader-png reader)
                              (reader-info reader))
    (free-cell (reader-ib reader))
    (set-reader-png! reader #f)))

;; ----------------------------------------
;; Writing

(provide create-png-writer
         write-png
         destroy-png-writer)

(define-struct writer (png info ob))

(define (write-png-bytes png p len)
  (let ([bstr (scheme_make_sized_byte_string p len 0)])
    (write-bytes bstr (car (ptr-ref (png_get_io_ptr png) _scheme)))))

(define (flush-png-bytes png)
  (flush-output (car (ptr-ref (png_get_io_ptr png) _scheme))))

(define (create-png-writer out w h b&w? alpha?)
  (let* ([png (png_create_write_struct PNG_LIBPNG_VER_STRING #f error-esc void)]
         [info (png_create_info_struct png)]
         [funs (box null)]
         [ob (make-cell (cons out funs))])
    (parameterize ([current-fun-keep (lambda (v)
                                       (set-box! funs (cons v (unbox funs))))])
      (png_set_write_fn png ob write-png-bytes flush-png-bytes))
    (png_set_IHDR png info w h (if b&w? 1 8)
                  (cond
                   [b&w? PNG_COLOR_TYPE_GRAY]
                   [alpha? PNG_COLOR_TYPE_RGB_ALPHA]
                   [else PNG_COLOR_TYPE_RGB])
                  PNG_INTERLACE_NONE PNG_COMPRESSION_TYPE_DEFAULT
                  PNG_FILTER_TYPE_DEFAULT)
    (png_write_info png info)
    (make-writer png info ob)))

(define (write-png writer vector-of-rows)
  (if (zero? (vector-length vector-of-rows))
      (png_write_image (writer-png writer) #f)
      (let* ([h (vector-length vector-of-rows)]
             [w (bytes-length (vector-ref vector-of-rows 0))]
             [rows (malloc-rows h w)])
        (for/list ([i (in-range h)])
          (memcpy (ptr-ref rows _gcpointer i)
                  (vector-ref vector-of-rows i)
                  w))
        (png_write_image (writer-png writer) rows)))
  (png_write_end (writer-png writer) (writer-info writer)))

(define (destroy-png-writer writer)
  (png_destroy_write_struct2 (writer-png writer)
                             (writer-info writer))
  (free-cell (writer-ob writer)))
