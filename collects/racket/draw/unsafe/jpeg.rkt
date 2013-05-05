#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "bstr.rkt"
         "../private/utils.rkt"
         "../private/libs.rkt")

(define-runtime-lib jpeg-lib
  [(unix) (ffi-lib "libjpeg" '("62" "8" "9" ""))]
  [(macosx) 
   ;; for PPC, it's actually version 8!
   (ffi-lib "libjpeg.62.dylib")]
  [(win32) (ffi-lib "libjpeg-7.dll")]
  [(win64) (ffi-lib "libjpeg-8.dll")])

(define-ffi-definer define-jpeg jpeg-lib
  #:provide provide)
(define-ffi-definer define-jpeg/private jpeg-lib)

(define _j_common_ptr _pointer)
(define _size_t _intptr)
(define _JDIMENSION _uint)
(define _J_COLOR_SPACE _int)
(define _J_DCT_METHOD _int)
(define _J_DITHER_MODE _int)
(define _J_COLOR_TRANSFORM _int)

(define _jbool (if win64? 
                   (make-ctype _byte
                               (lambda (v) (if v 1 0))
                               (lambda (v) (not (zero? v))))
                   _bool))
(define-enum
  0
  JCS_UNKNOWN
  JCS_GRAYSCALE
  JCS_RGB
  JCS_YCbCr
  JCS_CMYK
  JCS_YCCK)

(define _pool_id _int)
(define JPOOL_PERMANENT 0)
(define JPOOL_IMAGE 1)
(define JPOOL_NUMPOOLS 2)
(define JMSG_LENGTH_MAX  200)

(define-cstruct _jpeg_error_mgr ([error_exit _fpointer] ; (_fun _j_common_ptr -> _void)
                                 [emit_message _pointer]
                                 [output_message _pointer]
                                 [format_message (_fun _j_common_ptr _pointer -> _void)]
                                 ;; and more, including an inline character
                                 ;; array that is a pain to handle here
                                 ))
(define sizeof_jpeg_error_mgr 1024)

(define-cstruct _jpeg_memory_mgr ([alloc_small (_fun _j_common_ptr _pool_id _size_t -> _pointer)]
                                  [alloc_large (_fun _j_common_ptr _pool_id _size_t -> _pointer)]
                                  [alloc_sarray (_fun _j_common_ptr _pool_id _JDIMENSION _JDIMENSION -> _pointer)]
                                  ;; and more
                                  ))

(define-cstruct _jpeg_any_struct ([err _jpeg_error_mgr-pointer]))

(define (error-exit m) 
  (let ([bstr (make-bytes JMSG_LENGTH_MAX)])
    ((jpeg_error_mgr-format_message
      (jpeg_any_struct-err (cast m _pointer _jpeg_any_struct-pointer)))
     m
     bstr)
    (error 'jpeg "~a" (bytes->string/latin-1 (subbytes bstr 0 (let loop ([i 0])
                                                                (if (zero? (bytes-ref bstr i))
                                                                    i
                                                                    (loop (add1 i)))))))))

(define-jpeg/private jpeg_std_error (_fun _jpeg_error_mgr-pointer -> _jpeg_error_mgr-pointer))

(define-jpeg/private jpeg_CreateDecompress/test (_fun _pointer _int _int -> _void)
  #:c-id jpeg_CreateDecompress)

;; jpeglib offers no way to get the library version number dynamically,
;; so we hack it by intercepting an error from jpeg_CreateDecompress:
(define JPEG_LIB_VERSION
  (let ([dummy-size 4096])
    (let ([m (cast (malloc dummy-size 'raw) _pointer _jpeg_any_struct-pointer)]
          [e (cast (malloc sizeof_jpeg_error_mgr 'raw) _pointer _jpeg_error_mgr-pointer)])
      (set-jpeg_any_struct-err! m (jpeg_std_error e))
      (set-jpeg_error_mgr-error_exit! e (cast error-exit (_fun _j_common_ptr -> _void) _fpointer))
      (let ([s (with-handlers ([exn:fail? (lambda (exn) (exn-message exn))])
                 (jpeg_CreateDecompress/test m 0 dummy-size)
                 "")])
        (free m)
        (free e)
        (let ([m (regexp-match #rx"version: library is ([0-9]+)" s)])
          (if m
              (string->number (cadr m))
              "unknown"))))))

(unless (member JPEG_LIB_VERSION '(62 64 70 80 90))
  (error 'jpeg "unsupported library version: ~e" JPEG_LIB_VERSION))

(define-syntax-rule (cstruct-type/version elem ...)
  (make-cstruct-type (flatten-type (list (cstruct-type/version-elem elem) ...))))

(define-syntax cstruct-type/version-elem
  (syntax-rules (unquote)
    [(_ (unquote ([vers (elem ...)] ...)))
     (case JPEG_LIB_VERSION
       [vers (list (cstruct-type/version-elem elem) ...)]
       ...)]
    [(_ [name type]) type]))

(define (flatten-type l)
  (cond
   [(null? l) null]
   [(list? (car l))
    (append (flatten-type (car l)) (flatten-type (cdr l)))]
   [else
    (cons (car l) (flatten-type (cdr l)))]))

(define-cstruct _jpeg_decompress_struct ([err _jpeg_error_mgr-pointer]
                                         [mem _jpeg_memory_mgr-pointer]
                                         [progress _pointer] ; _jpeg_progress_mgr_pointer
                                         [client_data _pointer]
                                         [is_decompressor _jbool]
                                         [global_state _int]
                                         [src* _pointer] ; actually jpeg_source_mgr-pointer

                                         [image_width _JDIMENSION]
                                         [image_height _JDIMENSION]
                                         [input_components _int]
                                         [jpeg_color_space _J_COLOR_SPACE]
                                         [out_color_space _J_COLOR_SPACE]

                                         [scale_num _uint]
                                         [scale_denom _uint]

                                         [output_gamma _double]

                                         [buffered_image _jbool]
                                         [raw_data_out _jbool]

                                         [dct_method _J_DCT_METHOD]
                                         [do_fancy_upsampling _jbool]
                                         [do_block_smoothing _jbool]

                                         [quantize_colors _jbool]
                                         [dither_mode _J_DITHER_MODE]
                                         [two_pass_quantize _jbool]
                                         [desired_number_of_colors _int]
                                         [enable_1pass_quant _jbool]
                                         [enable_external_quant _jbool]
                                         [enable_2pass_quant _jbool]

                                         [output_width _JDIMENSION]
                                         [output_height _JDIMENSION] ; <-
                                         [out_color_components _int]
                                         [output_components _int]
                                         [rec_outbuf_height _int]

                                         [actual_number_of_colors _int]
                                         [colormap _pointer]

                                         ;; We don't need to refer to any of the remaining
                                         ;; fields, and the specific set of fields depends
                                         ;; on the version
                                         [rest
                                          (cstruct-type/version
                                           [output_scanline _JDIMENSION]

                                           [input_scan_number _int]
                                           [input_iMCU_row _JDIMENSION]

                                           [output_scan_number _int]
                                           [output_iMCU_row _JDIMENSION]
                                           
                                           [coef_bits _pointer]
                                           
                                           [quant_tbl_ptrs_1 _pointer]
                                           [quant_tbl_ptrs_2 _pointer]
                                           [quant_tbl_ptrs_3 _pointer]
                                           [quant_tbl_ptrs_4 _pointer]
                                           
                                           [dc_huff_tbl_ptrs_1 _pointer]
                                           [dc_huff_tbl_ptrs_2 _pointer]
                                           [dc_huff_tbl_ptrs_3 _pointer]
                                           [dc_huff_tbl_ptrs_4 _pointer]
                                           [ac_huff_tbl_ptrs_1 _pointer]
                                           [ac_huff_tbl_ptrs_2 _pointer]
                                           [ac_huff_tbl_ptrs_3 _pointer]
                                           [ac_huff_tbl_ptrs_4 _pointer]

                                           [data_precision _int]

                                           [comp_info _pointer]

                                           ,([(62 64 70) ()]
                                             [else ([is_baseline _jbool])])
                                           [progressive_mode _jbool]
                                           [arith_code _jbool]

                                           [arith_dc_L_1 _uint8]
                                           [arith_dc_L_2 _uint8]
                                           [arith_dc_L_3 _uint8]
                                           [arith_dc_L_4 _uint8]
                                           [arith_dc_L_5 _uint8]
                                           [arith_dc_L_6 _uint8]
                                           [arith_dc_L_7 _uint8]
                                           [arith_dc_L_8 _uint8]
                                           [arith_dc_L_9 _uint8]
                                           [arith_dc_L_10 _uint8]
                                           [arith_dc_L_11 _uint8]
                                           [arith_dc_L_12 _uint8]
                                           [arith_dc_L_13 _uint8]
                                           [arith_dc_L_14 _uint8]
                                           [arith_dc_L_15 _uint8]
                                           [arith_dc_L_16 _uint8]

                                           [arith_dc_U_1 _uint8]
                                           [arith_dc_U_2 _uint8]
                                           [arith_dc_U_3 _uint8]
                                           [arith_dc_U_4 _uint8]
                                           [arith_dc_U_5 _uint8]
                                           [arith_dc_U_6 _uint8]
                                           [arith_dc_U_7 _uint8]
                                           [arith_dc_U_8 _uint8]
                                           [arith_dc_U_9 _uint8]
                                           [arith_dc_U_10 _uint8]
                                           [arith_dc_U_11 _uint8]
                                           [arith_dc_U_12 _uint8]
                                           [arith_dc_U_13 _uint8]
                                           [arith_dc_U_14 _uint8]
                                           [arith_dc_U_15 _uint8]
                                           [arith_dc_U_16 _uint8]

                                           [arith_dc_K_1 _uint8]
                                           [arith_dc_K_2 _uint8]
                                           [arith_dc_K_3 _uint8]
                                           [arith_dc_K_4 _uint8]
                                           [arith_dc_K_5 _uint8]
                                           [arith_dc_K_6 _uint8]
                                           [arith_dc_K_7 _uint8]
                                           [arith_dc_K_8 _uint8]
                                           [arith_dc_K_9 _uint8]
                                           [arith_dc_K_10 _uint8]
                                           [arith_dc_K_11 _uint8]
                                           [arith_dc_K_12 _uint8]
                                           [arith_dc_K_13 _uint8]
                                           [arith_dc_K_14 _uint8]
                                           [arith_dc_K_15 _uint8]
                                           [arith_dc_K_16 _uint8]

                                           [restart_interval _uint]

                                           [saw_JFIF_marker _jbool]
                                           [JFIF_major_version _uint8]
                                           [JFIF_minor_version _uint8]
                                           [density_unit _uint8]
                                           [X_density _uint16]
                                           [Y_density _uint16]
                                           [saw_Adobe_marker _jbool]
                                           [Adobe_transform _uint8]

                                           ,([(90) ([color_transform _J_COLOR_TRANSFORM])]
                                             [else ()])

                                           [CCIR601_sampling _jbool]

                                           [marker_list _pointer]

                                           [max_h_samp_factor _int]
                                           [max_v_samp_factor _int]

                                           ,([(62 64) ([min_DCT_scaled_size _int])]
                                             [else ([min_DCT_h_scaled_size _int]
                                                    [min_DCT_v_scaled_size _int])])

                                           [total_iMCU_rows _JDIMENSION]
                                           [sample_range_limit _pointer]

                                           [comps_in_scan _int]

                                           [cur_comp_info_1 _pointer]
                                           [cur_comp_info_2 _pointer]
                                           [cur_comp_info_3 _pointer]
                                           [cur_comp_info_4 _pointer]

                                           [MCUs_per_row _JDIMENSION]
                                           [MCU_rows_in_scan _JDIMENSION]

                                           [blocks_in_MCU _int]
                                           
                                           [MCU_membership_1 _int]
                                           [MCU_membership_2 _int]
                                           [MCU_membership_3 _int]
                                           [MCU_membership_4 _int]
                                           [MCU_membership_5 _int]
                                           [MCU_membership_6 _int]
                                           [MCU_membership_7 _int]
                                           [MCU_membership_8 _int]
                                           [MCU_membership_9 _int]
                                           [MCU_membership_10 _int]

                                           [Ss _int]
                                           [Se _int]
                                           [Ah _int]
                                           [Al _int]
                                           
                                           ,([(62 64 70) ()]
                                             [else
                                              ([block_size _int]
                                               [natural_order _pointer]
                                               [lim_Se _int])])

                                           [unread_marker _int]

                                           [master _pointer]
                                           [main _pointer]
                                           [coef _pointer]
                                           [post _pointer]
                                           [inputctl _pointer]
                                           [marker _pointer]
                                           [entropy _pointer]
                                           [idct _pointer]
                                           [upsample _pointer]
                                           [cconvert _pointer]
                                           [cquantize _pointer])]))

(define _j_decompress_ptr _jpeg_decompress_struct-pointer)

(define-cstruct _jpeg_source_mgr ([next_input_byte _pointer] ;; /* => next byte to read from buffer */
                                  [bytes_in_buffer _size_t]  ;; /* # of bytes remaining in buffer */
                                  [init_source _fpointer] ; (_fun _j_decompress_ptr -> _void)
                                  [fill_input_buffer _fpointer] ; (_fun _j_decompress_ptr -> _jbool)
                                  [skip_input_data _fpointer] ; (_fun _j_decompress_ptr _long -> _void)
                                  [resync_to_restart _fpointer] ; (_fun _j_decompress_ptr _int -> _jbool)
                                  [term_source _fpointer] ; (_fun _j_decompress_ptr -> _void)
                                  ;; extra fields specific to this binding:
                                  [buffer _pointer]))

(define (jpeg_decompress_struct-src m)
  (ptr-cast (jpeg_decompress_struct-src* m) _jpeg_source_mgr-pointer))

(define-cstruct _jpeg_compress_struct ([err _jpeg_error_mgr-pointer]
                                       [mem _jpeg_memory_mgr-pointer]
                                       [progress _pointer] ; _jpeg_progress_mgr_pointer
                                       [client_data _pointer]
                                       [is_decompressor _jbool]
                                       [global_state _int]

                                       [dest* _pointer] ; actually jpeg_destination_mgr-pointer

                                       [image_width _JDIMENSION]
                                       [image_height _JDIMENSION]
                                       [input_components _int]
                                       [in_color_space _J_COLOR_SPACE] ; <-
  
                                       [input_gamma _double]

                                       ;; We don't need to refer to any of the remaining
                                       ;; fields, and the specific set of fields depends
                                       ;; on the version
                                       [rest
                                        (cstruct-type/version
                                         ,([(62 64) ()]
                                           [else
                                            ([scale_num _uint]
                                             [scale_denom _uint]
                                             [jpeg_width _JDIMENSION]
                                             [jpeg_height _JDIMENSION])])
                                         
                                         [data_precission _int]

                                         [num_components _int]
                                         [jpeg_color_space _J_COLOR_SPACE]

                                         [comp_info _pointer]

                                         [quant_tbl_ptrs_1 _pointer]
                                         [quant_tbl_ptrs_2 _pointer]
                                         [quant_tbl_ptrs_3 _pointer]
                                         [quant_tbl_ptrs_4 _pointer]
                                         
                                         ,([(62 64) ()]
                                           [else
                                            ([q_scale_factor_1 _int]
                                             [q_scale_factor_2 _int]
                                             [q_scale_factor_3 _int]
                                             [q_scale_factor_4 _int])])
                                       
                                         [dc_huff_tbl_ptrs_1 _pointer]
                                         [dc_huff_tbl_ptrs_2 _pointer]
                                         [dc_huff_tbl_ptrs_3 _pointer]
                                         [dc_huff_tbl_ptrs_4 _pointer]
                                         [ac_huff_tbl_ptrs_1 _pointer]
                                         [ac_huff_tbl_ptrs_2 _pointer]
                                         [ac_huff_tbl_ptrs_3 _pointer]
                                         [ac_huff_tbl_ptrs_4 _pointer]

                                         [arith_dc_L_1 _uint8]
                                         [arith_dc_L_2 _uint8]
                                         [arith_dc_L_3 _uint8]
                                         [arith_dc_L_4 _uint8]
                                         [arith_dc_L_5 _uint8]
                                         [arith_dc_L_6 _uint8]
                                         [arith_dc_L_7 _uint8]
                                         [arith_dc_L_8 _uint8]
                                         [arith_dc_L_9 _uint8]
                                         [arith_dc_L_10 _uint8]
                                         [arith_dc_L_11 _uint8]
                                         [arith_dc_L_12 _uint8]
                                         [arith_dc_L_13 _uint8]
                                         [arith_dc_L_14 _uint8]
                                         [arith_dc_L_15 _uint8]
                                         [arith_dc_L_16 _uint8]

                                         [arith_dc_U_1 _uint8]
                                         [arith_dc_U_2 _uint8]
                                         [arith_dc_U_3 _uint8]
                                         [arith_dc_U_4 _uint8]
                                         [arith_dc_U_5 _uint8]
                                         [arith_dc_U_6 _uint8]
                                         [arith_dc_U_7 _uint8]
                                         [arith_dc_U_8 _uint8]
                                         [arith_dc_U_9 _uint8]
                                         [arith_dc_U_10 _uint8]
                                         [arith_dc_U_11 _uint8]
                                         [arith_dc_U_12 _uint8]
                                         [arith_dc_U_13 _uint8]
                                         [arith_dc_U_14 _uint8]
                                         [arith_dc_U_15 _uint8]
                                         [arith_dc_U_16 _uint8]

                                         [arith_dc_K_1 _uint8]
                                         [arith_dc_K_2 _uint8]
                                         [arith_dc_K_3 _uint8]
                                         [arith_dc_K_4 _uint8]
                                         [arith_dc_K_5 _uint8]
                                         [arith_dc_K_6 _uint8]
                                         [arith_dc_K_7 _uint8]
                                         [arith_dc_K_8 _uint8]
                                         [arith_dc_K_9 _uint8]
                                         [arith_dc_K_10 _uint8]
                                         [arith_dc_K_11 _uint8]
                                         [arith_dc_K_12 _uint8]
                                         [arith_dc_K_13 _uint8]
                                         [arith_dc_K_14 _uint8]
                                         [arith_dc_K_15 _uint8]
                                         [arith_dc_K_16 _uint8]
                                         
                                         [num_scans _int]
                                         [scan_info _pointer]

                                         [raw_data_in _jbool]
                                         [arith_code _jbool]
                                         [optimize_coding _jbool]
                                         [CCIR601_sampling _jbool]
                                         ,([(62 64) ()]
                                           [else
                                            ([do_fancy_downsampling _jbool])])

                                         [smoothing_factor _int]
                                         [dct_method _J_DCT_METHOD]

                                         [restart_interval _uint]
                                         [restart_in_rows _int]
                                         
                                         [write_JFIF_header _jbool]
                                         [JFIF_major_version _uint8]
                                         [JFIF_minor_version _uint8]
                                         [density_unit _uint8]
                                         [X_density _uint16]
                                         [Y_density _uint16]
                                         [write_Adobe_marker _jbool]
                                         
                                         ,([(90) ([color_transform _J_COLOR_TRANSFORM])]
                                           [else ()])
                                         
                                         [next_scanline _JDIMENSION]
                                         
                                         [progressive_mode _jbool]
                                         
                                         [max_h_samp_factor _int]
                                         [max_v_samp_factor _int]

                                         ,([(62 64) ()]
                                           [else ([min_DCT_h_scaled_size _int]
                                                  [min_DCT_v_scaled_size _int])])

                                         [total_iMCU_rows _JDIMENSION]

                                         [comps_in_scan _int]
                                         [cur_comp_info_1 _pointer]
                                         [cur_comp_info_2 _pointer]
                                         [cur_comp_info_3 _pointer]
                                         [cur_comp_info_4 _pointer]
                                       
                                         [MCUs_per_row _JDIMENSION]
                                         [MCU_rows_in_scan _JDIMENSION]
                                         
                                         [blocks_in_MCU _int]
                                         
                                         [MCU_membership_1 _int]
                                         [MCU_membership_2 _int]
                                         [MCU_membership_3 _int]
                                         [MCU_membership_4 _int]
                                         [MCU_membership_5 _int]
                                         [MCU_membership_6 _int]
                                         [MCU_membership_7 _int]
                                         [MCU_membership_8 _int]
                                         [MCU_membership_9 _int]
                                         [MCU_membership_10 _int]

                                         [Ss _int]
                                         [Se _int]
                                         [Ah _int]
                                         [Al _int]
                                         
                                         ,([(62 64 70) ()]
                                           [else
                                            ([block_size _int]
                                             [natural_order _pointer]
                                             [lim_Se _int])])

                                         [master _pointer]
                                         [main _pointer]
                                         [prep _pointer]
                                         [coef _pointer]
                                         [marker _pointer]
                                         [cconvert _pointer]
                                         [downsample _pointer]
                                         [fdct _pointer]
                                         [entropy _pointer]
                                         [script_space _pointer]
                                         [script_space_size _int])]))

(define _j_compress_ptr _jpeg_compress_struct-pointer)

(define-cstruct _jpeg_destination_mgr ([next_output_byte _pointer] ;; /* => next byte to write in buffer */
                                       [free_in_buffer _size_t]  ;; /* # of byte spaces remaining in buffer */
                                       [init_destination _fpointer] ; (_fun _j_compress_ptr -> _void)
                                       [empty_output_buffer _fpointer] ; (_fun _j_compress_ptr -> _jbool)
                                       [term_destination _fpointer] ; (_fun _j_compress_ptr -> _void)
                                       ;; extra fields specific to this binding:
                                       [buffer _pointer]))

(define (jpeg_compress_struct-dest m)
  (ptr-cast (jpeg_compress_struct-dest* m) _jpeg_destination_mgr-pointer))


(define BUFFER-SIZE 4096)
(define JPEG_EOI #xD9)

(define (init-source m)
  (void))

(define (fill-input-buffer m)
  (let* ([s (jpeg_decompress_struct-src m)]
         [b (jpeg_source_mgr-buffer s)]
         [bstr (scheme_make_sized_byte_string b BUFFER-SIZE 0)]
         [in (car (ptr-ref (jpeg_decompress_struct-client_data m) _scheme))])
    (let* ([len (read-bytes! bstr in)]
           [len (if (zero? len)
                    (begin
                      (bytes-set! bstr 0 #xFF)
                      (bytes-set! bstr 1 JPEG_EOI)
                      2)
                    len)])
      (set-jpeg_source_mgr-next_input_byte! s b)
      (set-jpeg_source_mgr-bytes_in_buffer! s len)
      #t)))

(define (skip-input-data m len)
  (let* ([s (jpeg_decompress_struct-src m)]
         [avail (jpeg_source_mgr-bytes_in_buffer s)])
    (if (avail . >= . len)
        (begin
          (set-jpeg_source_mgr-next_input_byte! s (ptr-add (jpeg_source_mgr-next_input_byte s) len))
          (set-jpeg_source_mgr-bytes_in_buffer! s (- avail len)))
        (let ([in (car (ptr-ref (jpeg_decompress_struct-client_data m) _scheme))])
          (read-bytes (- len avail) in)
          (set-jpeg_source_mgr-next_input_byte! s #f)
          (set-jpeg_source_mgr-bytes_in_buffer! s 0)
          (void)))))

(define (term-source m)
  ;; Maybe add support to optionally close port as early as possible?
  (when #f
    (let ([in (car (ptr-ref (jpeg_decompress_struct-client_data m) _scheme))])
      (close-input-port in))))

(define (init-destination m)
  (void))

(define (do-empty-output-buffer m all?)
  (let* ([d (jpeg_compress_struct-dest m)]
         [b (jpeg_destination_mgr-buffer d)]
         [bstr (scheme_make_sized_byte_string b 
                                              (if all?
                                                  BUFFER-SIZE
                                                  (- BUFFER-SIZE (jpeg_destination_mgr-free_in_buffer d)))
                                              0)]
         [out (car (ptr-ref (jpeg_compress_struct-client_data m) _scheme))])
    (write-bytes bstr out)
    (set-jpeg_destination_mgr-next_output_byte! d b)
    (set-jpeg_destination_mgr-free_in_buffer! d BUFFER-SIZE)
    #t))

(define (empty-output-buffer m)
  (do-empty-output-buffer m #t))

(define (term-destination m)
  (do-empty-output-buffer m #f)
  ;; Maybe add support to optionally close port as early as possible?
  (when #f
    (let ([in (car (ptr-ref (jpeg_decompress_struct-client_data m) _scheme))])
      (close-input-port in))))

(define (ptr-cast p t) (cast p _pointer t))

(define destroy-decompress
  ((deallocator)
   (lambda (m)
     (free (jpeg_source_mgr-buffer (jpeg_decompress_struct-src m)))
     (free (jpeg_decompress_struct-err m))
     (free (jpeg_decompress_struct-src m))
     (free-immobile-cell (jpeg_decompress_struct-client_data m))
     (free m))))

(define create-decompress
  ((allocator destroy-decompress)
   (lambda (in)
     (let ([m (ptr-cast (malloc _jpeg_decompress_struct 'raw) _jpeg_decompress_struct-pointer)]
           [s (ptr-cast (malloc _jpeg_source_mgr 'raw) _jpeg_source_mgr-pointer)]
           [e (ptr-cast (malloc sizeof_jpeg_error_mgr 'raw) _jpeg_error_mgr-pointer)]
           [b (malloc 'raw BUFFER-SIZE)]
           [funs (box null)])
       (set-jpeg_decompress_struct-err! m (jpeg_std_error e))
       (set-jpeg_error_mgr-error_exit! e (cast error-exit
                                               (_fun #:keep funs _j_common_ptr -> _void)
                                               _fpointer))
       (jpeg_CreateDecompress m JPEG_LIB_VERSION (ctype-sizeof _jpeg_decompress_struct))
       (set-jpeg_decompress_struct-src*! m s)
       (set-jpeg_source_mgr-buffer! s b)
       (set-jpeg_decompress_struct-client_data! m (malloc-immobile-cell (cons in funs)))
       (set-jpeg_source_mgr-next_input_byte! s #f)
       (set-jpeg_source_mgr-bytes_in_buffer! s 0)
       (set-jpeg_source_mgr-init_source! s (cast init-source
                                                 (_fun #:keep funs _j_decompress_ptr -> _void)
                                                 _fpointer))
       (set-jpeg_source_mgr-fill_input_buffer! s (cast fill-input-buffer
                                                       (_fun #:keep funs _j_decompress_ptr -> _jbool)
                                                       _fpointer))
       (set-jpeg_source_mgr-skip_input_data! s (cast skip-input-data
                                                     (_fun #:keep funs _j_decompress_ptr _long -> _void)
                                                     _fpointer))
       (set-jpeg_source_mgr-resync_to_restart! s jpeg_resync_to_restart)
       (set-jpeg_source_mgr-term_source! s (cast term-source
                                                 (_fun #:keep funs _j_decompress_ptr -> _void)
                                                 _fpointer))
       m))))

(define destroy-compress
  ((deallocator)
   (lambda (m)
     (free (jpeg_destination_mgr-buffer (jpeg_compress_struct-dest m)))
     (free (jpeg_compress_struct-dest m))
     (free (jpeg_compress_struct-err m))
     (free-immobile-cell (jpeg_compress_struct-client_data m))
     (free m))))

(define create-compress
  ((allocator destroy-compress)
   (lambda (out)
     (let ([m (ptr-cast (malloc _jpeg_compress_struct 'raw) _jpeg_compress_struct-pointer)]
           [d (ptr-cast (malloc _jpeg_destination_mgr 'raw) _jpeg_destination_mgr-pointer)]
           [e (ptr-cast (malloc sizeof_jpeg_error_mgr 'raw) _jpeg_error_mgr-pointer)]
           [b (malloc 'raw BUFFER-SIZE)]
           [funs (box null)])
       (set-jpeg_compress_struct-err! m (jpeg_std_error e))
       (set-jpeg_error_mgr-error_exit! e (cast error-exit
                                               (_fun #:keep funs _j_common_ptr -> _void)
                                               _fpointer))
       (jpeg_CreateCompress m JPEG_LIB_VERSION (ctype-sizeof _jpeg_compress_struct))
       (set-jpeg_compress_struct-dest*! m d)
       (set-jpeg_destination_mgr-buffer! d b)
       (set-jpeg_compress_struct-client_data! m (malloc-immobile-cell (cons out funs)))
       (set-jpeg_destination_mgr-next_output_byte! d b)
       (set-jpeg_destination_mgr-free_in_buffer! d BUFFER-SIZE)
       (set-jpeg_destination_mgr-init_destination! d (cast init-destination
                                                           (_fun #:keep funs _j_compress_ptr -> _void)
                                                           _fpointer))
       (set-jpeg_destination_mgr-empty_output_buffer! d (cast empty-output-buffer
                                                              (_fun #:keep funs _j_compress_ptr -> _jbool)
                                                              _fpointer))
       (set-jpeg_destination_mgr-term_destination! d (cast term-destination
                                                           (_fun #:keep funs _j_compress_ptr -> _void)
                                                           _fpointer))
       m))))

(define (create-jpeg-sample-array m len)
  (let ([samps ((jpeg_memory_mgr-alloc_sarray (if (jpeg_decompress_struct? m)
                                                  (jpeg_decompress_struct-mem m)
                                                  (jpeg_compress_struct-mem m)))
                m
                JPOOL_IMAGE
                len
                1)])
    (values samps (scheme_make_sized_byte_string (ptr-ref samps _pointer) len 0))))

(define-jpeg/private jpeg_CreateDecompress (_fun _j_decompress_ptr _int _int -> _void))
(define-jpeg/private jpeg_resync_to_restart _fpointer) ; (_fun _j_decompress_ptr _int -> _jbool))
(define-jpeg jpeg_read_header (_fun _j_decompress_ptr _jbool -> _void))
(define-jpeg jpeg_start_decompress (_fun _j_decompress_ptr -> _void))
(define-jpeg jpeg_read_scanlines (_fun _j_decompress_ptr _pointer _int -> _void))
(define-jpeg jpeg_finish_decompress (_fun _j_decompress_ptr -> _int))

(define-jpeg/private jpeg_CreateCompress (_fun _j_compress_ptr _int _int -> _void))
(define-jpeg jpeg_set_defaults (_fun _j_compress_ptr -> _int))
(define-jpeg jpeg_set_quality (_fun _j_compress_ptr _int _jbool -> _int))
(define-jpeg jpeg_start_compress (_fun _j_compress_ptr _jbool -> _void))
(define-jpeg jpeg_write_scanlines (_fun _j_compress_ptr _pointer _int -> _void))
(define-jpeg jpeg_finish_compress (_fun _j_compress_ptr -> _int))

(provide create-decompress
         destroy-decompress

         create-compress
         destroy-compress

         create-jpeg-sample-array

         jpeg_decompress_struct-output_width
         jpeg_decompress_struct-output_height
         jpeg_decompress_struct-output_components
         jpeg_decompress_struct-mem

         set-jpeg_compress_struct-image_width!
         set-jpeg_compress_struct-image_height!
         set-jpeg_compress_struct-input_components!
         set-jpeg_compress_struct-in_color_space!
         
         JPOOL_IMAGE)
