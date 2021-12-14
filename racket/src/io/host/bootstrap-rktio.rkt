#lang racket/base
(require racket/include
         racket/fixnum
         (only-in '#%linklet primitive-table)
         ffi/unsafe
         ffi/unsafe/atomic
         (for-syntax racket/base)
         (only-in racket/base
                  [void racket:void]))

(define librktio (ffi-lib "librktio"))

(define << arithmetic-shift)

(define void _void)
(define char _byte)
(define int _int)
(define unsigned _uint)
(define unsigned-short _ushort)
(define unsigned-8 _ubyte)
(define intptr_t _intptr)
(define uintptr_t _uintptr)
(define rktio_int64_t _int64)
(define float _float)
(define double _double)
(define function-pointer _pointer)
(define NULL #f)

(define-syntax-rule (define-constant n v) (define n v))

(define-syntax (define-type stx)
  (syntax-case stx (rktio_bool_t rktio_ok_t rktio_const_string_t)
    [(_ rktio_bool_t _)
     (with-syntax ([(_ rktio_bool_t _) stx])
       #'(define rktio_bool_t _bool))]
    [(_ rktio_ok_t _)
     (with-syntax ([(_ rktio_ok_t _) stx])
       #'(define rktio_ok_t _bool))]
    [(_ rktio_const_string_t t)
     (with-syntax ([(_ rktio_const_string_t _) stx])
     #'(define rktio_const_string_t _bytes/nul-terminated))]
    [(_ n t) #'(define n t)]))

(define-syntax (define-struct-type stx)
  (syntax-case stx ()
    [(_ n ([type name] ...))
     (with-syntax ([_n (datum->syntax #'n
                                      (string->symbol (format "_R~a" (syntax-e #'n))))]
                   [_n-pointer (datum->syntax #'n
                                              (string->symbol (format "_R~a-pointer" (syntax-e #'n))))])
       #'(begin
           (define-cstruct _n ([name type] ...))
           (define n _n-pointer)))]))

(define-syntax-rule (*ref t) _pointer)
(define-syntax-rule (ref t) _pointer)
(define-syntax-rule (array n t) (_array t n))

(define-syntax-rule (define-function flags ret-type name ([arg-type arg-name] ...))
  (define name
    (get-ffi-obj 'name librktio (_fun arg-type ... -> ret-type))))

(define-syntax-rule (define-function/errno* err-v flags ret-type name ([rktio-type rktio-name] [arg-type arg-name] ...)
                      err-expr)
  (begin
    (define proc
      (get-ffi-obj 'name librktio (_fun rktio-type arg-type ... -> ret-type)))
    (define (name rktio-name arg-name ...)
      (begin
        (start-atomic)
        (begin0
          (let ([v (proc rktio-name arg-name ...)])
            (if (eqv? v err-v)
                err-expr
                v))
          (end-atomic))))))

(define-syntax-rule (define-function/errno err-v flags ret-type name ([rktio-type rktio-name] [arg-type arg-name] ...))
  (define-function/errno* err-v flags ret-type name ([rktio-type rktio-name] [arg-type arg-name] ...)
    (vector (rktio_get_last_error_kind rktio-name)
            (rktio_get_last_error rktio-name))))

(define-syntax-rule (define-function/errno+step err-v flags ret-type name ([rktio-type rktio-name] [arg-type arg-name] ...))
  (define-function/errno* err-v flags ret-type name ([rktio-type rktio-name] [arg-type arg-name] ...)
    (vector (rktio_get_last_error_kind rktio-name)
            (rktio_get_last_error rktio-name)
            (rktio_get_last_error_step rktio-name))))

(include "../../rktio/rktio.rktl")

(define rktio_NULL #f)

(define (rktio_filesize_ref fs)
  (ptr-ref fs rktio_filesize_t))
(define (rktio_timestamp_ref fs)
  (ptr-ref fs rktio_timestamp_t))
(define (rktio_is_timestamp v)
  (let ([radix (arithmetic-shift 1 (sub1 (* 8 (ctype-sizeof rktio_timestamp_t))))])
    (<= (- radix) v (sub1 radix))))

(define (rktio_recv_length_ref p)
  (Rrktio_length_and_addrinfo_t-len (cast p _pointer rktio_length_and_addrinfo_t)))

(define (rktio_recv_address_ref p)
  (Rrktio_length_and_addrinfo_t-address (cast p _pointer rktio_length_and_addrinfo_t)))

(define (rktio_stat_to_vector p)
  (let ([p (cast p _pointer _Rrktio_stat_t-pointer)])
    (vector
     (Rrktio_stat_t-device_id p)
     (Rrktio_stat_t-inode p)
     (Rrktio_stat_t-mode p)
     (Rrktio_stat_t-hardlink_count p)
     (Rrktio_stat_t-user_id p)
     (Rrktio_stat_t-group_id p)
     (Rrktio_stat_t-device_id_for_special_file p)
     (Rrktio_stat_t-size p)
     (Rrktio_stat_t-block_size p)
     (Rrktio_stat_t-block_count p)
     (Rrktio_stat_t-access_time_seconds p)
     (Rrktio_stat_t-access_time_nanoseconds p)
     (Rrktio_stat_t-modify_time_seconds p)
     (Rrktio_stat_t-modify_time_nanoseconds p)
     (Rrktio_stat_t-ctime_seconds p)
     (Rrktio_stat_t-ctime_nanoseconds p)
     (Rrktio_stat_t-ctime_is_change_time p))))

(define (rktio_identity_to_vector p)
  (let ([p (cast p _pointer _Rrktio_identity_t-pointer)])
    (vector
     (Rrktio_identity_t-a p)
     (Rrktio_identity_t-b p)
     (Rrktio_identity_t-c p)
     (Rrktio_identity_t-a_bits p)
     (Rrktio_identity_t-b_bits p)
     (Rrktio_identity_t-c_bits p))))

(define (in-date-range? si)
  (if (fixnum? (expt 2 33))
      (<= -9223372036854775808 si 9223372036854775807)
      (<= -2147483648 si 2147483647)))

(define unknown-zone-name (string->immutable-string "?"))

(define (rktio_seconds_to_date* rktio si nsecs get-gmt)
  (cond
    [(not (in-date-range? si))
     (vector RKTIO_ERROR_KIND_RACKET
             RKTIO_ERROR_TIME_OUT_OF_RANGE)]
    [else
     (let ([p (rktio_seconds_to_date rktio si nsecs get-gmt)])
       (cond
         [(vector? p) p]
         [else
          (define dt (cast p _pointer _Rrktio_date_t-pointer))
          (define tzn (Rrktio_date_t-zone_name dt))
          (begin0
            (date*
             (Rrktio_date_t-second dt)
             (Rrktio_date_t-minute dt)
             (Rrktio_date_t-hour dt)
             (Rrktio_date_t-day dt)
             (Rrktio_date_t-month dt)
             (Rrktio_date_t-year dt)
             (Rrktio_date_t-day_of_week dt)
             (Rrktio_date_t-day_of_year dt)
             (if (fx= 0 (Rrktio_date_t-is_dst dt)) #f #t)
             (Rrktio_date_t-zone_offset dt)
             (Rrktio_date_t-nanosecond dt)
             (if tzn
                 (string->immutable-string (bytes->string/utf-8 (rktio_to_bytes tzn)))
                 unknown-zone-name))
            (unless tzn
              (rktio_free tzn))
            (rktio_free p))]))]))

(define (rktio_convert_result_to_vector p)
  (let ([p (cast p _pointer _Rrktio_convert_result_t-pointer)])
    (vector
     (Rrktio_convert_result_t-in_consumed p)
     (Rrktio_convert_result_t-out_produced p)
     (Rrktio_convert_result_t-converted p))))

(define (rktio_to_bytes fs)
  (bytes-copy (cast fs _pointer _bytes)))

(define (rktio_to_shorts fs)
  (let loop ([len 0])
    (cond
      [(zero? (ptr-ref fs _short len))
       (define bstr (make-bytes (* len 2)))
       (memcpy bstr fs (* len 2))
       bstr]
      [else
       (loop (add1 len))])))

;; Unlike `rktio_to_bytes`, frees the array and strings
(define (rktio_to_bytes_list lls [len #f])
  (begin0
    (let loop ([i 0])
      (cond
        [(and len (= i len))
         null]
        [else
         (define bs (ptr-ref lls _pointer i))
         (if bs
             (cons (begin0
                     (cast bs _pointer _bytes)
                     (rktio_free bs))
                   (loop (add1 i)))
             null)]))
    (rktio_free lls)))

(define (rktio_from_bytes_list bstrs)
  (cast bstrs (_list i _bytes) _gcpointer))

(define (rktio_free_bytes_list lls len)
  (racket:void))

(define (rktio_process_result_stdin_fd r)
  (Rrktio_process_result_t-stdin_fd (cast r _pointer _Rrktio_process_result_t-pointer)))
(define (rktio_process_result_stdout_fd r)
  (Rrktio_process_result_t-stdout_fd (cast r _pointer _Rrktio_process_result_t-pointer)))
(define (rktio_process_result_stderr_fd r)
  (Rrktio_process_result_t-stderr_fd (cast r _pointer _Rrktio_process_result_t-pointer)))
(define (rktio_process_result_process r)
  (Rrktio_process_result_t-process (cast r _pointer _Rrktio_process_result_t-pointer)))

(define (rktio_status_running r)
  (Rrktio_status_t-running (cast r _pointer _Rrktio_status_t-pointer)))
(define (rktio_status_result r)
  (Rrktio_status_t-result (cast r _pointer _Rrktio_status_t-pointer)))

(define (rktio_pipe_results r)
  (values (ptr-ref r _pointer)
          (ptr-ref r _pointer 1)))

(define (rktio_do_install_os_signal_handler rktio)
  (racket:void))
(define (rktio_get_ctl_c_handler)
  (lambda (k)
    (racket:void)))

(define (rktio_make_sha1_ctx)
  (malloc _Rrktio_sha1_ctx_t))
(define (rktio_make_sha2_ctx)
  (malloc _Rrktio_sha2_ctx_t))

(define (replace-ltps-open ht)
  ;; Disable rktio_ltps_open in demo mode, since it is not
  ;; connected to the host scheduler
  (hash-set ht 'rktio_ltps_open
            (lambda (x) (vector RKTIO_ERROR_KIND_RACKET
                                RKTIO_ERROR_UNSUPPORTED))))

(primitive-table '#%rktio
                 (replace-ltps-open
                  (let ()
                    (define-syntax extract-functions
                      (syntax-rules (define-constant
                                      define-type
                                      define-struct-type
                                      define-function
                                      define-function/errno
                                      define-function/errno+step)
                        [(_ accum) (hasheq . accum)]
                        [(_ accum (define-constant . _) . rest)
                         (extract-functions accum . rest)]
                        [(_ accum (define-type . _) . rest)
                         (extract-functions accum . rest)]
                        [(_ accum (define-struct-type . _) . rest)
                         (extract-functions accum . rest)]
                        [(_ accum (define-function _ _ id . _) . rest)
                         (extract-functions ('id id . accum) . rest)]
                        [(_ accum (define-function/errno _ _ _ id . _) . rest)
                         (extract-functions ('id id . accum) . rest)]
                        [(_ accum (define-function/errno+step _ _ _ id . _) . rest)
                         (extract-functions ('id id . accum) . rest)]))
                    (define-syntax-rule (begin form ...)
                      (extract-functions [#;(begin)
                                          'rktio_NULL rktio_NULL
                                          'rktio_filesize_ref rktio_filesize_ref
                                          'rktio_timestamp_ref rktio_timestamp_ref
                                          'rktio_is_timestamp rktio_is_timestamp
                                          'rktio_recv_length_ref rktio_recv_length_ref
                                          'rktio_recv_address_ref rktio_recv_address_ref
                                          'rktio_stat_to_vector rktio_stat_to_vector
                                          'rktio_identity_to_vector rktio_identity_to_vector
                                          'rktio_seconds_to_date* rktio_seconds_to_date*
                                          'rktio_convert_result_to_vector rktio_convert_result_to_vector
                                          'rktio_to_bytes rktio_to_bytes
                                          'rktio_to_bytes_list rktio_to_bytes_list
                                          'rktio_to_shorts rktio_to_shorts
                                          'rktio_from_bytes_list rktio_from_bytes_list
                                          'rktio_free_bytes_list rktio_free_bytes_list
                                          'rktio_make_sha1_ctx rktio_make_sha1_ctx
                                          'rktio_make_sha2_ctx rktio_make_sha2_ctx
                                          'rktio_process_result_stdin_fd rktio_process_result_stdin_fd
                                          'rktio_process_result_stdout_fd rktio_process_result_stdout_fd
                                          'rktio_process_result_stderr_fd rktio_process_result_stderr_fd
                                          'rktio_process_result_process rktio_process_result_process
                                          'rktio_status_running rktio_status_running
                                          'rktio_status_result rktio_status_result
                                          'rktio_pipe_results rktio_pipe_results
                                          'rktio_do_install_os_signal_handler rktio_do_install_os_signal_handler
                                          'rktio_get_ctl_c_handler rktio_get_ctl_c_handler]
                                         form ...))
                    (include "../../rktio/rktio.rktl"))))
