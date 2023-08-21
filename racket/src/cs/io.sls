(library (io)
  (export)
  (import (rename (except (chezpart)
                          close-port)
                  [define chez:define])
          (rename (only (chezscheme)
                        read-char peek-char
                        current-directory
                        error
                        input-port? output-port?
                        file-position flush-output-port
                        file-symbolic-link?)
                  [input-port? chez:input-port?]
                  [output-port? chez:output-port?]
                  [flush-output-port flush-output])
          (rename (rumble)
                  ;; Remapped to place-local register operations:
                  [unsafe-make-place-local rumble:unsafe-make-place-local]
                  [unsafe-place-local-ref rumble:unsafe-place-local-ref]
                  [unsafe-place-local-set! rumble:unsafe-place-local-set!]
                  [immobile-cell->address rumble:immobile-cell->address]
                  [address->immobile-cell rumble:address->immobile-cell])
          (thread))

  (include "place-register.ss")
  (define-place-register-define place:define io-register-start io-register-count)

  (define-syntax (define stx)
    (syntax-case stx (unsafe-make-place-local)
      ;; Workaround for redirected access of `unsafe-make-place-local` from #%pthread:
      [(_ alias-id unsafe-make-place-local) #'(begin)]
      ;; Chain to place-register handling:
      [(_ . rest) #'(place:define . rest)]))

  ;; ----------------------------------------
  ;; Tie knots:

  (define (path? v) (is-path? v))
  (define (path->string v) (1/path->string v))
  (define path->complete-path
    (case-lambda
     [(v) (1/path->complete-path v)]
     [(v wrt) (1/path->complete-path v wrt)]))
  (define (absolute-path? v) (1/absolute-path? v))
  (define (relative-path? v) (1/relative-path? v))

  ;; ----------------------------------------

  (module (|#%rktio-instance| ptr->address address->ptr)
    (meta define (convert-type t)
          (syntax-case t (ref *ref rktio_bool_t rktio_const_string_t)
            [(ref . _) #'uptr]
            [(*ref rktio_const_string_t) #'uptr]
            [(*ref . _) #'u8*]
            [rktio_bool_t #'boolean]
            [rktio_const_string_t #'u8*]
            [else t]))

    (define-ftype intptr_t iptr)
    (define-ftype uintptr_t uptr)
    (define-ftype rktio_int64_t integer-64)
    (define-ftype function-pointer uptr)
    (define _uintptr (if (> (fixnum-width) 32) _uint64 _uint32))
    (define NULL 0)

    (define (<< a b) (bitwise-arithmetic-shift-left a b))

    (define-syntax define-constant
      (syntax-rules ()
        [(_ id expr) (define id expr)]))
    
    (define-syntax (define-type stx)
      (syntax-case stx (rktio_const_string_t rktio_ok_t rktio_bool_t)
        [(_ rktio_const_string_t old_type)
         ;; skip
         #'(begin)]
        [(_ rktio_ok_t old_type)
         (with-syntax ([(_ type _) stx])
           #'(define-ftype type boolean))]
        [(_ rktio_bool_t old_type)
         (with-syntax ([(_ type _) stx])
           #'(define-ftype type boolean))]
        [(_ type old-type)
         (with-syntax ([old-type (convert-type #'old-type)])
           #'(define-ftype type old-type))]))

    (define-syntax (define-struct-type stx)
      (syntax-case stx ()
        [(_ type ([old-type field] ...))
         (with-syntax ([(old-type ...) (map convert-type #'(old-type ...))])
           #'(define-ftype type (struct [field old-type] ...)))]))

    ;; Wrap foreign-pointer addressed in a record so that
    ;; the value can be finalized
    (define-record ptr (address))
    (define (ptr->address v) (if (eqv? v NULL) v (ptr-address v)))
    (define (address->ptr v) (if (eqv? v NULL) v (make-ptr v)))

    (define-syntax (let-unwrappers stx)
      ;; Unpack plain pointers; when an argument has type
      ;; `rktio_const_string_t`, add an explicit NUL terminator byte;
      ;; when an argument has a `nullable` wrapper, then add a #f -> 0
      ;; conversion
      (syntax-case stx (rktio_const_string_t ref nullable)
        [(_ () body) #'body]
        [(_ ([rktio_const_string_t arg-name] . args) body)
         #'(let ([arg-name (add-nul-terminator arg-name)])
             (let-unwrappers args body))]
        [(_ ([(ref (nullable type)) arg-name] . args) body)
         #'(let ([arg-name (ptr->address (or arg-name NULL))])
             (let-unwrappers args body))]
        [(_ ([(ref type) arg-name] . args) body)
         #'(let ([arg-name (ptr->address arg-name)])
             (let-unwrappers args body))]
        [(_ ([(*ref rktio_const_string_t) arg-name] . args) body)
         #'(let ([arg-name (ptr->address arg-name)])
             (let-unwrappers args body))]
        [(_ (_ . args) body)
         #'(let-unwrappers args body)]))

    (define (add-nul-terminator bstr)
      (and bstr (bytes-append bstr '#vu8(0))))

    (define-syntax (wrap-result stx)
      (syntax-case stx (ref)
        [(_ (ref _) v) #'(address->ptr v)]
        [(_ _ v) #'v]))

    (define-syntax (wrap-result/allow-callbacks stx)
      (syntax-case stx ()
        [(_ t v) #'(call-enabling-ffi-callbacks (lambda () (wrap-result t v)))]))
    
    (meta define (convert-function stx)
          (syntax-case stx ()
            [(_ (flag ...) orig-ret-type name ([orig-arg-type arg-name] ...))
             (with-syntax ([ret-type (convert-type #'orig-ret-type)]
                           [(arg-type ...) (map convert-type #'(orig-arg-type ...))]
                           [(conv ...) (if (#%memq 'blocking (map syntax->datum #'(flag ...)))
                                           #'(__collect_safe)
                                           #'())]
                           [wrap-result (if (#%memq 'msg-queue (map syntax->datum #'(flag ...)))
                                            #'wrap-result/allow-callbacks
                                            #'wrap-result)])
               #'(let ([proc (foreign-procedure conv ... (rktio-lookup 'name)
                                                (arg-type ...)
                                                ret-type)])
                   (lambda (arg-name ...)
                     (let-unwrappers
                      ([orig-arg-type arg-name] ...)
                      (wrap-result orig-ret-type (proc arg-name ...))))))]))

    (define-syntax (define-function stx)
      (syntax-case stx ()
        [(_ _ _ name . _)
         (with-syntax ([rhs (convert-function stx)])
           #'(define name rhs))]))

    (define-syntax (define-function*/errno stx)
      (syntax-case stx ()
        [(_ err-val err-expr flags ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (with-syntax ([rhs (convert-function
                             #'(define-function flags ret-type name ([rktio-type rktio] [arg-type arg] ...)))])
           #'(define name
               (let ([proc rhs])
                 (lambda (rktio arg ...)
                   (let ([v (proc rktio arg ...)])
                     (if (eqv? v err-val)
                         err-expr
                         v))))))]))

    (define-syntax define-function/errno
      (syntax-rules ()
        [(_ err-val flags ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (define-function*/errno err-val
           (vector (rktio_get_last_error_kind rktio)
                   (rktio_get_last_error rktio))
           flags ret-type name ([rktio-type rktio] [arg-type arg] ...))]))
    
    (define-syntax define-function/errno+step
      (syntax-rules ()
        [(_ err-val flags ret-type name ([rktio-type rktio] [arg-type arg] ...))
         (define-function*/errno err-val
           (vector (rktio_get_last_error_kind rktio)
                   (rktio_get_last_error rktio)
                   (rktio_get_last_error_step rktio))
           flags ret-type name ([rktio-type rktio] [arg-type arg] ...))]))

    (define loaded-librktio
      (or (foreign-entry? "rktio_init")
          (load-shared-object (path-build (or (#%getenv "RACKET_IO_SOURCE_DIR")
                                              (#%current-directory))
                                          (string-append "../../lib/librktio" (utf8->string (system-type 'so-suffix)))))))

    (define (rktio-lookup name)
      (foreign-entry (symbol->string name)))

    ;; workaround for `include` not using `(source-directories)` when
    ;; a path starts with "..":
    (define-syntax (include-rel stx)
      (syntax-case stx ()
        [(inc path)
         (let ([new-path (ormap (lambda (dir)
                                  (let ([p (path-build dir (#%syntax->datum #'path))])
                                    (and (#%file-exists? p)
                                         p)))
                                (source-directories))])
           (#%datum->syntax #'inc `(include ,(or new-path #'path))))]))
    (include-rel "../rktio/rktio.rktl")

    (define (rktio_filesize_ref fs)
      (ftype-ref rktio_filesize_t () (make-ftype-pointer rktio_filesize_t (ptr->address fs))))
    (define (rktio_timestamp_ref fs)
      (ftype-ref rktio_timestamp_t () (make-ftype-pointer rktio_timestamp_t (ptr->address fs))))
    (define (rktio_is_timestamp v)
      (let ([radix (arithmetic-shift 1 (sub1 (* 8 (ftype-sizeof rktio_timestamp_t))))])
        (<= (- radix) v (sub1 radix))))

    (define (rktio_recv_length_ref fs)
      (ftype-ref rktio_length_and_addrinfo_t (len) (make-ftype-pointer rktio_length_and_addrinfo_t (ptr->address fs)) 0))

    (define (rktio_recv_address_ref fs)
      (make-ptr
       (ftype-ref rktio_length_and_addrinfo_t (address) (make-ftype-pointer rktio_length_and_addrinfo_t (ptr->address fs)) 0)))

    (define (rktio_stat_to_vector p)
      (let ([p (make-ftype-pointer rktio_stat_t (ptr->address p))])
        (vector
         (ftype-ref rktio_stat_t (device_id) p)
         (ftype-ref rktio_stat_t (inode) p)
         (ftype-ref rktio_stat_t (mode) p)
         (ftype-ref rktio_stat_t (hardlink_count) p)
         (ftype-ref rktio_stat_t (user_id) p)
         (ftype-ref rktio_stat_t (group_id) p)
         (ftype-ref rktio_stat_t (device_id_for_special_file) p)
         (ftype-ref rktio_stat_t (size) p)
         (ftype-ref rktio_stat_t (block_size) p)
         (ftype-ref rktio_stat_t (block_count) p)
         (ftype-ref rktio_stat_t (access_time_seconds) p)
         (ftype-ref rktio_stat_t (access_time_nanoseconds) p)
         (ftype-ref rktio_stat_t (modify_time_seconds) p)
         (ftype-ref rktio_stat_t (modify_time_nanoseconds) p)
         (ftype-ref rktio_stat_t (ctime_seconds) p)
         (ftype-ref rktio_stat_t (ctime_nanoseconds) p)
         (ftype-ref rktio_stat_t (ctime_is_change_time) p))))

    (define (rktio_identity_to_vector p)
      (let ([p (make-ftype-pointer rktio_identity_t (ptr->address p))])
        (vector
         (ftype-ref rktio_identity_t (a) p)
         (ftype-ref rktio_identity_t (b) p)
         (ftype-ref rktio_identity_t (c) p)
         (ftype-ref rktio_identity_t (a_bits) p)
         (ftype-ref rktio_identity_t (b_bits) p)
         (ftype-ref rktio_identity_t (c_bits) p))))

    (define (in-date-range? si)
      (if (> (fixnum-width) 32)
          (<= -9223372036854775808 si 9223372036854775807)
          (<= -2147483648 si 2147483647)))

    (define unknown-zone-name (string->immutable-string "?"))

    (define (rktio_seconds_to_date* rktio si nsecs get-gmt)
      (cond
       [(not (in-date-range? si))
        (vector RKTIO_ERROR_KIND_RACKET
                RKTIO_ERROR_TIME_OUT_OF_RANGE)]
       [else
        (unsafe-start-atomic)
        (begin0
          (let ([p (rktio_seconds_to_date rktio si nsecs get-gmt)])
            (cond
             [(vector? p) p]
             [else
              (let* ([dt (make-ftype-pointer rktio_date_t (ptr->address p))]
                     [tzn (address->ptr (ftype-ref rktio_date_t (zone_name) dt))])
                (begin0
                  (date*
                   (ftype-ref rktio_date_t (second) dt)
                   (ftype-ref rktio_date_t (minute) dt)
                   (ftype-ref rktio_date_t (hour) dt)
                   (ftype-ref rktio_date_t (day) dt)
                   (ftype-ref rktio_date_t (month) dt)
                   (ftype-ref rktio_date_t (year) dt)
                   (ftype-ref rktio_date_t (day_of_week) dt)
                   (ftype-ref rktio_date_t (day_of_year) dt)
                   (if (fx= 0 (ftype-ref rktio_date_t (is_dst) dt))
                       #f
                       #t)
                   (ftype-ref rktio_date_t (zone_offset) dt)
                   (ftype-ref rktio_date_t (nanosecond) dt)
                   (if (eqv? tzn NULL)
                       unknown-zone-name
                       (string->immutable-string (utf8->string (rktio_to_bytes tzn)))))
                  (unless (eqv? tzn NULL)
                    (rktio_free tzn))
                  (rktio_free p)))]))
          (unsafe-end-atomic))]))

    (define (rktio_convert_result_to_vector p)
      (let ([p (make-ftype-pointer rktio_convert_result_t (ptr->address p))])
        (vector
         (ftype-ref rktio_convert_result_t (in_consumed) p)
         (ftype-ref rktio_convert_result_t (out_produced) p)
         (ftype-ref rktio_convert_result_t (converted) p))))

    (define (copy-bytes x i)
      (let ([bstr (make-bytevector i)])
        (let loop ([j 0])
          (unless (fx= j i)
            (bytes-set! bstr j (foreign-ref 'unsigned-8 x j))
            (loop (fx+ j 1))))
        bstr))

    (define (copy-terminated-bytes x)
      (let loop ([i 0])
        (if (fx= 0 (foreign-ref 'unsigned-8 x i))
            (copy-bytes x i)
            (loop (fx+ i 1)))))

    (define (copy-terminated-shorts x)
      (let loop ([i 0])
        (if (fx= 0 (foreign-ref 'unsigned-16 x i))
            (copy-bytes x i)
            (loop (fx+ i 2)))))
      
    (define (rktio_to_bytes fs)
      (copy-terminated-bytes (ptr->address fs)))

    (define (rktio_to_shorts fs)
      (copy-terminated-shorts (ptr->address fs)))

    ;; Unlike `rktio_to_bytes`, frees the array and strings
    (define rktio_to_bytes_list
      (case-lambda
       [(lls) (rktio_to_bytes_list lls #f)]
       [(lls len)
        (begin0
         (let loop ([i 0])
           (cond
            [(and len (fx= i len))
             '()]
            [else
             (let ([bs (foreign-ref 'uptr (ptr->address lls) (* i (foreign-sizeof 'uptr)))])
               (if (not (eqv? NULL bs))
                   (cons (begin0
                          (copy-terminated-bytes bs)
                          (rktio_free (make-ptr bs)))
                         (loop (add1 i)))
                   '()))]))
         (rktio_free lls))]))

    ;; Allocates pointers that must be released via `rktio_free_bytes_list`:
    (define (rktio_from_bytes_list bstrs)
      (let ([p (foreign-alloc (fx* (length bstrs) (foreign-sizeof 'uptr)))])
        (let loop ([bstrs bstrs] [i 0])
          (cond
           [(null? bstrs) p]
           [else
            (let* ([bstr (car bstrs)]
                   [len (bytes-length bstr)]
                   [s (foreign-alloc (fx+ len 1))])
              (let loop ([j 0])
                (cond
                 [(= j len)
                  (foreign-set! 'unsigned-8 s j 0)]
                 [else
                  (foreign-set! 'unsigned-8 s j (bytes-ref bstr j))
                  (loop (fx+ 1 j))]))
              (foreign-set! 'uptr p (fx* i (foreign-sizeof 'uptr)) s)
              (loop (cdr bstrs) (fx+ 1 i)))]))
        (address->ptr p)))

    (define (rktio_free_bytes_list lls len)
      (rktio_to_bytes_list lls len)
      (void))

    (define (rktio_make_sha1_ctx)
      (make-bytevector (ftype-sizeof rktio_sha1_ctx_t)))
    (define (rktio_make_sha2_ctx)
      (make-bytevector (ftype-sizeof rktio_sha2_ctx_t)))

    (define (null-to-false v) (if (eqv? v NULL) #f v))

    (define (rktio_process_result_stdin_fd r)
      (null-to-false (address->ptr (ftype-ref rktio_process_result_t (stdin_fd) (make-ftype-pointer rktio_process_result_t (ptr->address r))))))
    (define (rktio_process_result_stdout_fd r)
      (null-to-false (address->ptr (ftype-ref rktio_process_result_t (stdout_fd) (make-ftype-pointer rktio_process_result_t (ptr->address r))))))
    (define (rktio_process_result_stderr_fd r)
      (null-to-false (address->ptr (ftype-ref rktio_process_result_t (stderr_fd) (make-ftype-pointer rktio_process_result_t (ptr->address r))))))
    (define (rktio_process_result_process r)
      (address->ptr (ftype-ref rktio_process_result_t (process) (make-ftype-pointer rktio_process_result_t (ptr->address r)))))

    (define (rktio_status_running r)
      (ftype-ref rktio_status_t (running) (make-ftype-pointer rktio_status_t (ptr->address r))))
    (define (rktio_status_result r)
      (ftype-ref rktio_status_t (result) (make-ftype-pointer rktio_status_t (ptr->address r))))

    (define (rktio_pipe_results r)
      (values
       (address->ptr (foreign-ref 'uptr (ptr->address r) 0))
       (address->ptr (foreign-ref 'uptr (ptr->address r) (foreign-sizeof 'uptr)))))

    (define (rktio_do_install_os_signal_handler rktio)
      (rktio_install_os_signal_handler rktio))

    (define (rktio_get_ctl_c_handler)
      (get-ctl-c-handler))

    (define |#%rktio-instance|
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
        (define-syntax begin
          (syntax-rules ()
            [(begin form ...)
             (extract-functions ['rktio_NULL
                                 NULL
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
                                form ...)]))
        (include-rel "../rktio/rktio.rktl"))))

  (define (immobile-cell->address p)
    (address->ptr (rumble:immobile-cell->address p)))

  (define (address->immobile-cell p)
    (rumble:address->immobile-cell (ptr->address p)))

  ;; ----------------------------------------

  (define format
    (case-lambda
     [(fmt arg)
      (unless (equal? fmt "~s")
        (raise-arguments-error 'format "should only be used as a fallback"
                               "format string" fmt
                               "argument" arg))
      (cond
       [(and (record? arg)
             (not (extflonum? arg))
             (or (not (impersonator? arg))
                 (record? (unsafe-struct*-ref arg 0))))
        (let ([arg (if (impersonator? arg)
                       (unsafe-struct*-ref arg 0)
                       arg)])
          (chez:format "#<~a>" (record-type-name (record-rtd arg))))]
       [else
        (chez:format "~s" arg)])]
     [(fmt . args)
      (raise-arguments-error 'format "should only be used as a fallback"
                             "format string" fmt
                             "arguments" args)]))

  ;; ----------------------------------------

  ;; For glib logging, we need a function pointer that works across
  ;; places and logs to the main place's root logger. Although it's
  ;; kind of a hack, it's much simpler to implement that here and
  ;; export the function pointer as a primitive.
  
  (export glib-log-message
          ;; Make sure the callable is retained:
          glib-log-message-callable)

  (define G_LOG_LEVEL_ERROR    2)
  (define G_LOG_LEVEL_CRITICAL 3)
  (define G_LOG_LEVEL_WARNING  4)
  (define G_LOG_LEVEL_MESSAGE  5)
  (define G_LOG_LEVEL_INFO     6)
  (define G_LOG_LEVEL_DEBUG    7)
  
  (define-values (glib-log-message glib-log-message-callable)
    (let ([glib-log-message
           (lambda (domain glib-level message)
             (let ([level (cond
                           [(fxbit-set? glib-level G_LOG_LEVEL_ERROR) 'fatal]
                           [(fxbit-set? glib-level G_LOG_LEVEL_CRITICAL) 'error]
                           [(fxbit-set? glib-level G_LOG_LEVEL_WARNING) 'warning]
                           [(fxbit-set? glib-level G_LOG_LEVEL_MESSAGE) 'warning]
                           [(fxbit-set? glib-level G_LOG_LEVEL_INFO) 'info]
                           [else 'debug])])
               (let ([go (lambda ()
                           (unsafe-start-atomic)
                           (disable-interrupts)
                           (let ([message (if domain
                                              (string-append domain ": " message)
                                              message)])
                             (log-message* (unsafe-root-logger) level #f message #f #f #f))
                           (enable-interrupts)
                           (unsafe-end-atomic))])
                 (cond
                  [(eqv? 0 (get-thread-id)) (go)]
                  [else
                   (ensure-virtual-registers)
                   (post-as-asynchronous-callback go)]))))])
      (let ([callable (foreign-callable __collect_safe glib-log-message (string int string) void)])
        (values
         (foreign-callable-entry-point callable)
         callable))))
  
  ;; ----------------------------------------

  (export system-library-subpath)
  (define system-library-subpath
    (case-lambda
     [() (system-library-subpath (system-type 'gc))]
     [(mode)
      (1/string->path
       (string-append
        system-library-subpath-string
        (cond
         [(eq? mode '3m) (if (eq? 'windows (system-path-convention-type))
                             "\\3m"
                             "/3m")]
         [(eq? mode 'cs) (if (eq? 'windows (system-path-convention-type))
                             "\\cs"
                             "/cs")]
         [(or (eq? mode 'cgc) (not mode)) ""]
         [else (raise-argument-error 'system-library-subpath
                                     "(or/c '3m 'cgc 'cs #f)"
                                     mode)])))]))

  (define (primitive-table key)
    (case key
      [(|#%pthread|) (hasheq)]
      [(|#%thread|) |#%thread-instance|]
      [(|#%rktio|) |#%rktio-instance|]
      [else #f]))

  (include "include.ss")
  (include-generated "io.scm")

  (include "io/terminal.ss")

  ;; Initialize:
  (set-log-system-message! (lambda (level str)
                             (1/log-message (|#%app| 1/current-logger) level str #f)))
  (set-error-display-eprintf! (lambda (fmt . args)
                                (apply 1/fprintf (|#%app| 1/current-error-port) fmt args))
                              1/srcloc->string
                              1/error-print-source-location)
  (set-ffi-get-lib-and-obj! ffi-get-lib ffi-get-obj ffi-unload-lib ptr->address)
  (set-make-async-callback-poll-wakeup! 1/unsafe-make-signal-received)
  (set-get-machine-info! get-machine-info)
  (set-processor-count! (1/processor-count))
  (install-future-logging-procs! logging-future-events? log-future-event)
  (install-place-logging-procs! logging-place-events? log-place-event))
